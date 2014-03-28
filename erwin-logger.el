;;; erwin-logger.el --- logging rcirc robot

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a channel history logger for Erwin

;;; Code:

(require 'rcirc)
(require 'dash)
(require 's)
(require 'json)
(require 'kv)

(unless (fboundp 'kva)
  (fset 'kva (function (lambda (key a) (cdr (assoc key alist))))))

(defmacro comment (&rest args))

(defconst erwin-logger/log-dir "/tmp/erwinlogs"
  "The location of the history files.")

(defun erwin-logger/make-proc ()
  "Make a process for logging erwin data.

The process is a pipe mill taking an input line with:

  date channel json-data ...

where the date is a YYYY-MM-DDTHH:MM:SS date, the channel is the
IRC channel and the json-data is a JSON record."
  (let ((temp-name (make-temp-name "erwin-logger")))
    (start-process-shell-command
     (concat " *" temp-name "*")
     (concat " *" temp-name "*")
     (format
      "export LOGDIR=%s
while read -r date channel json
do
   mkdir -p ${LOGDIR}/${channel}
   echo $json >> ${LOGDIR}/${channel}/${date}
done" erwin-logger/log-dir))))


(defun erwin-logger/get-history (channel &optional date)
  "Get the history data from the most recent file for CHANNEL.

CHANNEL must be missing the leading # or whatever special
character."
  (with-temp-buffer
    (insert-file-contents-literally
     (car (reverse
           (-sort
            'string-lessp
            (directory-files
             (format "%s/%s/" erwin-logger/log-dir channel) t "^[0-9]")))))
    (goto-char (point-min))
    (let ((history
           (reverse
            (let (hist)
              (while (condition-case err (push (json-read) hist) (error nil)))
              hist))))
      (if (not date)
          history
          ;; Else filter it
          (-filter (lambda (r) (string-lessp date (kva 'time r))) history)))))

(defun erwin-logger/history-send (process sender channel &optional date)
  "Send the history to the SENDER over rcirc PROCESS."
  (let ((history (erwin-logger/get-history channel date)))
    (--each
        (-filter
         (lambda (e) (equal "PRIVMSG" (kva 'response e)))
         history)
      (rcirc-send-message
       process sender ; the target is the same
       (format "%s %s: %s"
               (kva 'time it)
               (kva 'sender it)
               (kva 'text it))))))

(defun erwin-logger/get-logger-proc (rcirc-process)
  "Get the logging mill shell process for the RCIRC-PROCESS.

The mill process is stored in a buffer local variable on the
RCIRC-PROCESS process buffer."
  (let ((rcirc-buffer (process-buffer rcirc-process)))
    (with-current-buffer procbuf
      (or
       (when (local-variable-p 'erwin-logger-proc)
         (symbol-value 'erwin-logger-proc))
       (set (make-local-variable 'erwin-logger-proc)
            (erwin-logger/make-proc))))))

(defconst erwin-logger-do-history t
  "Whether to collect and respond with history")

(defun erwin-logger/get-quit-hash (rcirc-process)
  "Get a hash used to store when IRC users quit."
  (let ((rcirc-buffer (process-buffer rcirc-process)))
    (with-current-buffer rcirc-buffer
      (or
       (when (local-variable-p 'erwin-quit-hash)
         (symbol-value 'erwin-quit-hash))
       (set (make-local-variable 'erwin-quit-hash)
            (make-hash-table :test 'equal))))))

(defun erwin-logger/history-receive-print-hook (process sender response target text)
  "Store and respond history, ask for \"history\".

History is stored in a structure of `channel-name/day' where
`day' is the file with data in. 

The word `history' when sent to the nick running this hook will
deliver the history data in a private message."
  (when erwin-logger-do-history
    (let* ((time-str (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time)))
           (procbuf (process-buffer process))
           (server (with-current-buffer procbuf  rcirc-server))
           (my-nick (with-current-buffer procbuf  rcirc-nick))
           (plist (list :server server :time time-str :response response
                        :sender sender :target target :text text))
           (json (json-encode plist)))
      ;;(message json)
      (save-match-data
        (cond
          ;; A request for history
          ((string-match (format "^%s:[ ]*history\\([ ]+\\(.*\\)\\)*" my-nick) text)
           (erwin-logger/history-send
            process sender
            (substring target 1) ; the channel
            (or (match-string 2 text)
                (gethash sender (erwin-logger/get-quit-hash process)))))
          ;; A private request for history
          ((and (equal target sender)
                (string-match
                 "^history\\([ ]+\\([^ ]+\\)\\([ ]+\\(.*\\)\\)*\\)*"
                 text))
           (if (match-string 1 text)
               (erwin-logger/history-send
                process sender
                (match-string 2 text) ; the channel
                (or (match-string 4 text) ; possible argument
                    (gethash sender (erwin-logger/get-quit-hash process))))
               ;; Else...
               (rcirc-send-message process target "say \"history channel\"")))
          (t
           (condition-case err
               (progn
                 (when (equal response "QUIT")
                   (puthash
                    sender (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))
                    (erwin-logger/get-quit-hash process)))
                 (when (s-starts-with? "#" target)
                   (let ((day (substring time-str 0 10)))
                     (process-send-string
                      (erwin-logger/get-logger-proc process)
                      (concat day " " (substring target 1) " " json "\n")))))            
             (error nil))))))))


;; Setup the receive hook for the upstream IRC connection -- we don't
;; need to if this really because `erwin-logger-do-history' is a
;; control for whether this does any work.
(add-hook
 'rcirc-print-functions
 'erwin-logger/history-receive-print-hook)

(defun erwin-logger/ping-receive-print-hook (process sender response target text)
  "Ping/pong response, ask \"ping\".

The response goes back to the channel from where it was given or
inside the private chat where it was issued."
  (let* ((procbuf (process-buffer process))
         (my-nick (with-current-buffer procbuf  rcirc-nick)))
    ;;(message "%s %s %s" sender target text)
    (when (or (string-match (format "^%s: ping" my-nick) text)
              (and (equal target sender)
                   (string-match "^ping$" text)))
      (rcirc-send-message process target (format "%s: pong" sender)))))

(add-hook
 'rcirc-print-functions
 'erwin-logger/ping-receive-print-hook)

(defun erwin-logger/help-robot-funcs ()
  "List the robot functions."
  (-keep
   (lambda (fn)
     (when (documentation fn)
       (save-match-data
         (let ((name (symbol-name fn)))
           (string-match
            (format "erwin\\(-[a-z]+\\)*/\\([a-z0-9_-]+\\)-receive-print-hook")
            name)
           (format
            "'%s' -- %s"
            (match-string 2 name)
            (elt (split-string (documentation fn) "\n") 0))))))
   rcirc-print-functions))

(defun erwin-logger/help-receive-print-hook (process sender response target text)
  "Explain what this robot does, ask for \"help\"."
  (let* ((procbuf (process-buffer process))
         (my-nick (with-current-buffer procbuf  rcirc-nick)))
    (when (or (string-match (format "^%s: help$" my-nick) text)
              (and (equal target sender)
                   (string-match "^help$" text)))
      (-each
       (erwin-logger/help-robot-funcs)
       (lambda (help-line)
         (rcirc-send-message process target (concat sender ": " help-line)))))))

(add-hook
 'rcirc-print-functions
 'erwin-logger/help-receive-print-hook)

(defun erwin-logger/origin-receive-print-hook (process sender response target text)
  "Explain where the code for this robot is, ask for \"origin\"."
  (let* ((procbuf (process-buffer process))
         (my-nick (with-current-buffer procbuf  rcirc-nick)))
    (when (or (string-match (format "^%s: origin$" my-nick) text)
              (and (equal target sender)
                   (string-match "^origin$" text)))
      (rcirc-send-message
       process target
       (concat sender
               ": you can see inside me here - http://github.com/teamchat/erwin")))))

(add-hook
 'rcirc-print-functions
 'erwin-logger/origin-receive-print-hook)

(provide 'erwin-logger)

;;; erwin-logger.el ends here
