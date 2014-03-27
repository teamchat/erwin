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

(defun erwin-logger/make-proc ()
  "Make a process for logging erwin data.

The process is a pipe mill taking an input line with:

  date channel json-data ...

where the date is a YYYY-MM-DDTHH:MM:SS date, the channel is the
IRC channel and the json-data is a JSON record."
  (let ((temp-name (make-temp-name "erwin-logger")))
    ;; FIXME - we need to abstract the location of the logs
    ;; don't forget to update the test
    (start-process-shell-command
     (concat " *" temp-name "*")
     (concat " *" temp-name "*")
     (format
      "while read -r date channel json
do
   mkdir -p /tmp/erwinlogs/${channel}
   echo $json >> /tmp/erwinlogs/${channel}/${date}
done"))))


(defun erwin-logger/get-history (channel)
  "Get the history data from the most recent file for CHANNEL.

CHANNEL must be missing the leading # or whatever special
character."
  (with-temp-buffer
    (insert-file-contents-literally
     (car (reverse
           (-sort
            'string-lessp
            (directory-files
             (format "/tmp/erwinlogs/%s/" channel) t "^[^.]")))))
    (goto-char (point-min))
    (let (hist)
      (while (condition-case err (push (json-read) hist) (error nil)))
      (reverse hist))))

(defun erwin-logger/history-send (process sender channel)
  "Send the history to the SENDER over rcirc PROCESS."
  (let ((history (erwin-logger/get-history channel)))
    (--each history
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

(defun erwin-logger/history-receive-print-hook (process sender response target text)
  "Print hook that sends channel data to a log structure in JSON.

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
      (cond
        ;; A request for history
        ((or 
          (string-match (format "^%s:[ ]*history\\([ ]+\\(.*\\)\\)*" my-nick) text)
          (and (equal target my-nick)
               (string-match "^history\\([ ]+\\(.*\\)\\)*" text)))
         (erwin-logger/history-send process sender (substring target 1)))
        (t
         (condition-case err
             (when (s-starts-with? "#" target)
               (let ((day (substring time-str 0 10)))
                 (process-send-string
                  (erwin-logger/get-logger-proc process)
                  (concat day " " (substring target 1) " " json "\n"))))            
           (error nil)))))))

;; Setup the receive hook for the upstream IRC connection -- we don't
;; need to if this really because `erwin-logger-do-history' is a
;; control for whether this does any work.
(add-hook
 'rcirc-print-functions
 'erwin-logger/receive-print-hook)


(provide 'erwin-logger)

;;; erwin-logger.el ends here
