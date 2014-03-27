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

(defmacro comment (&rest args))

(defun erwin-logger/receive-hook (process cmd sender args text)
  "Hook attached to rcirc to intercept the upstream irc server."
  (message
   "erwin-logger %s"
   (json-encode (list :process (process-id process)
                      :command cmd
                      :sender sender
                      :args args
                      :text text)))
  ;; FIXME - Should we check to see if this is a shoes-off session?
  (comment
   (condition-case nil
       (let ((cmdstr
              (or (aget shoes-off/cmd-names (string-to-number cmd)) cmd)))
         (when (equal cmdstr "RPL_LIST")
           (apply 'shoes-off/list-entry process args))
         (when (equal cmdstr "WELCOME")
           (shoes-off/welcome process text))
         (when (member
                cmdstr
                shoes-off/cache-response-welcome-commands)
           (shoes-off/puthash process
                              :shoes-off-welcome-cache
                              cmdstr text))
         (when (equal cmdstr "PART")
           (message "shoes-off/receive-hook [%s] %s (%s) |%s|"
                    process cmd args text)
           (shoes-off/part process args text))
         (when (equal cmdstr "JOIN")
           (shoes-off/join process args text))
         ;; if we have a current bouncer con then send stuff there
         (awhen (process-get process :shoes-off-connection)
           (catch :shoes-off-escape-privmsg
             (when (and
                    (equal cmdstr "PRIVMSG")
                    (functionp shoes-off-receive-privmsg-plugin))
               (funcall shoes-off-receive-privmsg-plugin
                        process sender args))
             ;; If we didn't throw carry on call send-string
             (rcirc-send-string it text))))
     (error "whoops! something went wrong!!!"))))

;; Setup the receive hook for the upstream IRC connection
(when (symbol-value 'erwin-logger-add-receive-hook)
  (add-hook
   'rcirc-receive-message-hooks
   'erwin-logger/receive-hook))

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
      "while read date channel json
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

(defun erwin-logger/receive-print-hook (process sender response target text)
  "Print hook that sends channel data to a log structure in JSON."
  (let* ((time-str (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time)))
         (procbuf (process-buffer process))
         (server (with-current-buffer procbuf  rcirc-server))
         (my-nick (with-current-buffer procbuf  rcirc-nick))
         (json
          (json-encode (list :server server
                             :time time-str
                             :response response
                             :sender sender
                             :target target
                             :text text))))
    ;;(message json)
    (cond
      ((or 
        (string-match (format "^%s:[ ]*history\\([ ]+\\(.*\\)\\)*" my-nick) text)
        (and (equal target my-nick)
             (string-match "^history\\([ ]+\\(.*\\)\\)*" text)))
       (let ((history (erwin-logger/get-history (substring target 1))))
         (--each history
           (rcirc-send-message
            process sender ; the target is the same
            (format "%s %s: %s"
                    (kva 'time it)
                    (kva 'sender it)
                    (kva 'text it))))))
      (t
       (condition-case err
           (when (s-starts-with? "#" target) ; Only do it for channels
             (let* ((day (substring time-str 0 10))
                    (logger-proc (with-current-buffer procbuf
                                   (or
                                    (when (local-variable-p 'erwin-logger-proc)
                                      (symbol-value 'erwin-logger-proc))
                                    (set (make-local-variable 'erwin-logger-proc)
                                         (erwin-logger/make-proc))))))
               (process-send-string
                logger-proc
                (concat day " " (substring target 1) " " json "\n"))))
         (error nil))))))

;; Setup the receive hook for the upstream IRC connection
(add-hook  ; fixme - needs some config
 'rcirc-print-functions
 'erwin-logger/receive-print-hook)


(provide 'erwin-logger)

;;; erwin-logger.el ends here
