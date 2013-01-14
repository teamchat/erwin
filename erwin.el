;;; erwin - Emacs Robots Within IRC Network -*- lexical-binding: t -*-

(require 'rcirc)
(require 'web)
(require 'cl)
(require 's)

(defvar erwin/db '()
  "Erwin's alist database of regexs to functions.")

(defun erwin/db-put (regex handler)
  (unless (aget erwin/db regex)
    (setq erwin/db (acons regex handler erwin/db))))

(defun erwin/db-list ()
  "List the database."
  erwin-db)

(defun erwin/detect (text)
  "Detect erwin in the TEXT."
  (when (string-match
         "\\(^erwin[:, ]\\|[ ,;]erwin[.?, ;]\\|[ ,;]erwin$\\)"
         text) text))

(defun erwin/send (process channel data)
  "Send DATA to CHANNEL on PROCESS."
  (with-current-buffer (rcirc-get-buffer process channel)
    (save-excursion
      (goto-char (point-max))
      (insert data)
      (rcirc-send-input))))

(defun erwin/data->response (data)
  "Convert DATA to a response."
  (let* ((response (aget data "erwin")))
    (condition-case err
        (s-format response 'aget data)
      (s-format-resolve
       (format
        "The response from the service: %S"
        data)))))

(defun erwin/web (process sender target url)
  "Send a generic web call to URL.

It's sent as a POST with sender and target as POST variables.

Erwin expects a JSON object response with a key \"erwin\" which
will specify the message string to go back to the user. The
message string may include refferences to the other data parts
with `s-format' syntax:

   \"the repository is: ${url}\"

with data:

  { \"url\": \"http://github.com/nicferrier/shoes-off\" }

will do what you'd expect.  If a key is not present in the data
but used in the format string then erwin will respond with an
error."
  ;; FIXME deal with failure, send sensible return value etc...
  (web-json-post
   (lambda (data httpcon header)
     (erwin/send process target (erwin/data->response data)))
   :url url
   :data (list (cons "sender" sender)
               (cons "target" target))))

(defun erwin-input (process sender target text)
  "`rcirc-print-hooks' handler to connect erwin."
  (when (or
         (equal target "erwin")
         ;; we could have additional text matches here like: ???
         (erwin/detect text))
    (loop for (regex . handler) in (erwin/db-list)
       if (string-match regex input)
       return
         (let* ((md (match-data))
                (strings ;; this is match-strings
                 (loop for a in md by 'cddr
                    for b in (cdr md) by 'cddr
                    collect (substring input a b))))
           (cond
             ((stringp handler)
              (erwin/web
               process sender target
               (s-format handler 'elt strings)))
             ((functionp handler)
              (funcall
               handler process sender target
               strings)))))))

;; Some simple built in robots
(erwin/db-put "^erwin[:, ] *insult \\(.*\\)"
              "http://localhost:8007/insult/?who=$1")

(erwin/db-put "^erwin[:, ] *\\(.*\\)\\+\\+"
              "http://localhost:8007/cred/?who=$1")

(defun erwin-hammertime (process sender target matches)
  "Impersonate MC Hammer."
  (let ((quotes (list
                 "READY THE ENORMOUS TROUSERS!"
                 "YOU CAN'T TOUCH THIS!")))
    (erwin/send process target (elt quotes (random (length quotes))))))

(erwin/db-put "^erwin[:, ] *hammertime" 'erwin-hammertime)

(provide 'erwin)

;;; erwin.el ends here
