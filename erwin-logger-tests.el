;;; erwin-logger-tests.el --- tests for erwin's history stuff

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm

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

;; 

;;; Code:

(require 'erwin-logger)
(require 'ert)

(ert-deftest erwin-logger/make-proc ()
  "Test that the proc does sensible JSON encoding.

We know what the mill does so we can fake the file it uses by
sending it dummy data."
  (let ((mill (erwin-logger/make-proc))
        (file (format "%s/testchannel/2014-14-01" erwin-logger/log-dir))
        (text "this is just some text \"with some quotes\""))
    (when (file-exists-p file) (delete-file file))
    (unwind-protect
         (progn
           (process-send-string
            mill (concat
                  "2014-14-01 testchannel "
                  (json-encode
                   (list
                    :test t
                    :text text)) "\n"))
           ;; We do have to sit and wait for the response
           (sit-for 1))
      (delete-process mill))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (let ((alist (json-read)))
        (assert (equal (kva 'text alist) text))))))


(provide 'erwin-logger-tests)
;;; erwin-logger-tests.el ends here
