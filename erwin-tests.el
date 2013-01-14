

(ert-deftest erwin/detect ()
  (should (erwin/detect "erwin what do you think?"))
  (should (erwin/detect "what do you think erwin?"))
  (should (erwin/detect "what do you think erwin?"))
  (should (erwin/detect "what do you think erwin?"))
  (should (erwin/detect "what do you think erwin"))
  (should-not (erwin/detect "what do you think erwing")))

(ert-deftest erwin/data->response ()
  (should
   (equal "hello nic you look great!"
          (erwin/data->response
           '(("erwin" . "hello ${name} you look ${greet}")
             ("name" . "nic")
             ("greet" . "great!")))))
  (should
   (equal
    (concat
     "The response from the service: "
     "((\"erwin\" . \"hello ${name} you look ${greet}\"))")
    (erwin/data->response '(("erwin" . "hello ${name} you look ${greet}"))))))

;;; test-erwin.el ends here
