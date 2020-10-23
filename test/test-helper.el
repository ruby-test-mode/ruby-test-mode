(when (require 'undercover nil t)
  (undercover "ruby-test-mode.el"))

(require 'testcover)
(require 'f)

(eval-when-compile (require 'cl-lib))
(require 'ert)

(require 'ruby-test-mode)

(defmacro with-test-file (name &rest forms)
  "Create temporary NAME file and evaluate FORMS"
  `(unwind-protect
     (progn
       (f-mkdir (f-dirname ,name))
       (f-touch ,name)
       ,@forms)
     (f-delete ,name t)))
