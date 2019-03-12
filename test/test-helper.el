(when (require 'undercover nil t)
  (undercover "ruby-test-mode.el"))

(require 'testcover)

(eval-when-compile (require 'cl))
(require 'ert)

(require 'ruby-test-mode)
