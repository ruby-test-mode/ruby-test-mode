(eval-when-compile (require 'cl))
(require 'ert)
(require 'ruby-test-mode)

(ert-deftest ruby-test-unit-filename ()
  (should (equal "test/functional/path/controller_test.rb" (ruby-test-unit-filename "app/controllers/path/controller.rb")))

  (should (equal "test/unit/path/model_test.rb" (ruby-test-unit-filename "app/models/path/model.rb")))

  (should (equal "test/file_test.rb" (ruby-test-unit-filename "lib/file.rb")))

  (should (equal "path/file_test.rb" (ruby-test-unit-filename "path/file.rb")))
  )

(ert-deftest ruby-test-find-target-filename ()
  (let ((mapping '(("\\(.*\\)\\(.rb\\)" "\\1_test.rb" "other/\\1_test.rb"))))
    (should (equal "exists_test.rb" (ruby-test-find-target-filename "exists.rb" mapping)))
    (flet ((file-exists-p (filename)
                          (if (string-match "other" filename)
                              t
                            nil)))
      (should (equal "other/exists_test.rb" (ruby-test-find-target-filename "exists.rb" mapping))))))

(ert-deftest ruby-test-tescase-name ()
  (should (equal nil (ruby-test-tescase-name "setup")))
  (should (equal "test_with_question_mark_\\\\?" (ruby-test-tescase-name "\"test with question mark ?\"")))
  (should (equal "test_with_quotes_.*somewhere" (ruby-test-tescase-name "\"test with quotes ' somewhere\"")))
  (should (equal "test_with_parenthesis_.*somewhere.*" (ruby-test-tescase-name "\"test with parenthesis (somewhere)\""))))