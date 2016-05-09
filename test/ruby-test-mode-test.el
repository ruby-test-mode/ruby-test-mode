(eval-when-compile (require 'cl))
(require 'ert)
(require 'ruby-test-mode)

(ert-deftest ruby-test-unit-filename ()
  (should (equal "test/controllers/path/controller_test.rb" (ruby-test-unit-filename "app/controllers/path/controller.rb")))

  (should (equal "test/models/path/model_test.rb" (ruby-test-unit-filename "app/models/path/model.rb")))

  (should (equal "test/file_test.rb" (ruby-test-unit-filename "lib/file.rb")))

  (should (equal "path/file_test.rb" (ruby-test-unit-filename "path/file.rb")))
  )

(ert-deftest ruby-test-find-target-filename ()
  (let ((mapping '(("\\(.*\\)\\(.rb\\)" "\\1_test.rb" "other/\\1_test.rb" "third/\\1_test.rb"))))
    (should (equal "exists_test.rb" (ruby-test-find-target-filename "exists.rb" mapping)))
    (flet ((file-exists-p (filename)
                          (if (string-match "other" filename)
                              t
                            nil)))
      (should (equal "other/exists_test.rb" (ruby-test-find-target-filename "exists.rb" mapping))))))

(ert-deftest ruby-test-testcase-name ()
  (should (equal nil (ruby-test-testcase-name "setup" "def")))
  (should (equal "test_with_question_mark_\\\\?" (ruby-test-testcase-name "\"test with question mark ?\"" "def")))
  (should (equal "test_with_quotes_.*somewhere" (ruby-test-testcase-name "\"test with quotes ' somewhere\"" "def")))
  (should (equal "test_with_parenthesis_.*somewhere.*" (ruby-test-testcase-name "\"test with parenthesis (somewhere)\"" "def")))
  (should (equal "test with spaces from minitest" (ruby-test-testcase-name "test with spaces from minitest" "it")))
  )

(ert-deftest ruby-test-specification-filename ()
  (should (equal "spec/models/file_spec.rb" (ruby-test-specification-filename "app/models/file.rb")))
  (should (equal "spec/controllers/file_spec.rb" (ruby-test-specification-filename "app/controllers/file.rb")))
  (should (equal "spec/helpers/file_spec.rb" (ruby-test-specification-filename "app/helpers/file.rb")))
  (should (equal "spec/views/posts/new.html.erb_spec.rb" (ruby-test-specification-filename "app/views/posts/new.html.erb")))
  (should (equal "spec/services/some_service/file_spec.rb" (ruby-test-specification-filename "app/services/some_service/file.rb")))
  (should (equal "spec/lib/something/file_spec.rb" (ruby-test-specification-filename "lib/something/file.rb")))
  (should (equal "spec/lib/some_lib/file_spec.rb" (ruby-test-specification-filename "lib/some_lib/file.rb")))
  (should (equal "something/file_spec.rb" (ruby-test-specification-filename "something/file.rb"))))
