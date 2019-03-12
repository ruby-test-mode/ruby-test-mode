(ert-deftest ruby-test-unit-filename ()
  (should (equal "project/test/controllers/path/controller_test.rb"
                 (ruby-test-unit-filename "project/app/controllers/path/controller.rb")))
  (should (equal "project/test/models/path/model_test.rb"
                 (ruby-test-unit-filename "project/app/models/path/model.rb")))
  (should (equal "project/test/file_test.rb"
                 (ruby-test-unit-filename "project/lib/file.rb")))
  (should (equal "project/path/file_test.rb"
                 (ruby-test-unit-filename "project/path/file.rb"))))

(ert-deftest ruby-test-find-target-filename ()
  (let ((mapping '(("\\(.*\\)\\(.rb\\)" "\\1_test.rb" "other/\\1_test.rb"
                    "third/\\1_test.rb"))))
    (should (equal "exists_test.rb"
                   (ruby-test-find-target-filename "exists.rb" mapping)))
    (flet ((file-exists-p (filename)
                          (if (string-match "other" filename)
                              t
                            nil)))
      (should (equal "other/exists_test.rb"
                     (ruby-test-find-target-filename "exists.rb" mapping))))))

(ert-deftest ruby-test-testcase-name ()
  (should (equal nil (ruby-test-testcase-name "setup" "def")))
  (should (equal "test_with_question_mark_\\\\?"
                 (ruby-test-testcase-name "\"test with question mark ?\"" "def")))
  (should (equal "test_with_quotes_.*somewhere"
                 (ruby-test-testcase-name "\"test with quotes ' somewhere\"" "def")))
  (should (equal "test_with_parenthesis_.*somewhere.*"
                 (ruby-test-testcase-name "\"test with parenthesis (somewhere)\"" "def")))
  (should (equal "test with spaces from minitest"
                 (ruby-test-testcase-name "test with spaces from minitest" "it")))
  (should (equal "test_method_with_def"
                 (ruby-test-testcase-name "test_method_with_def" "def"))))

(ert-deftest ruby-test-specification-filename ()
  (should (equal "project/spec/models/file_spec.rb"
                 (ruby-test-specification-filename "project/app/models/file.rb")))
  (should (equal "project/spec/controllers/file_spec.rb"
                 (ruby-test-specification-filename "project/app/controllers/file.rb")))
  (should (equal "project/spec/helpers/file_spec.rb"
                 (ruby-test-specification-filename "project/app/helpers/file.rb")))
  (should (equal "project/spec/views/posts/new.html.erb_spec.rb"
                 (ruby-test-specification-filename "project/app/views/posts/new.html.erb")))
  (should (equal "project/spec/services/some_service/file_spec.rb"
                 (ruby-test-specification-filename "project/app/services/some_service/file.rb")))
  (should (equal "project/spec/something/file_spec.rb"
                 (ruby-test-specification-filename "project/lib/something/file.rb")))
  (should (equal "project/spec/some_lib/file_spec.rb"
                 (ruby-test-specification-filename "project/lib/some_lib/file.rb")))
  (should (equal "project/something/file_spec.rb"
                 (ruby-test-specification-filename "project/something/file.rb")))
  (should (equal "project/spec/javascripts/file_spec.coffee"
                 (ruby-test-specification-filename "project/app/assets/javascripts/file.coffee"))))

(ert-deftest ruby-test-testcase-name-test ()
  (find-file "test/unit_test.rb")
  (should (equal "test_one"
                 (ruby-test-find-testcase-at "unit_test.rb" 4)))
  (should (equal "test_one"
                 (ruby-test-find-testcase-at "unit_test.rb" 5)))
  (should (equal "test_one"
                 (ruby-test-find-testcase-at "unit_test.rb" 6))))


(ert-deftest ruby-test-testcase-name-saves-position ()
  (find-file "test/unit_test.rb")
  (with-current-buffer "unit_test.rb"
    (let ((target-line 5))
      (goto-line target-line)
      (should (equal "test_one"
                     (ruby-test-find-testcase-at "unit_test.rb" target-line)))
      (should (equal target-line (line-number-at-pos))))))
