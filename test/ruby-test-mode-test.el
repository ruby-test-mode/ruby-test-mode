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
    (cl-letf (((symbol-function 'file-exists-p) (lambda (filename)
                                                  (if (string-match "other" filename)
                                                      t
                                                    nil))))
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

(ert-deftest ruby-test-command ()
  (with-test-file "test/minitest_helper.rb"
    (with-test-file ".gemspec"
      (should (string-match "bundle exec ruby -I'lib:test:spec' -rrubygems ./test/unit_test.rb"
                (ruby-test-command "./test/unit_test.rb")))))
  (with-test-file "spec/minitest_helper.rb"
    (with-test-file ".gemspec"
      (should (string-match "bundle exec ruby -I'lib:test:spec' -rrubygems ./spec/unit_spec.rb"
                (ruby-test-command "./spec/unit_spec.rb")))))
  (with-test-file ".gemspec"
    (should (string-match "bundle exec ruby -I'lib:test' -rrubygems ./test/unit_test.rb"
              (ruby-test-command "./test/unit_test.rb"))))
  (with-test-file "config/environment.rb"
    (should (string-match "PAGER=cat bundle exec rails test -v ./test/unit_test.rb"
              (ruby-test-command "./test/unit_test.rb"))))
  (should (string-match "bundle exec rspec -b ./spec/unit_spec.rb"
            (ruby-test-spec-command "./spec/unit_spec.rb"))))

(ert-deftest ruby-test-minitest-command ()
  (with-test-file ".gemspec"
    (should (string-match "bundle exec ruby -I'lib:test:spec' -rrubygems ./test/unit_test.rb"
              (ruby-test-minitest-command "./test/unit_test.rb")))))

(ert-deftest ruby-test-spec-command ()
  (with-test-file ".zeus.sock"
    (should (equal "zeus rspec -b project/spec/hello_spec.rb"
              (ruby-test-spec-command "project/spec/hello_spec.rb"))))
  (with-test-file "bin/rspec"
    (should (equal "bin/rspec -b project/spec/hello_spec.rb"
              (ruby-test-spec-command "project/spec/hello_spec.rb"))))
  (should (equal "bundle exec rspec -b project/spec/hello_spec.rb"
            (ruby-test-spec-command "project/spec/hello_spec.rb")))
  (should (equal "bundle exec rspec -b project/spec/hello_spec.rb:7"
            (ruby-test-spec-command "project/spec/hello_spec.rb" 7))))

(ert-deftest ruby-test-test-command ()
  (with-test-file ".gemspec"
    (should (string-match "bundle exec ruby -I'lib:test' -rrubygems ./test/unit_test.rb"
              (ruby-test-test-command "./test/unit_test.rb")))))

(ert-deftest ruby-test-rails-root-p ()
  (with-test-file "config/environment.rb"
    (should (ruby-test-rails-root-p "."))))

(ert-deftest ruby-test-rails-root ()
  (with-test-file "config/database.yml"
    (should (equal "./" (ruby-test-rails-root "./test/unit_test.rb")))))

(ert-deftest ruby-test-gem-root-p ()
  (with-test-file ".gemspec"
    (should (ruby-test-gem-root-p "."))))

(ert-deftest ruby-test-gem-root ()
  (with-test-file ".gemspec"
    (should (equal "./" (ruby-test-gem-root "./test/unit_test.rb")))))

(ert-deftest ruby-test-ruby-root-p ()
  (should (ruby-test-ruby-root-p "./"))
  (should-not (ruby-test-ruby-root-p "test")))

(ert-deftest ruby-test-ruby-root ()
  (should (equal "./" (ruby-test-ruby-root "./test/unit_test.rb"))))

(ert-deftest ruby-test-minitest-p ()
  (with-test-file "spec/minitest_helper.rb"
    (should (ruby-test-minitest-p "spec/hello_spec.rb")))
  (with-test-file "test/minitest_helper.rb"
    (should (ruby-test-minitest-p "test/hello_test.rb"))))

(ert-deftest ruby-test-rails-p ()
  (with-test-file "config/environment.rb"
    (should (ruby-test-rails-p "./test/unit_test.rb")))
  (with-test-file "config/database.yml"
    (should (ruby-test-rails-p "./test/unit_test.rb")))
  (with-test-file "config/routes.rb"
    (should (ruby-test-rails-p "./test/unit_test.rb"))))

(ert-deftest ruby-test-spec-p ()
  (should (ruby-test-spec-p "spec/hello_spec.rb")))

(ert-deftest ruby-test-test-p ()
  (should (ruby-test-p "test/hello_test.rb")))
