;;; ruby-test-mode.el --- Minor mode for Behaviour and Test Driven
;;; Development in Ruby.

;; Copyright (C) 2009 Roman Scherer, Caspar Florian Ebeling

;; Author: Roman Scherer <roman.scherer@gmx.de>
;;         Caspar Florian Ebeling <florian.ebeling@gmail.com>
;;
;; Maintainer: Roman Scherer <roman.scherer@burningswell.com>
;; Created: 09.02.08
;; Version: 1.7
;; Keywords: ruby unit test rspec
;; Package-Requires: ((ruby-mode "1.0") (pcre2el "1.8"))

;; This software can be redistributed. GPL v2 applies.

;; This mode provides commands for running ruby tests. The output is
;; shown in separate buffer '*Ruby-Test*' in ruby-test
;; mode. Backtraces from failures and errors are marked, and can be
;; clicked to bring up the relevent source file, where point is moved
;; to the named line.
;;
;; The tests can be both, either rspec behaviours, or unit
;; tests. (File names are assumed to end in _spec.rb or _test.rb to
;; tell the type.)  When the command for running a test is invoked, it
;; looks at several places for an actual test to run: first, it looks
;; if the current buffer is a test (or spec), secondly, if not, it
;; checks whether one of the visible buffers is, thirdly it looks if
;; there has been a test run before (during this session), in which
;; case that test is invoked again.
;;
;; Using the command `ruby-test-run-test-at-point', you can run test
;; cases separately from others in the same file.

;; Keybindings:
;;
;; C-c C-,   - Runs the current buffer's file as an unit test or an
;;             rspec example.
;;
;; C-c M-,   - Runs the unit test or rspec example at the current buffer's
;;             buffer's point.
;;
;; C-c C-s   - Toggle between implementation and test/example files.

(require 'ruby-mode)
(require 'pcre2el)

(defgroup ruby-test nil
  "Minor mode providing commands and helpers for Behavioural and
Test Driven Development in Ruby."
  :group 'ruby)

(defcustom ruby-test-rspec-options
  '("-b")
  "Pass extra command line options to RSpec when running specs."
  :initialize 'custom-initialize-default
  :type '(list)
  :group 'ruby-test)

(defvar ruby-test-default-library
  "test"
  "Define the default test library")

(defvar ruby-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") 'ruby-test-run)
    (define-key map (kbd "C-c M-,") 'ruby-test-run-at-point)
    (define-key map (kbd "C-c C-s") 'ruby-test-toggle-implementation-and-specification)
    map)
  "The keymap used in `ruby-test-mode' buffers.")

(defcustom ruby-test-file-name-extensions
  '("builder" "erb" "haml" "rb" "rjs" "rake" "slim")
  "*A list of filename extensions that trigger the loading of the
minor mode."
  :type '(list)
  :group 'ruby-test)

(defcustom ruby-test-implementation-filename-mapping
  `(
    (,(pcre-to-elisp "(.*)/spec/routing/routes_spec\\.rb$") "\\1/config/routes.rb")
    (,(pcre-to-elisp "(.*)/spec/routing/routes_spec\\.rb$") "\\1/config/routes.rb")
    (,(pcre-to-elisp "(.*)/test/routing/routes_test\\.rb$") "\\1/config/routes.rb")
    (,(pcre-to-elisp "(.*)/spec/(controllers|models|helpers|mailers|uploaders|api)/(.*)_spec\\.rb$")
     "\\1/app/\\2/\\3.rb")
    (,(pcre-to-elisp "(.*)/test/(controllers|models|helpers|mailers|uploaders|api)/(.*)_test\\.rb$")
     "\\1/app/\\2/\\3.rb")

    (,(pcre-to-elisp "(.*)/spec/(views/.*)_spec\\.rb$") "\\1/app/\\2")
    (,(pcre-to-elisp "(.*)/test/(views/.*)_test\\.rb$") "\\1/app/\\2")

    (,(pcre-to-elisp "(.*)/spec/(.*)_tasks_spec\\.rb$") "\\1/\\2.rake")
    (,(pcre-to-elisp "(.*)/test/(.*)_tasks_test\\.rb$") "\\1/\\2.rake")

    ;; Project/spec/lib/aaa/bbb_spec.rb => Project/lib/aaa/bbb.rb
    (,(pcre-to-elisp "(.*)/spec/lib/(.*)_spec\\.rb$") "\\1/lib/\\2.rb")
    (,(pcre-to-elisp "(.*)/test/lib/(.*)_test\\.rb$") "\\1/lib/\\2.rb")

    ;; Project/spec/aaa/bbb_spec.rb => Project/lib/aaa/bbb.rb
    (,(pcre-to-elisp "(.*)/spec/(.*)_spec\\.rb$") "\\1/lib/\\2.rb")

    (,(pcre-to-elisp "(.*)/test/unit/(.*)_test\\.rb$")
     "\\1/app/models/\\2.rb"
     "\\1/lib/\\2.rb")
    (,(pcre-to-elisp "(.*)/test/functional/(.*)_test\\.rb$") "\\1/app/controllers/\\2.rb")

    ;; Project/test/aaa/bbb_test.rb => Project/lib/aaa/bbb.rb
    (,(pcre-to-elisp "(.*)/test/(.*)_test\\.rb$") "\\1/lib/\\2.rb")

    ;; make ruby-test-mode support asserts spec.
    (,(pcre-to-elisp "(.*)/spec/javascripts/(.*)_spec\\.(js|coffee)$")
     "\\1/app/assets/javascripts/\\2.\\3")

    ;; in same folder,  gem/aaa_spec.rb => gem/aaa.rb
    (,(pcre-to-elisp "(.*)_(spec|test)\\.rb$") "\\1.rb")
    )
  "Regular expressions to map Ruby implementation to unit
filenames). The first element in each list is the match, the
second the replace expression."
  :type '(list)
  :group 'ruby-test)

(defcustom ruby-test-specification-filename-mapping
  `(
    (,(pcre-to-elisp "(.*)/config/routes\\.rb$")
     "\\1/spec/routing/routes_spec.rb"
     "\\1/test/routing/routes_test.rb")
    (,(pcre-to-elisp "(.*)/app/views/(.*)$")
     "\\1/spec/views/\\2_spec.rb"
     "\\1/test/views/\\2_test.rb")

    ;; everything in app, should exist same path in spec/test.
    (,(pcre-to-elisp "(.*)/app/(.*)\\.rb$")
     "\\1/spec/\\2_spec.rb"
     "\\1/test/\\2_test.rb")

    (,(pcre-to-elisp "(.*)/lib/(tasks/.*)\\.rake$")
     "\\1/spec/\\2_tasks_spec.rb"
     "\\1/test/\\2_tasks_test.rb")

    ;; Project/lib/aaa/bbb.rb, search order:
    ;; => Project/spec/lib/aaa/bbb_spec.rb, Project/spec/aaa/bbb_spec.rb
    (,(pcre-to-elisp "(.*)/lib/(.*)\\.rb$")
     "\\1/spec/\\2_spec.rb"
     "\\1/spec/lib/\\2_spec.rb"
     "\\1/test/\\2_test.rb"
     "\\1/test/lib/\\2_test.rb")

    ;; make ruby-test-mode support asserts spec.
    (,(pcre-to-elisp "(.*)/app/assets/javascripts/(.*)\\.(js|coffee)$")
     "\\1/spec/javascripts/\\2_spec.\\3")

    ;; in same folder,  gem/aaa.rb => gem/aaa_spec.rb
    (,(pcre-to-elisp "(.*)\\.rb$") "\\1_spec.rb" "\\1_test.rb")
    )
  "Regular expressions to map Ruby specification to
implementation filenames). The first element in each list is the
match, the second the replace expression."
  :type '(list)
  :group 'ruby-test)

;; TODO: It seem like we does not need this mapping anymore.
;; We could add more candicaate to `ruby-test-specification-filename-mapping' to
;; instead this.
(defcustom ruby-test-unit-filename-mapping
  `(
    (,(pcre-to-elisp "(.*)/config/routes\\.rb$") "\\1/test/routing/routes_test.rb")
    (,(pcre-to-elisp "(.*)/app/views/(.*)$") "\\1/test/views/\\2_test.rb")
    (,(pcre-to-elisp "(.*)/app/controllers/(.*)\\.rb$")
     "\\1/test/controllers/\\2_test.rb"
     "\\1/test/functional/\\2_test.rb")
    (,(pcre-to-elisp "(.*)/app/models/(.*)\\.rb$")
     "\\1/test/models/\\2_test.rb"
     "\\1/test/unit/\\2_test.rb")
    (,(pcre-to-elisp "(.*)/app/(.*)\\.rb$")
     "\\1/test/\\2_test.rb")
    (,(pcre-to-elisp "(.*)/lib/(tasks/.*)\\.rake$") "\\1/test/\\2_tasks_test.rb")
    (,(pcre-to-elisp "(.*)/lib/(.*)\\.rb$")
     "\\1/test/\\2_test.rb"
     "\\1/test/unit/\\2_test.rb"
     "\\1/test/lib/\\2_test.rb")
    (,(pcre-to-elisp "(.*)\\.rb$") "\\1_test.rb")
    )
  "Regular expressions to map Ruby unit to implementation
filenames. The first element in each list is the match, the
second the replace expression."
  :type '(list)
  :group 'ruby-test)

;;;###autoload
(define-minor-mode ruby-test-mode
  "Toggle Ruby-Test minor mode.
With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode."
  :init-value nil
  :lighter " Ruby-Test"
  :keymap 'ruby-test-mode-map
  :group 'ruby-test)

(defun select (fn ls)
  "Create a list from elements of list LS for which FN returns
non-nil."
  (let ((result nil))
    (dolist (item ls)
      (if (funcall fn item)
          (setq result (cons item result))))
    (reverse result)))

(defalias 'find-all 'select)

(defun ruby-test-spec-p (filename)
  (and (stringp filename) (string-match "spec\.rb$" filename)))

(defun ruby-test-p (filename)
  (and (stringp filename) (string-match "test\.rb$" filename)))

(defun ruby-test-any-p (filename)
  (or (ruby-test-spec-p filename)
      (ruby-test-p filename)))

(defun ruby-test-file-name-extension-p (&optional filename)
  "Returns t if the minor mode should be enabled for the current
buffer's filename or the optional filename argument."
  (member
   (file-name-extension (or filename buffer-file-name))
   ruby-test-file-name-extensions))

(defun ruby-test-find-file ()
  "Find the test file to run in number of diffeerent ways:
current buffer (if that's a test; another open buffer which is a
test; or the last run test (if there was one)."
  (let ((files))
    (if (buffer-file-name)
        (setq files (cons (buffer-file-name) files)))
    (setq files (append
                 (mapcar
                  (lambda (win-name) (buffer-file-name (window-buffer win-name)))
                  (window-list))))
    (if (boundp 'ruby-test-last-run)
        (nconc files (list ruby-test-last-run)))
    (setq ruby-test-last-run (car (select 'ruby-test-any-p (select 'identity files))))))

(defun ruby-test-find-target-filename (filename mapping)
  "Find the target filename by matching FILENAME with the first
element of each list in mapping, and replacing the match with the
second element."
  (let ((target-filename nil))
    (while (and (not target-filename) mapping)
      (let ((regexp-match (car (car mapping)))
            (regexp-replace-candidates (cdr (car mapping))))
        (if (string-match regexp-match filename)
            (let ((target-filename-candidates
                   (mapcar #'(lambda (regexp)
                               (replace-match regexp nil nil filename nil))
                           regexp-replace-candidates))
                  exist-filename)
              (setq target-filename
                    (or (dolist (filename target-filename-candidates exist-filename)
                          (unless exist-filename
                            (setq exist-filename (if (file-exists-p filename)
                                                     filename
                                                   nil))))
                        (car target-filename-candidates)))))
        (setq mapping (cdr mapping))))
    target-filename))

(defun ruby-test-find-testcase-at (file line)
  (with-current-buffer (get-file-buffer file)
    (goto-char (point-min))
    (forward-line (1- line))
    (end-of-line)
    (message "%s:%s" (current-buffer) (point))
    (if (re-search-backward (concat "^[ \t]*\\(def\\|test\\|it\\|should\\)[ \t]+"
                                    "\\([\"']\\(.*?\\)[\"']\\|" ruby-symbol-re "*\\)"
                                    "[ \t]*") nil t)
        (let ((name (or (match-string 3)
                        (match-string 2)))
              (method (match-string 1)))
          (ruby-test-testcase-name name method)))))

(defun ruby-test-testcase-name (name method)
  "Returns the sanitized name of the test"
  (cond
   ;; assume methods created with it are from minitest
   ;; so no need to sanitize them
   ((string= method "it")
    name)
   ((string= name "setup")
    nil)
   ((string-match "^[\"']\\(.*\\)[\"']$" name)
    (replace-regexp-in-string
     "\\?" "\\\\\\\\?"
     (replace-regexp-in-string
      "'_?\\|(_?\\|)_?" ".*"
      (replace-regexp-in-string " +" "_" (match-string 1 name)))))
   ((string= method "def")
    name)))

(defun ruby-test-implementation-filename (&optional filename)
  "Returns the implementation filename for the current buffer's
filename or the optional FILENAME, else nil."
  (let ((filename (or filename (buffer-file-name))))
    (ruby-test-find-target-filename filename ruby-test-implementation-filename-mapping)))

(defun ruby-test-implementation-p (&optional filename)
  "Returns t if the current buffer's filename or the given
filename is a Ruby implementation file."
  (let ((filename (or filename buffer-file-name)))
    (and (file-readable-p filename)
         (string-match (regexp-opt ruby-test-file-name-extensions)
                       (file-name-extension filename))
         (not (string-match "_spec\\.rb$" filename))
         (not (string-match "_test\\.rb$" filename)))))

(defvar ruby-test-not-found-message "No test among visible buffers or run earlier.")

;;;###autoload
(defun ruby-test-run ()
  "Run the current buffer's file as specification or unit test."
  (interactive)
  (let ((filename (ruby-test-find-file)))
    (if filename
        (ruby-test-run-command (ruby-test-command filename))
      (message ruby-test-not-found-message))))

;;;###autoload
(defun ruby-test-run-at-point ()
  "Run test at point individually, using the same search strategy
as `ruby-test-run-file'"
  (interactive)
  (let ((filename (ruby-test-find-file)))
    (let ((test-file-buffer (get-file-buffer filename)))
      (if (and filename
               test-file-buffer)
          (with-current-buffer test-file-buffer
            (let ((line (line-number-at-pos (point))))
              (ruby-test-run-command (ruby-test-command filename line))))
        (message ruby-test-not-found-message)))))

(defun ruby-test-run-command (command)
  (setq default-directory (or (ruby-test-rails-root filename)
                              (ruby-test-ruby-root filename)
                              default-directory))
  (compilation-start command t))

(defun ruby-test-command (filename &optional line-number)
  "Return the command to run a unit test or a specification
depending on the filename."
  (cond ((ruby-test-spec-p filename)
         (ruby-test-spec-command filename line-number))
        ((ruby-test-p filename)
         (ruby-test-test-command filename line-number))
        (t (message "File is not a known ruby test file"))))

(defun ruby-test-spec-command (filename &optional line-number)
  (let (command options)
    (if (file-exists-p ".zeus.sock")
        (setq command "zeus rspec")
      (setq command "bundle exec rspec"))
    (setq options ruby-test-rspec-options)
    (if line-number
        (setq filename (format "%s:%s" filename line-number)))
    (format "%s %s %s" command (mapconcat 'identity options " ") filename)))

(defun ruby-test-test-command (filename &optional line-number)
  (let (command options name-options)
    (if (file-exists-p ".zeus.sock")
        (setq command "zeus test")
      (setq command "bundle exec ruby"))
    (if (ruby-test-gem-root filename)
        (setq options (cons "-rubygems" options)))
    (setq options (cons "-I'lib:test'" options))
    (if line-number
        (let ((test-case (ruby-test-find-testcase-at filename line-number)))
          (if test-case
              (setq name-options (format "--name \"/%s/\"" test-case))
            (error "No test case at %s:%s" filename line-number)))
      (setq name-options ""))
    (format "%s %s %s %s" command (mapconcat 'identity options " ") filename name-options)))

(defun ruby-test-project-root (filename root-predicate)
  "Returns the project root directory for a FILENAME using the
given ROOT-PREDICATE, else nil. The function returns a directory
if any of the directories in FILENAME is tested to t by
evaluating the ROOT-PREDICATE."
  (if (funcall root-predicate filename)
      filename
    (and
     filename
     (not (string= "/" filename))
     (ruby-test-project-root
      (file-name-directory
       (directory-file-name (file-name-directory filename)))
      root-predicate))))

(defun ruby-test-project-root-p (directory candidates)
  "Returns t if one of the filenames in CANDIDATES is existing
relative to the given DIRECTORY, else nil."
  (let ((found nil))
    (while (and (not found) (car candidates))
      (setq found
            (file-exists-p
             (concat (file-name-as-directory directory) (car candidates))))
      (setq candidates (cdr candidates)))
    found))

(defun ruby-test-rails-root (filename)
  "Returns the Ruby on Rails project directory for the given
FILENAME, else nil."
  (ruby-test-project-root filename 'ruby-test-rails-root-p))

(defun ruby-test-rails-root-p (directory)
  "Returns t if the given DIRECTORY is the root of a Ruby on
Rails project, else nil."
  (and (ruby-test-ruby-root-p directory)
       (ruby-test-project-root-p directory
                                 '("config/environment.rb" "config/database.yml"))))

(defun ruby-test-gem-root (filename)
  "Returns the gem project directory for the given
FILENAME, else nil."
  (ruby-test-project-root filename 'ruby-test-gem-root-p))

(defun ruby-test-gem-root-p (directory)
  "Returns t if the given DIRECTORY is the root of a Ruby on
gem, else nil."
  (and (ruby-test-ruby-root-p directory)
       (> (length (directory-files directory nil ".gemspec")) 0)))

(defun ruby-test-ruby-root (filename)
  "Returns the Ruby project directory for the given FILENAME,
else nil."
  (ruby-test-project-root filename 'ruby-test-ruby-root-p))

(defun ruby-test-ruby-root-p (directory)
  "Returns t if the given DIRECTORY is the root of a Ruby
project, else nil."
  (or (ruby-test-project-root-p directory '("Rakefile"))
      (ruby-test-project-root-p directory '("Rakefile.rb"))
      (ruby-test-project-root-p directory '("spec"))
      (ruby-test-project-root-p directory '("test"))))

(defun ruby-test-specification-filename (&optional filename)
  "Returns the specification filename for the current buffer's
filename or the optional FILENAME, else nil."
  (let ((filename (or filename (buffer-file-name))))
    (ruby-test-find-target-filename filename ruby-test-specification-filename-mapping)))

;;;###autoload
(defun ruby-test-toggle-implementation-and-specification (&optional filename)
  "Toggle between the implementation and specification/test file
for the current buffer or the optional FILENAME."
  (interactive)
  (let ((filename (or filename (buffer-file-name))))
    (cond ((ruby-test-implementation-p filename)
           (cond ((file-exists-p (ruby-test-specification-filename filename))
                  (find-file (ruby-test-specification-filename filename)))
                 ((file-exists-p (ruby-test-unit-filename filename))
                  (find-file (ruby-test-unit-filename filename)))
                 ((ruby-test-default-test-filename filename)
                  (find-file (ruby-test-default-test-filename filename)))
                 (t
                  (put-text-property 0 (length filename) 'face 'bold filename)
                  (message "Sorry, can't guess unit/specification filename from %s." filename))))
          ((or (ruby-test-spec-p filename) (ruby-test-p filename))
           (find-file (ruby-test-implementation-filename filename)))
          (t
           (put-text-property 0 (length filename) 'face 'bold filename)
           (message "Sorry, %s is neither a Ruby implementation nor a test file." filename)))))

(defun ruby-test-unit-filename (&optional filename)
  "Returns the unit filename for the current buffer's filename or
the optional FILENAME, else nil."
  (let ((filename (or filename (buffer-file-name))))
    (ruby-test-find-target-filename filename ruby-test-unit-filename-mapping)))

(defun ruby-test-default-test-filename (filename)
  "Returns the default test filename"
  (cond ((and (string-equal ruby-test-default-library "test")
              (ruby-test-unit-filename filename))
         (ruby-test-unit-filename filename))
        ((and (string-equal ruby-test-default-library "spec")
              (ruby-test-specification-filename filename))
         (ruby-test-specification-filename filename))
        (t nil)))

(defun ruby-test-enable ()
  "Enable the ruby-test-mode."
  (ruby-test-mode t))

(add-hook 'ruby-mode-hook 'ruby-test-enable)

(provide 'ruby-test-mode)

;;; ruby-test-mode.el ends here
