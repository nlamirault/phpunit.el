;;; phpunit.el --- Launch PHP unit tests using phpunit

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;         Eric Hansen <hansen.c.eric@gmail.com>
;;
;; URL: https://github.com/nlamirault/phpunit.el
;; Version: 0.7.0
;; Keywords: php, tests, phpunit

;; Package-Requires: ((s "1.9.0") (f "0.16.0") (pkg-info "0.5") (Emacs "24") (helm "0.0.0"))

;;; License:

;; Copyright (C) 2014, 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Thanks to tox.el(https://github.com/chmouel/tox.el) from Chmouel Boudjnah.

;; To use this code, bind the functions `phpunit-current-test', `phpunit-current-class'
;; and `phpunit-current-project' to convenient keys with something like :

;; (define-key web-mode-map (kbd "C-x t") 'phpunit-current-test)
;; (define-key web-mode-map (kbd "C-x c") 'phpunit-current-class)
;; (define-key web-mode-map (kbd "C-x p") 'phpunit-current-project)

;;; Code:

(require 's)
(require 'f)

(defgroup phpunit nil
  "PHPUnit utility"
  :group 'php)

(defcustom phpunit-program "phpunit"
  "PHPUnit binary path."
  :type 'file
  :group 'phpunit)

(defcustom phpunit-arg ""
  "Argument to pass to phpunit."
  :type 'string
  :group 'phpunit)

(defcustom phpunit-stop-on-error nil
  "Stop execution upon first error."
  :type 'boolean
  :group 'phpunit)

(defcustom phpunit-stop-on-failure nil
  "Stop execution upon first error or failure."
  :type 'boolean
  :group 'phpunit)

(defcustom phpunit-stop-on-skipped nil
  "Stop execution upon first skipped test."
  :type 'boolean
  :group 'phpunit)

(defcustom phpunit-verbose-mode nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'phpunit)

(defcustom phpunit-configuration-file "phpunit.xml"
  "The PHPUnit configuration file."
  :type 'string
  :group 'phpunit)

(defconst php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")

(defconst php-beginning-of-class
  "^\\s-*class\\s-+&?\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)"
  "Regular expression for a PHP class.")

(defconst php-labelchar-regexp
  "[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]"
  "Valid syntax for a character in a PHP label.")

;; Allow for error navigation after a failed test
(add-hook 'compilation-mode-hook
          (lambda ()
            (interactive)
            (add-to-list 'compilation-error-regexp-alist '("^\\(.+\\.php\\):\\([0-9]+\\)$" 1 2))))

;; Commands
;; -----------

(defun phpunit-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to phpunit command line arguments."
  (s-concat phpunit-program " -c "
	    (phpunit-get-root-directory)
	    phpunit-configuration-file
	    args))


(defun phpunit-get-root-directory()
  "Return the root directory to run tests."
  ;; The function doesn't detect the root directory when used with
  ;; tramp mode. In that case, the phpunit-root-directory variable can
  ;; be set which takes precedence
  (if (boundp 'phpunit-root-directory)
      phpunit-root-directory
    (let ((filename (buffer-file-name)))
      (when filename
        (file-truename (or (locate-dominating-file filename phpunit-configuration-file)
                           "./"))))))



(defun phpunit-get-current-class (&optional class-or-path)
  "Return the canonical unit test class name associated with the current class or buffer."
  (let ((class-name
	 (let ((class-or-filename (f-filename (or class-or-path
						  (save-excursion (re-search-backward php-beginning-of-class 0 t)
								  (match-string 1))
						  (buffer-file-name)))))
	   (string-match (concat "\\(" php-labelchar-regexp "*\\)")
			 class-or-filename)
	   (match-string 1 class-or-filename))))
    (if (string-match "Test$" class-name)
	class-name
      (concat class-name "Test"))))

(defun phpunit-get-current-test ()
  "Get the name of the current test function"
  (save-excursion
    (when (re-search-backward php-beginning-of-defun-regexp nil t)
      (match-string-no-properties 1))))

(defun phpunit-helm-get-all-test-candidates ()
  "Populates Helm with a list of test functions within a class/file."
  (with-helm-current-buffer
    (let ((test-functions '()))
      (save-excursion
	(beginning-of-buffer)
	(while (search-forward-regexp php-beginning-of-defun-regexp nil t)
	  (add-to-list 'test-functions (match-string-no-properties 1))))
      test-functions)))

(setq phpunit-helm-select-test-source
  '((name . "PHPUnit Tests")
    (candidates . phpunit-helm-get-all-test-candidates)
    (action . (lambda (test)
		(phpunit-selected-test test)))))

(defun phpunit-helm-select-test ()
  "Use Helm to select which test should be ran."
  (interactive)
  (require 'helm)
  (helm :sources '(phpunit-helm-select-test-source)
	:buffer "*phpunit-function-tests*"))

(defun phpunit-arguments (args)
  (let ((opts args))
     (when phpunit-stop-on-error
       (setq opts (s-concat opts " --stop-on-error")))
     (when phpunit-stop-on-failure
       (setq opts (s-concat opts " --stop-on-failure")))
     (when phpunit-stop-on-skipped
       (setq opts (s-concat opts " --stop-on-skipped")))
     (when phpunit-verbose-mode
       (setq opts (s-concat opts " --verbose")))
     opts))

(defun phpunit-get-compile-command (args)
  (let ((column-setting-command (format "stty cols %d" (frame-width)))
        (command-separator "; ")
        (phpunit-command (phpunit-get-program (phpunit-arguments args))))
    (concat column-setting-command command-separator phpunit-command)))

(defun phpunit-run (args)
  (compile (phpunit-get-compile-command args)))


;; API
;; ----

;;;###autoload
(defun phpunit-selected-test (test-function)
  "Launch PHPUnit on selected test (Helm)."
  (interactive)
  (let ((args (s-concat " --filter '" (phpunit-get-current-class) "::" test-function "'")))
    (phpunit-run args)))

;;;###autoload
(defun phpunit-current-test ()
  "Launch PHPUnit on curent test."
  (interactive)
  (let ((args (s-concat " --filter '"
			(phpunit-get-current-class)
			"::"
			(phpunit-get-current-test) "'")))
    (phpunit-run args)))


;;;###autoload
(defun phpunit-current-class ()
  "Launch PHPUnit on current class."
  (interactive)
  (let ((args (s-concat " --filter '(?<!" php-labelchar-regexp ")" (phpunit-get-current-class) "'")))
    (phpunit-run args)))


;;;###autoload
(defun phpunit-current-project ()
  "Launch PHPUnit on current project."
  (interactive)
  (phpunit-run ""))


(provide 'phpunit)
;;; phpunit.el ends here
