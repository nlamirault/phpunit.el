;;; phpunit.el --- Launch PHP unit tests using phpunit

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/phpunit.el
;; Version: 0.1.0
;; Keywords: php, tests, phpunit

;; Package-Requires: ((s "1.9.0") (f "0.16.0") (pkg-info "0.5"))

;;; License:

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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


(defconst php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")


;; Commands
;; -----------


(defun phpunit-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to phpunit command line arguments."
  (s-concat phpunit-program " -c "
	    (phpunit-get-root-directory)
	    "phpunit.xml"
	    args))


(defun phpunit-get-root-directory()
  "Return the root directory to run tests."
  (let ((filename (buffer-file-name)))
    (when filename
      (file-truename (or (locate-dominating-file filename "phpunit.xml")
			 "./")))))

(defun phpunit-get-current-class (&optional file)
  "Return the class name of the PHPUnit test for `FILE'."
  (let* ((file (or file (buffer-file-name))))
    (f-filename (replace-regexp-in-string "\\.php\\'" "" file))))


(defun phpunit-get-current-test ()
  (save-excursion
    (when (re-search-backward php-beginning-of-defun-regexp nil t)
      (match-string-no-properties 1))))

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


(defun phpunit-run (args)
  (compile (phpunit-get-program (phpunit-arguments args))))


;; API
;; ----


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
  (let ((args (s-concat " --filter '" (phpunit-get-current-class) "'")))
    (phpunit-run args)))


;;;###autoload
(defun phpunit-current-project ()
  "Launch PHPUnit on current project."
  (interactive)
  (phpunit-run ""))


(provide 'phpunit)
;;; phpunit.el ends here
