;;; phpunit.el --- Launch PHP unit tests using phpunit.

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; Homepage: https://github.com/nlamirault/phpunit.el
;; Version: 0.1.0
;; X-Original-Version: 0.1.0
;; Keywords: php, tests, phpunit

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

;; (define-key web-mode-map (kbd "C-c c") 'phpunit-current-class)
;; (define-key web-mode-map (kbd "C-c p") 'phpunit-current-project)

;;; Code:

(require 's)
(require 'f)

(defvar phpunit-program "phpunit"
  "PHPUnit binary path.")

(defvar phpunit-arg ""
  "Argument to pass to phpunit.")

;; Commands
;; -----------


(defun phpunit-get-program (test)
  "Return the command to launch unit test.
`TEST' corresponds to a classname, or a testname."
  (concat phpunit-program " -c "
	  (phpunit-get-root-directory)
	  "phpunit.xml"
	  test))


(defun phpunit-get-root-directory()
  "Return the root directory to run tests."
  (file-truename (or (locate-dominating-file
                      (buffer-file-name) "phpunit.xml")
                     "./")))


(defun phpunit-get-current-class (&optional file)
  "Return the class name of the PHPUnit test for `FILE'."
  (let* ((file (or file (buffer-file-name))))
    ;;(f-filename (replace-regexp-in-string "\\(tests/\\|\\(Test\\)?\.php$\\)" "" file))))
    (f-filename (replace-regexp-in-string "\\.php\\'" "" file))))

;; API
;; -----


;;;###autoload
;; (defun phpunit-current-test ()
;;   (let ((args (s-concat " --filter '"
;; 			(phpunit-get-current-class) "::" (phpunit-get-current-test) "'")))
;;     (compile (phpunit-get-program args))))


;;;###autoload
(defun phpunit-current-class ()
  (interactive)
  (let ((args (s-concat " --filter '" (phpunit-get-current-class) "'")))
    (compile (phpunit-get-program args))))


;;;###autoload
(defun phpunit-current-project ()
  "Launch phphunit for current project."
  (interactive)
  (compile (phpunit-get-program "")))


(provide 'phpunit)
;;; phpunit.el ends here
