;; phpunit-test-helper.el --- Test helpers for phpunit.el

;; Copyright (C) 2014-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author: Nicolas Lamirault <nicolas.lamirault@chmouel.com>
;; Homepage: https://github.com/nlamirault/phpunit.el

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ansi)
(require 'cl) ;; http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal
(require 'ert)
(require 'f)
(require 'undercover)


(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(defvar phpunit-test-helper-username (getenv "HOME"))

(defconst phpunit-testsuite-dir (f-parent (f-this-file))
  "The testsuite directory.")

(defconst phpunit-source-dir
  (f-parent phpunit-testsuite-dir)
  "The Phpunit.el source directory.")

;; (message "Running tests on Emacs %s" emacs-version)

;; (message "Load Phpunit : %s" phpunit-source-dir)
;; (load (s-concat phpunit-source-dir "/phpunit.elc"))

(defconst phpunit-sandbox-path
  (f-expand "sandbox" phpunit-testsuite-dir)
  "The sandbox path for phpunit.")

(defun phpunit-test-helper-cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[phpunit] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat phpunit-test-helper-username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path))

(defun phpunit-test-helper-load-unit-tests (path)
  "Load all unit test from PATH."
  (message (ansi-green "[phpunit] Execute unit tests %s"
                       path))
  (dolist (test-file (or argv (directory-files path t "-test.el$")))
    (load test-file nil t)))

(defun phpunit-test-helper-load-library (file)
  "Load current library from FILE."
  (let ((path (s-concat phpunit-source-dir file)))
    (message (ansi-yellow "[phpunit] Load library from %s" path))
    (undercover "*.el" (:exclude "*-test.el"))
    (require 'phpunit path)))

(defmacro phpunit-test-helper-with-test-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(unwind-protect
       (condition-case nil ;ex
           (let ((default-directory phpunit-source-dir))
             (phpunit-test-helper-cleanup-load-path)
             (phpunit-test-helper-load-library "/phpunit.el")
             ,@body))))

;;; test-helper.el ends here
