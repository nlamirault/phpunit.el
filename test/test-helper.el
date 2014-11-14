;; test-helper.el --- Test helpers for Phpunit.el

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
(require 'f)


(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(defvar username (getenv "HOME"))

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

(defun cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[phpunit] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path))

(defun load-unit-tests (path)
  "Load all unit test from PATH."
  (dolist (test-file (or argv (directory-files path t "-test.el$")))
    (load test-file nil t)))


(provide 'test-helper)
;;; test-helper.el ends here
