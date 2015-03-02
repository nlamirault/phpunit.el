;;; phpunit-test.el --- Tests for phpunit.el

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;;; Commentary:

;; Unit tests for phpunit.el

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

;;; Code:


;; (require 'test-helper)
;; (require 'phpunit)


(defun phpunit-command (&rest arg)
  (apply 's-concat "phpunit -c " "phpunit.xml" arg))


(ert-deftest test-phpunit-get-current-class ()
  (should (string= "PhpUnitTest"
		   (phpunit-get-current-class "/tmp/foo/PhpUnitTest.php"))))

;; Arguments

(ert-deftest test-phpunit-get-program-without-args ()
  :tags '(arguments)
  (with-test-sandbox
   (should (string= (phpunit-command)
                    (phpunit-get-program (phpunit-arguments ""))))))

(ert-deftest test-phpunit-add-stop-on-error-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-stop-on-error t))
     (should (string= (phpunit-command " --stop-on-error")
                      (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-add-stop-on-failure-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-stop-on-failure t))
     (should (string= (phpunit-command " --stop-on-failure")
                      (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-add-stop-on-skipped-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-stop-on-skipped t))
     (should (string= (phpunit-command " --stop-on-skipped")
                      (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-add-verbose-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-verbose-mode t))
     (should (string= (phpunit-command " --verbose")
                      (phpunit-get-program (phpunit-arguments "")))))))


(provide 'phpunit-test)
;;; phpunit-test.el ends here
