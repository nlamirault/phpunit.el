;;; phpunit-test.el --- Tests for phpunit.el

;; Copyright (C) 2014, 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
  (let ((composer-dir (s-concat (concat (getenv "HOME") "/") ".composer"))
        (conf (if phpunit-configuration-file
                  (s-concat "-c " phpunit-configuration-file " ")
                "")))
    (if (f-dir? composer-dir)
        (apply 's-concat composer-dir "/vendor/bin/phpunit " conf arg)
      (apply 's-concat "./vendor/bin/phpunit " conf arg))))


(ert-deftest test-phpunit-get-class-from-file-path()
  :tags '(tools)
  (should (string= "PhpUnitTest"
		   (phpunit-get-current-class "/tmp/foo/PhpUnit.class.under.test.php"))))

(ert-deftest test-phpunit-get-class-from-source-class()
    :tags '(tools)
  (should (string= "PhpUnitTest"
		   (phpunit-get-current-class "PhpUnit"))))

(ert-deftest test-phpunit-get-class-from-unit-test-class()
    :tags '(tools)
  (should (string= "PhpUnitTest"
		   (phpunit-get-current-class "PhpUnitTest"))))


;; Using configuration file

(ert-deftest test-phpunit-with-configuration-file ()
  :tags '(configuration-file)
  (with-test-sandbox
   (let ((phpunit-configuration-file "phpunit.xml"))
     (should (s-contains? "-c phpunit.xml"
                          (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-without-configuration-file ()
  :tags '(configuration-file)
  (with-test-sandbox
   (should-not (s-contains? "-c phpunit.xml"
                            (phpunit-get-program (phpunit-arguments ""))))))

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
     (should (s-suffix? (phpunit-command " --stop-on-error")
                      (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-add-stop-on-failure-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-stop-on-failure t))
     (should (s-suffix? (phpunit-command " --stop-on-failure")
                      (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-add-stop-on-skipped-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-stop-on-skipped t))
     (should (s-suffix? (phpunit-command " --stop-on-skipped")
                      (phpunit-get-program (phpunit-arguments "")))))))

(ert-deftest test-phpunit-add-verbose-argument ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((phpunit-verbose-mode t))
     (should (s-suffix? (phpunit-command " --verbose")
                      (phpunit-get-program (phpunit-arguments "")))))))


(provide 'phpunit-test)
;;; phpunit-test.el ends here
