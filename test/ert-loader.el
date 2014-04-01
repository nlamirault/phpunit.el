;;; ert-loader.el --- Load Ert if not included in Emacs

(require 'f)
(eval-when-compile
  (require 'cl))


(defvar phpunit-root-path
  (f-parent (f-parent load-file-name))
  "Path to root.")

(defvar phpunit-vendor-path
  (f-expand "vendor" phpunit-root-path)
  "Path to vendor.")

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" phpunit-vendor-path)))

