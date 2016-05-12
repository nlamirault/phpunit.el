;;; phpunit-mode.el --- Minor mode for PHPUnit

(require 'phpunit)

(defvar phpunit-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-t f") 'phpunit-helm-function-tests)
    (define-key map (kbd "C-t t") 'phpunit-current-test)
    (define-key map (kbd "C-t c") 'phpunit-current-class)
    (define-key map (kbd "C-t p") 'phpunit-current-project)
    map)
  "Keymap for PHPUnit minor mode")

(define-minor-mode phpunit-mode
  "PHPUnit minor mode"
  :lighter " phpunit"
  :keymap phpunit-mode-map)

(add-to-list 'auto-mode-alist '("\\.php$'" . phpunit-mode))

;;; phpunit-mode.el ends here
