;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  ;; Customize Flycheck settings here
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc))

  ;; Keybindings similar to Flymake
  (define-key flycheck-mode-map (kbd "C-c ! l") 'flycheck-list-errors)
  (define-key flycheck-mode-map (kbd "C-c ! n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c ! p") 'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c ! c") 'flycheck-buffer))
  (define-key flymake-mode-map (kbd "C-c ! c") 'flymake-start))

(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose)

(provide 'init-flymake)
;;; init-flymake.el ends here
