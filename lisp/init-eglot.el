;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (setq-default eglot-extend-to-xref t)
  (setq eglot-code-action-indicator "✓"
        eglot-code-action-indications '(eldoc-hint mode-line))

  (defun sanityinc/disable-eglot-semantic-tokens ()
    (when (fboundp 'eglot-semantic-tokens-mode)
      (eglot-semantic-tokens-mode -1)))

  (add-hook 'eglot-managed-mode-hook
            #'sanityinc/disable-eglot-semantic-tokens)

  (dolist (hook '(c-mode-hook c++-mode-hook
                              c-ts-mode-hook c++-ts-mode-hook))
    (add-hook hook #'eglot-ensure))

  (maybe-require-package 'consult-eglot))



(provide 'init-eglot)
;;; init-eglot.el ends here
