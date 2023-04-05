;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq auto-mode-case-fold nil)
;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L167
(setq ad-redefinition-action 'accept)
;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L194
(setq initial-major-mode 'fundamental-mode)
;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L358
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))


;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
