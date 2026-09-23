;;; init-preload-local.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Something that I want to load earlier.

;;; Code:

(setq exec-path-from-shell-arguments (list "-l"))
(set-frame-font "Menlo 18" nil t)

(provide 'init-preload-local)

;;; init-preload-local.el ends here
