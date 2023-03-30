;;; init-local.el --- Load the custom configures. -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; packages
(require-package 'ctrlf)
(require-package 'ace-window)

;; tools
(delete-selection-mode t)

(defun osx-copy (beg end)
  (interactive "r")
  (call-process-region beg end  "pbcopy"))

(defun osx-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil))

(defun launch-separate-emacs-in-terminal ()
  "Launch another Emacs process in terminal."
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-gui ()
  "Launch another Emacs process under GUI."
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  "Restart Emacs from within Emacs."
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "Sorry, this function does not support windows.")
    (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                             #'launch-separate-emacs-under-gui
                                                           #'launch-separate-emacs-in-terminal)))))
      (save-buffers-kill-emacs))))


(defun flymacs/delete-empty-line (beg end)
  "Delete empty lines from selected BEG to END, otherwise the whole buffer."
  (interactive "r")
  (let ((empty-line "^[\t  ]*$"))
    (save-excursion
      (if (region-active-p)
          (flush-lines empty-line beg end)
        (flush-lines empty-line (point-min) (point-max))))))
(defun rename-this-file (new-name)
  ;; from https://github.com/seagle0128/.emacs.d/blob/754eb554ca2dd22807898bd5a4257a57f6ab5cfd/lisp/init-funcs.el#L97
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; editing
(require 'edit-autosave)

(defface ctrlf-highlight-active
  '((t (:weight bold :foreground "medium blue" :background "#5AC896")))
  "ctrlf highlight active face")
(ctrlf-mode 1)

;; keybinding
(add-to-list 'load-path "site-lisp/xah-fly-keys")
(setq xah-fly-use-meta-key nil
      xah-fly-use-control-key nil)

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(define-key xah-fly-command-map (kbd "n") 'ctrlf-forward-default)
(define-key xah-fly-command-map (kbd "2") 'delete-window)
(define-key xah-fly-command-map (kbd "M-<SPC>") nil)
(define-key xah-fly-command-map (kbd "'") 'avy-goto-line)
(define-key xah-fly-command-map (kbd ",") 'ace-window)
(define-key xah-fly-command-map (kbd "C-'") 'avy-goto-char)
(define-key xah-fly-insert-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
(define-key xah-fly-leader-key-map (kbd "r") 'anzu-query-replace)

(global-set-key (kbd "C-c r") 'consult-ripgrep)

(xah-fly-keys 1)

;; mac configure
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; C/C++ mode configure
(setq c-basic-offset 4)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)

(global-whitespace-cleanup-mode 0)

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n" (format ";; Init completed in %.2fms"
                                                                                       (sanityinc/time-subtract-millis after-init-time before-init-time))))

(unless (display-graphic-p)
  (require-package 'corfu-terminal)
  (corfu-terminal-mode +1))

;; mode-line
(defun roife/ml/shortened-path (path max-len)
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun roife/ml/mode-info ()
  (let ((info (cond ((derived-mode-p 'calc-mode) (prin1-to-string calc-angle-mode))
                    (t nil))))
    (if info (concat " [" (propertize info 'face '(:foreground "#cc6666")) "]")))
  )

(defvar roife/ml/selected-window nil)
(add-hook 'post-command-hook '(lambda () (setq roife/ml/selected-window (selected-window))))
(add-hook 'buffer-list-update-hook '(lambda () (force-mode-line-update)))
(defun roife/ml/selected-window-p (x y)
  "Return X if the current window is selected, if not, return Y."
  (if (eq roife/ml/selected-window (selected-window)) x y))

(defun roife/ml/fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun roife/ml/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s " err))))

(defun roife/ml/compute-mode-line ()
  (let* ((left (list
                ;; file
                " %* %I "
                '(:eval (propertize (if overwrite-mode "Ovr " "") 'face '(:foreground "#f0c674")))
                '(:eval (propertize
                         (if (and (buffer-file-name) (projectile-project-p))
                             (roife/ml/shortened-path (file-relative-name buffer-file-name (projectile-project-root)) 15)
                           "%b")
                         'face `(:weight ,(roife/ml/selected-window-p 'bold 'normal)
                                         :foreground ,(roife/ml/selected-window-p "#b5bd68" "#969896"))))
                " "

                ))

         (center (list
                  ;; projectile
                  '(:eval (when (and (buffer-file-name) (projectile-project-p))
                            (concat "["(propertize (projectile-project-name)
                                                   'face `(:foreground ,(roife/ml/selected-window-p "#81a2be" "#969896")))
                                    "] ")))
                  ;; major-mode
                  '(:eval (propertize "%m" 'face `(:foreground ,(roife/ml/selected-window-p "#b294bb" "#969896"))))
                  '(:eval (roife/ml/mode-info))
                  ;; flycheck
                  '(:eval
                    (when (and (bound-and-true-p flycheck-mode)
                               (or flycheck-current-errors
                                   (eq 'running flycheck-last-status-change)))
                      (concat
                       " "
                       (propertize " " 'face '(:background "#282a2e"))
                       (cl-loop for state in '((error . "#cc6666")
                                               (warning . "#de935f")
                                               (info . "#8abeb7"))
                                as lighter = (roife/ml/flycheck-lighter (car state))
                                when lighter
                                concat (propertize
                                        lighter
                                        'face `(:foreground ,(cdr state)
                                                            :background "#282a2e")))
                       )))
                  ;; git
                  '(:eval vc-mode)
                  ;; selected
                  " "
                  '(:eval (when (and (use-region-p) (roife/ml/selected-window-p t nil))
                            (concat
                             (propertize (format " C:%d W:%d L:%d "
                                                 (abs (- (mark t) (point)))
                                                 (count-words (region-beginning) (region-end))
                                                 (count-lines (region-beginning) (region-end)))
                                         'face '(:background "#969896" :foreground "#1D1F21"))
                             " ")))
                  ))
         (right (list
                 ;; encoding
                 '(:eval (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
                                       (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                                           (match-string 1 buf-coding)
                                         buf-coding))))
                 " "
                 ;; position
                 '(:eval (propertize " %l: %C " 'face `(:background ,(roife/ml/selected-window-p "#969896" "#373b41")
                                                                    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
                 " "
                 '(-3 "%p")
                 " "
                 ))
         (fill-space (roife/ml/fill 'mode-line (string-width (format-mode-line right))))
         (width-lcr (string-width (format-mode-line (list left center right))))
         (width-lr (string-width (format-mode-line (list left right)))))
    (cond ((> width-lr (window-width)) (list left))
          ((> width-lcr (window-width)) (list left fill-space right))
          (t (list left center fill-space right)))
    ))
(setq-default mode-line-format '(:eval (roife/ml/compute-mode-line)))

(provide 'init-local)

;;; init-local.el ends here
