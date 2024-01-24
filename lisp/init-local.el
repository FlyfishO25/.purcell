;;; init-local.el --- Load the custom configures. -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; packages
(require-package 'ctrlf)
(require-package 'ace-window)
(require-package 'prescient)
(require-package 'corfu-prescient)
(require-package 'popper)
(require-package 'vertico-prescient)
;; (require-package 'vertico-posframe)

(setq +modeline-xah-status " C ")

;; tools
(delete-selection-mode t)

(defun osx-copy (beg end)
  "Perform copy from BEG to END to clipboard on macOS."
  (interactive "r")
  (call-process-region beg end  "pbcopy"))

(defun osx-paste ()
  "Perform paste from clipboard on macOS."
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
      (message "Windows Operating System is not supported.")
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

(require-package 'xah-fly-keys)
(require 'xah-fly-keys)

(define-key xah-fly-command-map (kbd "n") 'ctrlf-forward-default)
(xah-fly-keys-set-layout "qwerty")
(define-key xah-fly-command-map (kbd "2") 'delete-window)
(define-key xah-fly-command-map (kbd "M-<SPC>") nil)
(define-key xah-fly-command-map (kbd "'") 'avy-goto-line)
(define-key xah-fly-command-map (kbd ",") 'ace-window)
(define-key xah-fly-command-map (kbd "C-'") 'avy-goto-char)
(define-key xah-fly-insert-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
(define-key xah-fly-leader-key-map (kbd "r") 'anzu-query-replace)

(global-set-key (kbd "C-c r") 'consult-ripgrep)

(defface +modeline-meta-active-face
  '((t (:inherit (font-lock-function-name-face bold) :inverse-video t)))
  "Face used for meta panel on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-meta-inactive-face
  '((t (:inherit (bold font-lock-function-name-face))))
  "The face for meta panel on the mode-line of an inactive window."
  :group '+modeline)

(defun my-config-xah-fly-key-command ()
  "Modify keys for xah fly key command mode keys to be added to `xah-fly-command-mode-activate-hook'."
  (interactive)
  (setq +modeline-xah-status " C ")
  (set-face-foreground '+modeline-meta-active-face "#e78c45")
  (set-face-foreground '+modeline-meta-inactive-face "#e78c45")
  ;; more here
  )

(defun my-config-xah-fly-key-insert ()
  "Modify keys for xah fly key command mode keys to be added to `xah-fly-insert-mode-activate-hook'."
  (interactive)
  (setq +modeline-xah-status " I ")
  (set-face-foreground '+modeline-meta-active-face "#5AC896")
  (set-face-foreground '+modeline-meta-inactive-face "#5AC896")
  ;; more here
  )

(add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key-command)
(add-hook 'xah-fly-insert-mode-activate-hook 'my-config-xah-fly-key-insert)

(xah-fly-keys 1)

;; mac configure
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; C/C++ mode configure
(setq c-basic-offset 4)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n" (format ";; Init completed in %.2fms"
                                                                                       (sanityinc/time-subtract-millis after-init-time before-init-time))))

(unless (display-graphic-p)
  (require-package 'corfu-terminal)
  (corfu-terminal-mode +1))

;; mode-line
;;; Get current window
(defvar +modeline-current-window nil)
(defun +modeline-set-selected-window (&rest _)
  "Set `+modeline-current-window' appropriately."
  (let ((win (frame-selected-window)))
    (setq +modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defsubst +modeline-window-active-p ()
  "Whether is an active window."
  (eq (frame-selected-window) +modeline-current-window))
(add-hook 'pre-redisplay-functions #'+modeline-set-selected-window)

;;; Check whether `window-total-width' is larger than the limit
(defconst +modeline-window-width-limit 90)
(defvar-local +modeline-large-width-p nil)
(defun +modeline-window-size-change-function (&rest _)
  "Function for `window-size-change-functions'."
  (setq +modeline-large-width-p
        (> (window-total-width) +modeline-window-width-limit)))
(add-hook 'after-revert-hook #'+modeline-window-size-change-function)
(add-hook 'buffer-list-update-hook #'+modeline-window-size-change-function)
(add-hook 'window-size-change-functions #'+modeline-window-size-change-function)

;;;; face
(defgroup +modeline nil
  "Modeline faces."
  :group 'faces)

(defface +modeline-line-number-active-face
  '((t (:inherit (mode-line-inactive) :inverse-video t)))
  "The face for line number on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-vc-mode-active-face
  '((t (:inherit (font-lock-constant-face))))
  "The face for `vc-mode' on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-modification-active-face
  '((t (:inherit (font-lock-function-name-face))))
  "The face for modification indicator on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-project-name-active-face
  '((t (:inherit (bold font-lock-variable-name-face))))
  "The face for project name on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-project-name-inactive-face
  '((t (:inherit (mode-line-inactive))))
  "The face for project name on the mode-line of an inactive window."
  :group '+modeline)

(defface +modeline-buffer-name-active-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for buffer name on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-buffer-name-inactive-face
  '((t (:inherit (mode-line-inactive bold))))
  "The face for buffer name on the mode-line of an inactive window."
  :group '+modeline)

(defface +modeline-host-name-active-face
  '((t (:inherit (font-lock-function-name-face bold italic))))
  "The face for host name on the mode-line of an active window."
  :group '+modeline)

;;; Indicators
(with-eval-after-load 'popper
  ;; modeline indicator
  (setq popper-mode-line
        '(:propertize " POP |"
                      face +modeline-meta-active-face)))

(defsubst +modeline-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "| MacroDef ")
        (executing-kbd-macro "| MacroExc ")))

(defsubst +modeline-multiple-cursors-indicator ()
  "Display the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (format "| %d cursors " (mc/num-cursors))))

(defsubst +modeline-use-region-indicator ()
  "Display selected region in current buffer."
  (when (use-region-p)
    (format "| L%d W%d C%d "
            (count-lines (region-beginning) (region-end))
            (count-words (region-beginning) (region-end))
            (abs (- (mark t) (point))))))

(defsubst +modeline-overwrite-indicator ()
  "Display whether it is in overwrite mode."
  (when overwrite-mode "| Ovr "))

(defsubst +modeline-symbol-overlay-indicator ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (format (concat  "| %d/%d sym " (and (cadr keyword) "in scope "))
                  (+ count 1)
                  (+ count (length after)))))))


;;; Cache project name
(defvar-local +modeline-project-name nil)
(defsubst +modeline-update-project-name ()
  "Get project name for current buffer."
  (setq +modeline-project-name
        (when (buffer-file-name)
          (when-let ((project (project-current)))
            (concat
             (file-name-nondirectory
              (directory-file-name (project-root project)))
             ":")))))
(add-hook 'find-file-hook #'+modeline-update-project-name)
(add-hook 'after-change-major-mode-hook #'+modeline-update-project-name)

;;; Cache remote host name
(defvar-local +modeline-remote-host-name nil)
(defsubst +modeline-update-remote-host-name ()
  "Hostname for remote buffers."
  (setq +modeline-remote-host-name
        (when-let ((hostname (and default-directory
                                  (file-remote-p default-directory 'host))))
          (when (not (string-equal hostname "localhost"))
            (format "@%s" hostname)))
        ))
(add-hook 'find-file-hook #'+modeline-update-remote-host-name)

;;; Cache flymake report
(defvar-local +modeline-flymake-indicator nil)
(defun +modeline-flymake-update (&rest _)
  "Display flymake info for current buffer."
  (setq +modeline-flymake-indicator
        (when (and flymake-mode (flymake-running-backends))
          (let* ((err-count (cadadr (flymake--mode-line-counter :error t)))
                 (warning-count (cadadr (flymake--mode-line-counter :warning t)))
                 (note-count (cadadr (flymake--mode-line-counter :note t)))
                 (err (when err-count (propertize err-count 'face '(:inherit compilation-error))))
                 (warning (when warning-count (propertize (concat " " warning-count) 'face '(:inherit compilation-warning))))
                 (note (when note-count (propertize (concat " " note-count) 'face '(:inherit compilation-info)))))
            (concat " [" err warning note "]"))))
  )
(advice-add #'flymake--handle-report :after #'+modeline-flymake-update)
(add-hook 'flymake-mode-hook #'+modeline-flymake-update)

;;; Cache encoding info
(defvar-local +modeline-encoding nil)
(defsubst +modeline-update-encoding (&rest _)
  "Get encoding and EOL type of current buffer."
  (setq +modeline-encoding
        `(,(if (memq (coding-system-category buffer-file-coding-system)
                     '(coding-category-undecided coding-category-utf-8))
               "UTF-8"
             (upcase (symbol-name (coding-system-get buffer-file-coding-system :name))))
          ,(pcase (coding-system-eol-type buffer-file-coding-system)
             (0 ":LF ")
             (1 ":CRLF ")
             (2 ":CR ")
             (_ " ")))))
(add-hook 'find-file-hook #'+modeline-update-encoding)
(advice-add #'after-insert-file-set-coding :after #'+modeline-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+modeline-update-encoding)

(defsubst +mode-line-active-long ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize +modeline-xah-status
                             face +modeline-meta-active-face)
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        ;; (+modeline-anzu-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-symbol-overlay-indicator)
                                        (+modeline-use-region-indicator)
                                        (+modeline-overwrite-indicator)))
                             face +modeline-meta-active-face)
                (:propertize " %*" face +modeline-modification-active-face)
                " %I "
                (:propertize +modeline-project-name
                             face +modeline-project-name-active-face)
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                (:propertize vc-mode
                             face +modeline-vc-mode-active-face)
                ))
         (rhs '(
                (:propertize mode-name
                             face +modeline-buffer-name-active-face)
                (:eval +modeline-flymake-indicator)
                " "
                (:eval +modeline-encoding)
                (:propertize " %l,%C "
                             face +modeline-line-number-active-face)
                " "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))


(defsubst +mode-line-inactive-long ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize +modeline-xah-status
                             face +modeline-meta-inactive-face)
                "%* %I "
                (:propertize +modeline-project-name
                             face +modeline-project-name-inactive-face)
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                (:eval vc-mode)
                ))
         (rhs '((:eval mode-name)
                " "
                (:eval +modeline-encoding)
                "%l,%C "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))


(defsubst +mode-line-active-short ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize +modeline-xah-status
                             face +modeline-meta-active-face)
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        ;; (+modeline-anzu-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-symbol-overlay-indicator)
                                        (+modeline-use-region-indicator)
                                        (+modeline-overwrite-indicator)))
                             face +modeline-meta-active-face)
                (:propertize " %*" face +modeline-modification-active-face)
                " "
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                ))
         (rhs '(
                (:propertize mode-name
                             face +modeline-buffer-name-active-face)
                " "
                (:eval +modeline-encoding)
                (:propertize " %l "
                             face +modeline-line-number-active-face)
                " "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(defsubst +mode-line-inactive-short ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize +modeline-xah-status
                             face +modeline-meta-inactive-face)
                "%* "
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                ))
         (rhs '(" %l  "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(setq-default mode-line-format
              '((:eval (if (+modeline-window-active-p)
                           (if +modeline-large-width-p (+mode-line-active-long) (+mode-line-active-short))
                         (if +modeline-large-width-p (+mode-line-inactive-long) (+mode-line-inactive-short))))))

(setq-default header-line-format nil)

(vertico-reverse-mode)
(vertico-prescient-mode)
(add-hook 'corfu-mode-hook #'corfu-prescient-mode)
(prescient-persist-mode)
;; (vertico-posframe-mode)

(setq markdown-command
      '("pandoc"
        "--from=markdown"
        "--to=html5"
        "--no-highlight"
        "--standalone=false"
        "--html-q-tags=false"
        "--toc=false"
        "--wrap=none"))
(setq markdown-enable-math t)
(setq markdown-command-needs-filename t)

(setq custom-enabled-themes '(modus-vivendi))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-operandi))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-vivendi))
  (reapply-themes))


(message "excuted personal script")
(provide 'init-local)

;;; init-local.el ends here
