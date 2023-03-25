;;; init-local.el --- Load the custom configures. -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(windmove-default-keybindings)
(delete-selection-mode t)

(defun osx-copy (beg end)
  (interactive "r")
  (call-process-region beg end  "pbcopy"))

(defun osx-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil))

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

(require-package 'ctrlf)
(defface ctrlf-highlight-active
  '((t (:weight bold :foreground "medium blue" :background "#5AC896")))
  "ctrlf highlight active face")
(ctrlf-mode 1)


(add-to-list 'load-path "site-lisp/xah-fly-keys")
(setq xah-fly-use-meta-key nil
      xah-fly-use-control-key nil)

(require 'xah-fly-keys)
(require 'edit-autosave)

(xah-fly-keys-set-layout "qwerty")
(define-key xah-fly-command-map (kbd "n") 'ctrlf-forward-default)
(define-key xah-fly-command-map (kbd "2") 'delete-window)
(define-key xah-fly-command-map (kbd "M-<SPC>") nil)
(define-key xah-fly-command-map (kbd "'") 'avy-goto-line)
(define-key xah-fly-command-map (kbd "C-'") 'avy-goto-char)
(define-key xah-fly-insert-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
(define-key xah-fly-leader-key-map (kbd "r") 'anzu-query-replace)

(xah-fly-keys 1)

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(provide 'init-local)

;;; init-local.el ends here
