(setq-default major-mode 'text-mode)

;; for tilde to work
(require 'iso-transl) 

(require 'package)
;; common lisp
(require 'cl)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages
  '(haskell-mode
    markdown-mode
    projectile
    python
    rust-mode
    solarized-theme
    zenburn-theme org)
  )

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) 
	do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(load-theme 'zenburn t)
(custom-set-faces
 '(default ((t (
		:family "Droid Sans Mono" 
			:foundry "unknown" 
			:slant normal 
			:weight normal 
			:height 90 
			:width normal)))))

(custom-set-variables
 '(column-number-mode t)
 '(tool-bar-mode nil))


;; Rename file
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)
(put 'narrow-to-region 'disabled nil)
