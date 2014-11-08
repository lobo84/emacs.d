(setq-default major-mode 'text-mode)

;; for tilde to work
(require 'iso-transl) 

(require 'package)
;; common lisp
(require 'cl) 

(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(defvar my-packages
  '(haskell-mode 
    markdown-mode 
    projectile 
    python		 
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
