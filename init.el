(setq-default major-mode 'text-mode)

;; for tilde to work
(require 'iso-transl)

(require 'package)
;; common lisp
(require 'cl)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defvar my-packages
  '(haskell-mode
    markdown-mode
    projectile
    python
    rust-mode
    solarized-theme
    zenburn-theme org
    irony
    company
    company-irony
    flycheck
    flycheck-irony
    auto-complete-clang
    rtags
    cmake-ide
    magit)
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

(load-theme 'solarized-dark t)

(put 'narrow-to-region 'disabled nil)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; Add company-irony to your company backends.
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


;;; init.el ends here
(setq inhibit-startup-screen t)

(global-set-key (kbd "C-x g") 'magit-status)
(setq cmake-ide-build-dir "/home/niclas/projects/test2/build")
(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
