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
    flycheck-irony)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 98 :width normal)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(irony-cdb-compilation-databases (quote (irony-cdb-json)))
 '(tool-bar-mode nil))

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Irony mode for c++
;; install libclang then run irony-install-server
;; for cmake projectes generate a compilation database with
;; -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Add company-irony to your company backends.
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Add company mode to all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; Add flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;; init.el ends here
