;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq inhibit-startup-screen t)
(setq show-trailing-whitespace t)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package dynamic-fonts
  :ensure dynamic-fonts
  :init
  (progn (setq dynamic-fonts-preferred-proportional-fonts
               '("Source Sans Pro" "DejaVu Sans" "Helvetica"))
         (setq dynamic-fonts-preferred-monospace-fonts
               '("Source Code Pro" "Inconsolata" "Monaco" "Consolas" "Menlo"
                 "DejaVu Sans Mono" "Droid Sans Mono Pro" "Droid Sans Mono")))
  :config
  (if initial-window-system
      (dynamic-fonts-setup)
    (add-to-list 'after-make-frame-functions
                 (lambda (frame) (dynamic-fonts-setup)))))

(use-package undo-tree
  :ensure t)

;; Spaceline - A mode line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme))


;(use-package powerline
;  :init
;  (powerline-default-theme))

(use-package elpy
  :ensure t
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

(use-package company
  :ensure t
  :diminish ""
  :commands global-company-mode
  :init
  (setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers t)
  :config
  (global-company-mode))


(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren nil
                show-paren-when-point-in-periphery t))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material 'no-confirm))

(use-package zenburn-theme
  :ensure t
  :disabled t
  :init
  (load-theme 'zenburn 'no-confirm))

(use-package solarized-theme
  :ensure t
  :disabled t
  :init
  (load-theme 'solarized-light 'no-confirm))

(use-package leuven-theme
  :ensure t
  :disabled t
  :init (load-theme 'leuven 'no-confirm))

(use-package faff-theme
  :ensure t
  :disabled t
  :init (load-theme 'faff 'no-confirm))

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-buffers-list)) 
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package image-file
  :init (auto-image-file-mode))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spaceline which-key use-package unicode-fonts spacemacs-theme python-mode powerline material-theme jedi helm-projectile elpy dynamic-fonts doom-themes company-statistics company-jedi))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
