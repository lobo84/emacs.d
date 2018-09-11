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
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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

(use-package dynamic-fonts
  :ensure dynamic-fonts
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-point-size 12)
    (setq dynamic-fonts-preferred-proportional-point-size 12)
    (setq dynamic-fonts-preferred-monospace-fonts
          '("Source Code Pro" "Roboto Mono" "Ubuntu Mono"
            "Consolas" "Courier New" "Monospace"))
    (dynamic-fonts-setup)))

(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

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

(use-package magit
  :commands magit-get-top-dir
  :bind (("C-x g" . magit-status)))

;; run omnisharp-install-server
(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key omnisharp-mode-map (kbd "<C-.>") 'omnisharp-add-dot-and-auto-complete))
;;  (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete))
  ;;(define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete))
