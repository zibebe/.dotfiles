;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Base Setup

;; Set the environment variables at first
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package save-place-mode
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))
    
;;; Appearance

;; Set Font
(set-face-attribute 'default nil
                    :height 180 :weight 'normal :family "Comic Code Ligatures")

;; No Titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;; Always start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Theme
(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord :no-confirm))

;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;;; Comfy Helpers

;; Which-Key helps finding the right keys
(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))
