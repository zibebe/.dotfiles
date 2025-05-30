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
  :hook ((after-init . (lambda()
                         (let ((inhibit-message t))
                           (recentf-mode 1))))
         (kill-emacs . recentf-cleanup)))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package save-place-mode
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Used for umlauts
(setq mac-right-option-modifier 'none)
    
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

;;; Code completion with corfu

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
