;;; post-init.el --- Set-up various packages and settings -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Settings

;; Fonts
(set-face-attribute 'default nil :font "Fira Code Retina" :height 160)
(set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 160)

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

;; Show line-numbers only in prog-modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; General emacs settings
(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq-default indent-tabs-mode nil))

;;; Packages

;; Enable `electrict' pair mode
(use-package electric
  :ensure nil
  :config
  (electric-pair-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

(use-package dired
  :ensure nil
  :config
  ;; We need that to use gnu-ls on macOS
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls")))

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-nord t))

(use-package magit
  :ensure t)

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project")))

(use-package which-key
  :ensure nil ; built into Emacs 30
  :hook (after-init . which-key-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult-eglot
  :ensure t
  :bind
  ( :map global-map
    ("M-s M-s" . consult-eglot-symbols)))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ( :map global-map
    ("M-g M-f" . consult-flymake)
    ("M-g M-g" . consult-goto-line)
    ("M-g M-i" . consult-imenu)
    ("M-s M-b" . consult-buffer)
    ("M-s M-f" . consult-find)
    ("M-s M-g" . consult-grep)
    ("M-s M-l" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-preview-current nil
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

;;; LSP Setup

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-on-save t))

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)))

(use-package go-mode
  :ensure t
  :defer t
  :hook (before-save . eglot-format-buffer))

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c f" . eglot-format-buffer)
              ("C-c c r" . eglot-rename)
              ("C-c c h" . eldoc))
  :hook ((( rust-mode c-mode
            c++-mode go-mode)
          . eglot-ensure)))

(provide 'post-init)
;;; post-init.el ends here
