;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Settings

(use-package emacs
  :ensure nil
  :demand t
  :hook ((after-init . global-auto-revert-mode)
         (after-init . recentf-mode)
         (after-init . savehist-mode)
         (after-init . save-place-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 160)
  (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 160)
  (setq tab-always-indent 'complete)
  (setq-default indent-tabs-mode nil))

;;; Packages

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
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

(use-package magit
  :ensure t)

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project")))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

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
  (setq corfu-preview-current nil
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
  :hook (before-save . gofmt-before-save))

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
