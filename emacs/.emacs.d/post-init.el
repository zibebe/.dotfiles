;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Base Setup

;; Set the environment variables at first
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;; General emacs setup
(defun zibebe-recentf-mode ()
  "Enable recentf-mode silently without startup messages."
  (let ((inhibit-message t))
    (recentf-mode 1)))

(use-package emacs
  :ensure nil
  :demand t
  :bind
  ( :map global-map
    ("C-x C-c" . nil)
    ("C-x C-c C-c" . save-buffers-kill-emacs))
  :hook ((after-init . global-auto-revert-mode)
         (after-init . zibebe-recentf-mode)
         (kill-emacs . recentf-cleanup)
         (after-init . savehist-mode)
         (after-init . save-place-mode)
         (after-init . pixel-scroll-precision-mode)
         (after-init . delete-selection-mode)
         (after-init . which-key-mode)
         (after-init . electric-pair-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (setq trash-directory "~/.Trash")
  (setq delete-by-moving-to-trash t)
  (setq insert-directory-program "gls")
  (set-face-attribute 'default nil :font "Comic Code" :height 180)
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none)
  (setq custom-file (make-temp-file "emacs-custom-")))

;; Dired settings
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso"))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;;; Appearance

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

;; Increase the padding/spacing of Emacs frames and windows
(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode))

;;; Code completion

;; COmpletion in Region FUnction
(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :bind
  ( :map corfu-map
    ("C-SPC" . corfu-insert-separator))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode))

;; Cape provides Completion At Point Extensions
(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; Enhance completion and navigation capabilities

(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;;; LSP and Coding

;; Rust
(defun zibebe-rust-mode-settings ()
  "Recommended settings according to the Rust Style-Guide"
  (setq fill-column 100
        indent-tabs-mode nil))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . zibebe-rust-mode-settings)
  :config
  (setq rust-format-on-save t))

;; Golang
(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)))

;; Yaml
(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . display-line-numbers-mode))

;; Built-in LSP Client
(use-package eglot
  :ensure nil
  :hook ((rust-mode
          c-mode
          c++-mode
          go-mode)
         . eglot-ensure)
  :bind
  ( :map eglot-mode-map
    ("C-c c a" . eglot-code-actions)
    ("C-c c f" . eglot-format-buffer)
    ("C-c c h" . eldoc)
    ("C-c c r" . eglot-rename)
    ("C-c c s" . consult-eglot-symbols)))

;; Enhance consult capabilities with eglot workspace symbols
(use-package consult-eglot
  :ensure t
  :commands (consult-eglot-symbols))

;;; Version Control

;; Magit (Git interface)
(use-package magit
  :ensure t
  :commands (magit-status))

;; Git Gutters
(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

;;; DevOps stuff

;; K8s Management inside Emacs
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;;; Org Mode

;; Basic Setup
(use-package org
  :ensure nil
  :defer t
  :init
  (setq org-directory (expand-file-name "~/Documents/org/")))

