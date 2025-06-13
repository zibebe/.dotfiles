;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; User defined functions

(defun zibebe-recentf-mode ()
  "Enable recentf-mode silently without startup messages."
  (let ((inhibit-message t))
    (recentf-mode 1)))

(defun zibebe-rust-mode-settings ()
  "Recommended settings according to the Rust Style-Guide"
  (setq fill-column 100
        indent-tabs-mode nil))

(defun zibebe-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(defun zibebe-ensure-treesit-grammars ()
  "Ensure that the tree-sitter languages are installed"
  (dolist (lang (mapcar 'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;;; Base Setup

;; Set the environment variables at first
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;; General emacs setup
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
         (before-save . delete-trailing-whitespace)
         (prog-mode . display-line-numbers-mode)
         (special-mode . visual-line-mode)
         (ns-system-appearance-change-functions . zibebe-apply-theme)
         (text-mode . variable-pitch-mode))
  :config
  (setq trash-directory "~/.Trash")
  (setq delete-by-moving-to-trash t)
  (setq insert-directory-program "gls")
  (setq Man-sed-command "gsed")
  (set-face-attribute 'default nil :font "Comic Code Ligatures-18:minspace=true")
  (set-face-attribute 'fixed-pitch nil :font (face-attribute 'default :font))
  (set-face-attribute 'variable-pitch nil :font "SF Pro Text-18")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none)
  (setq custom-file (make-temp-file "emacs-custom-")))

;; Dired settings
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso"))

;; Move content easily
(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;;; Appearance

;; Theme
(use-package modus-themes
  :ensure t
  :demand t
  :bind
  ( :map global-map
    ("<f5>" . modus-themes-toggle))
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)
  (setq modus-themes-common-palette-overrides
        '((comment red-faint))))

;; Increase the padding/spacing of Emacs frames and windows
(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :init
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line-active
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive)))

;; Pulse highlight line on demand or after running select functions
(use-package pulsar
  :ensure t
  :hook (after-init . pulsar-global-mode))

;;; Code completion

;; COmpletion in Region FUnction
(use-package corfu
  :ensure t
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
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; Enhance completion and navigation capabilities

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

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
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
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

;; Provides snippet mechanism
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

;; Collection of snippets
(use-package yasnippet-snippets
  :ensure t)

;; Tree-Sitter setup using built in treesit.el
(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((rust "https://github.com/tree-sitter/tree-sitter-rust")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (json "https://github.com/tree-sitter/tree-sitter-json")))
  (setq auto-mode-alist
        (append
         '(("\\.c\\'" . c-ts-mode)
           ("\\.h\\'" . c-ts-mode)
           ("\\.cpp\\'" . c++-ts-mode)
           ("\\.hpp\\'" . c++-ts-mode)
           ("\\.go\\'" . go-ts-mode)
           ("go.mod" . go-mod-ts-mode)
           ("\\.toml\\'" . toml-ts-mode)
           ("\\.ya?ml\\'" . yaml-ts-mode)
           ("\\.json\\'" . json-ts-mode))
         auto-mode-alist))
  :hook (after-init . zibebe-ensure-treesit-grammars))

;; Eglot (built in LSP Client)
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c f" . eglot-format)
              ("C-c c h" . eldoc)
              ("C-c c r" . eglot-rename)
              ("C-c c s" . consult-eglot-symbols)
              ("C-c c d" . flymake-show-project-diagnostics))
  :hook ((rust-ts-mode
          c-ts-mode
          cpp-ts-mode
          go-ts-mode)
         . eglot-ensure)
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; Enhance consult capabilities with eglot workspace symbols
(use-package consult-eglot
  :ensure t
  :commands (consult-eglot-symbols))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)))

;; Rust
(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t)
  :hook (rust-mode . zibebe-rust-mode-settings)
  :config
  (setq rust-format-on-save t))

;;; Version Control

;; Magit (Git interface)
(use-package magit
  :ensure t
  :commands (magit-status))

;; Git Gutters
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode))

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
