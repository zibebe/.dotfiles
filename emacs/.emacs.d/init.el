
;; User defined functions
(defun zibebe-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (pcase appearance
    ('light (modus-themes-load-theme 'modus-operandi))
    ('dark (modus-themes-load-theme 'modus-vivendi)))
  (if (eq window-system 'ns)
      (add-to-list 'default-frame-alist '(ns-appearance . dark))))

(setq use-package-compute-statistics t) ; only for the benches
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)
(setq kill-buffer-delete-auto-save-files t)
(setq custom-file (make-temp-file "emacs-custom-")) ; disable the the custom file by sending it to oblivion.

(if (eq window-system 'ns)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;;; Package Manager

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;;; General Settings

(use-package emacs
  :ensure nil
  :demand t
  :init
  (require-theme 'modus-themes)
  :bind
  ( :map global-map
    ("<f1>" . vterm)
    ("<f2>" . modus-themes-toggle)
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-x C-c C-c" . save-buffers-kill-emacs)) ; more cumbersome, less error-prone
  :hook ((after-init . global-auto-revert-mode)
         (after-init . recentf-mode)
         (after-init . savehist-mode)
         (after-init . save-place-mode)
         (after-init . pixel-scroll-precision-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (setq tab-always-indent 'complete)
  (setq-default indent-tabs-mode nil)
  (set-face-attribute 'default nil :font "SF Mono" :height 160)
  (set-face-attribute 'variable-pitch nil :font "SF Pro" :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold)
        modus-themes-headings
        '((0 . (variable-pitch light 1.25))
          (1 . (variable-pitch light 1.2))
          (2 . (variable-pitch regular 1.15))
          (3 . (variable-pitch regular 1.1))
          (4 . (variable-pitch regular 1.05))
          (agenda-date . (semilight 1.05))
          (agenda-structure . (variable-pitch light 1.25))
          (t . (variable-pitch 1.05))))
  (if (eq window-system 'ns)
      (add-hook 'ns-system-appearance-change-functions #'zibebe-apply-theme)
    (load-theme 'modus-vivendi)))

;; Delete selection by default
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Increase padding of windows/frames
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode))

;; Enable electric pair and quote mode
(use-package electric
  :ensure nil
  :config
  (electric-pair-mode))

;; Get the environment variables set by zsh
(use-package exec-path-from-shell
  :if (eq window-system 'ns)
  :ensure t
  :init
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;;; Completion

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("M-g M-g" . consult-goto-line)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s l" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator
        corfu-preview-current nil)
  (corfu-popupinfo-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Various

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package dired
  :ensure nil
  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls")))

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project")))

(use-package magit
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

(use-package vterm
  :ensure t
  :defer t)

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;; Languages

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

;; Format on save
;; apheleia has some issues with rust, as it needs a rustfmt.toml in the project root
;; to determine which rust edition is being used by the project
;; `cargo fmt' would read it from Cargo.toml but can not work on a single file
;; Maybe just delegate formatting of rust files to eglot and let the LSP do it...
;; See: https://github.com/radian-software/apheleia/issues/278
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c f" . consult-flymake)
              ("C-c c r" . eglot-rename)
              ("C-c c h" . eldoc)
              ("C-c c s" . consult-eglot-symbols))
  :hook ((rust-ts-mode
          c-ts-mode
          cpp-ts-mode
          go-ts-mode)
         . eglot-ensure)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :ensure t)

;; Ensure that all treesit grammars are installed

(defun zibebe-ensure-treesit-grammars-available ()
  (dolist (lang (mapcar 'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(setq treesit-language-source-alist '((rust "https://github.com/tree-sitter/tree-sitter-rust")
                                      (c "https://github.com/tree-sitter/tree-sitter-c")
                                      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                      (go "https://github.com/tree-sitter/tree-sitter-go")
                                      (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
                                      (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                      (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                      (json "https://github.com/tree-sitter/tree-sitter-json")))

(zibebe-ensure-treesit-grammars-available)

;; Expand auto-mode-list
(setq auto-mode-alist
      (append
       '(("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.rs\\'" . rust-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.toml\\'" . toml-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.json\\'" . json-ts-mode))
       auto-mode-alist))

;;; AI Stuff

(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5:14b-instruct-q4_K_M"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:14b-instruct-q4_K_M"
           :default-chat-non-standard-params '(("num_ctx" . 32768)))))

;;; Org-mode (personal information manager)
(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/Documents/org/"))
  :bind
  ( :map global-map
    ("C-c o l" . org-store-link)
    ("C-c o a" . org-agenda))
  :config
  (setq org-hide-emphasis-markers t))
