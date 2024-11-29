;;; User defined functions

(defun zibebe-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the the custom file by sending it to oblivion.
(setq custom-file (make-temp-file "emacs-custom-"))

;;; Package Manager

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

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
    ("ESC ESC" . zibebe-simple-keyboard-quit-dwim)
    ("C-g" . zibebe-simple-keyboard-quit-dwim)
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-x C-c C-c" . save-buffers-kill-emacs) ; more cumbersome, less error-prone
    ("M-z" . zap-up-to-char)) ; NOT `zap-to-char'
  :hook ((after-init . global-auto-revert-mode)
         (after-init . recentf-mode)
         (after-init . savehist-mode)
         (after-init . save-place-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 160)
  (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (setq tab-always-indent 'complete)
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-headings
        '((agenda-date . (variable-pitch regular 1.05))
          (agenda-structure . (variable-pitch light 1.1))
          (t . (regular 1.05))))
  (setq user-full-name "Tobias Tschinkowitz")
  (setq user-mail-address "me@zibebe.net")
  (setq-default indent-tabs-mode nil))

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
  :if (memq window-system '(ns x))
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;; Date/Time Specific
(use-package calendar
  :ensure nil
  :commands (calendar)
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-date-style 'iso)
  (setq calendar-time-zone-style 'numeric)
  (setq calendar-time-display-form
        '( 24-hours ":" minutes
           (when time-zone (format "(%s)" time-zone))))
  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0100"
        calendar-daylight-time-zone-name "+0200"))

;; Autodark  - follows the system dark/light mode
(use-package auto-dark
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (setq auto-dark-allow-osascript t))
  (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-mode))

;;; File managing

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Mail

(use-package message
  :ensure nil
  :defer t
  :hook
  (message-setup . message-sort-headers)
  :config
  (setq mail-user-agent 'message-user-agent
        message-mail-user-agent t
        message-sendmail-envelope-from 'header
        message-signature nil
        message-kill-buffer-on-exit t))

(use-package sendmail
  :ensure nil
  :after message
  :config
  (setq send-mail-function 'sendmail-send-it
        sendmail-program (executable-find "msmtp")))

;; See https://github.com/djcb/mu/issues/2767
(defun mu4e-trash-by-moving-advice (args)
  "Makes `mu4e-mark-at-point' handle trash marks as moves to the trash folder."
  (cl-destructuring-bind (mark &optional target) args
    (if (eql mark 'trash)
        (list 'move (mu4e-get-trash-folder (mu4e-message-at-point)))
      args)))

(use-package mu4e
  :ensure nil
  :bind ("C-c m" . mu4e)
  :config
  (advice-add 'mu4e-mark-at-point :filter-args 'mu4e-trash-by-moving-advice)
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
        mu4e-update-interval 300
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent Messages"
        mu4e-trash-folder  "/Deleted Messages"
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                                 ("/Sent Messages" . ?s)
                                 ("/Archive" . ?a)
                                 ("/Deleted Messages" . ?d)
                                 ("/Junk" . ?j))))

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
  :hook (after-init . global-corfu-mode)
  :config
  (setq corfu-preview-current nil
        corfu-popupinfo-delay '(1.0 . 0.5))
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
  :ensure nil
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
  :ensure t)

(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

(use-package vterm
  :ensure t
  :defer t)

;;; Org Mode

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Documents/Org/"
        org-default-notes-file (file-name-concat org-directory "inbox.org")
        org-agenda-files `(,org-directory)
        org-startup-indented t
        org-capture-templates
        `(("t" "Task" entry
           (file+headline "" "Tasks")
           ,(concat "* TODO %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n\n"
                    "%?")
           :empty-lines 1)
          ("n" "Note" entry
           (file+headline "" "Notes")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n\n"
                    "%?")
           :empty-lines 1)))
  :bind
  (("C-c o l" . org-store-link)
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)))

;;; Languages

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package haskell-ts-mode
  :ensure t)

;; Format on save
;; apheleia has some issues with rust, as it needs a rustfmt.toml in the project root
;; to determine which rust edition is being used by the project
;; `cargo fmt' would read it from Cargo.toml but can not work on a single file
;; Maybe just delegate formatting of rust files to eglot and let the LSP do it...
;; See: https://github.com/radian-software/apheleia/issues/278
(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-mode-alist '(haskell-ts-mode . ormolu))
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
          go-ts-mode
          haskell-ts-mode)
         . eglot-ensure)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)
  (haskell-ts-setup-eglot))

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
                                      (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
                                      (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                      (json "https://github.com/tree-sitter/tree-sitter-json")
                                      (haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

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
         ("\\.lua\\'" . lua-ts-mode)
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
