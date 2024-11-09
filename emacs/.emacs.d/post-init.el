;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; General Settings

(use-package emacs
  :ensure nil
  :demand t
  :init
  (require-theme 'modus-themes)
  :bind
  ( :map global-map
    ("<f1>" . vterm)
    ("<f2>" . toggle-input-method)
    ("<f5>" . modus-themes-toggle)
    ("C-x C-d" . nil) ; never use list directory (brief)
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
  (setq tab-first-completion 'word-or-paren-or-punct)
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

;;; Delete selection by default
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;; Binding to remove trailing whitespaces in the active buffer
(use-package whitespace
  :ensure nil
  :bind (("<f3>" . whitespace-mode)
         ("C-c z" . delete-trailing-whitespace)))

;;; Increase padding of windows/frames
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f4>" . spacious-padding-mode))

;;; Disable electric indent mode in org-mode
;;; Enable electric-pair and quote modes
(use-package electric
  :ensure nil
  :hook (org-mode . (lambda () (electric-indent-local-mode -1)))
  :config
  (electric-pair-mode)
  (electric-quote-mode))

;;; Get the environment variables set by zsh

(use-package exec-path-from-shell
  :if (memq window-system '(ns x))
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;;; easypg Assistant

(use-package epa
  :ensure nil
  :config
  (setq epg-pinentry-mode 'loopback))

;;; Auth Source
(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

;;; Date/Time Specific

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

;;; Autodark  - follows the system dark/light mode

(use-package auto-dark
  :if (memq window-system '(ns x))
  :ensure t
  :init
  (auto-dark-mode)
  :config
  (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (when (memq window-system '(ns))
    (setq auto-dark-allow-osascript t)))

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
        message-signature nil))

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
        mu4e-change-filenames-when-moving t
        mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                                 ("/Sent Messages" . ?s)
                                 ("/Archive" . ?a)
                                 ("/Deleted Messages" . ?d)
                                 ("/Junk" . ?j))))

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
  :ensure t)

;;; Org Mode

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Documents/org/")
  (setq org-archive-location "~/Documents/org/archive.org::* Source: %s")
  :bind ("C-c o l" . org-store-link)
  :config
  (setq org-ellipsis " ▾")
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 1))
          (nil . (:maxlevel . 1))))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-startup-indented t))

(use-package org-capture
  :ensure nil
  :bind ("C-c o c" . org-capture)
  :config
  (setq org-capture-templates
        `(("u" "Unprocessed" entry
           (file+headline "tasks.org" "Unprocessed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)
          ("w" "Add to the wishlist (may do some day)" entry
           (file+headline "tasks.org" "Wishlist")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%?")
           :empty-lines-after 1)
          ("c" "Clock in and do immediately" entry
           (file+headline "tasks.org" "Clocked tasks")
           ,(concat "* TODO %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                    ":END:\n\n"
                    "%a\n")
           :prepend t
           :clock-in t
           :clock-keep t
           :immediate-finish t
           :empty-lines-after 1)
          ("t" "Time-sensitive task" entry
           (file+headline "tasks.org" "Tasks with a date")
           ,(concat "* TODO %^{Title}\n"
                    "%^{How time sensitive it is|SCHEDULED|DEADLINE}: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%?")
           :empty-lines-after 1))))

(defun zibebe-org-agenda-include-priority-no-timestamp ()
  "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should
continue, per `org-agenda-skip-function'."
  (let ((point (point)))
    (if (and (eq (nth 3 (org-heading-components)) ?A)
             (not (org-get-deadline-time point))
             (not (org-get-scheduled-time point)))
        nil
      (line-beginning-position 2))))

(use-package org-agenda
  :ensure nil
  :bind (("C-c o A" . org-agenda)
         ("C-c o a" . (lambda ()
                        (interactive)
                        (org-agenda nil "A"))))
  :config
  (setq org-default-notes-file (make-temp-file "emacs-org-notes-"))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-files `(,org-directory))
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ((tags-todo "*"
                       ((org-agenda-overriding-header "Important tasks without a date\n")
                        (org-agenda-skip-function #'zibebe-org-agenda-include-priority-no-timestamp)
                        (org-agenda-block-separator nil)))
            (agenda "" ((org-agenda-overriding-header "\nPending scheduled tasks")
                        (org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-span 1)
                        (org-agenda-show-all-dates nil)
                        (org-scheduled-past-days 365)
                        (org-scheduled-delay-days 1)
                        (org-agenda-block-separator nil)
                        (org-agenda-entry-types '(:scheduled))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-date "")))
            (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                        (org-agenda-span 1)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-scheduled-past-days 0)
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-date "%A %-e %B %Y")))
            (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 3)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
                        (org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-entry-types '(:deadline))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))))))
  (setq diary-file (make-temp-file "emacs-diary-"))
  (setq org-agenda-diary-file 'diary-file)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          ( 0500 0600 0700 0800 0900 1000
            1100 1200 1300 1400 1500 1600
            1700 1800 1900 2000 2100 2200)
          "" "")))

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
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Denote

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
         ("C-c n N" . denote-type)
         ("C-c n o" . denote-sort-dired)
         ("C-c n r" . denote-rename-file)
         ("C-c n i" . denote-link)
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks))
  :config
  (setq denote-directory "~/Documents/notes/")
  (setq denote-known-keywords '("gaming"
                                "coding"
                                "music"
                                "movies")))

(use-package consult-denote
  :ensure t
  :bind (("C-c n f" . consult-denote-find)
         ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;;; Languages

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

(use-package flyspell
  :ensure nil
  :bind
  ( :map ctl-x-x-map
    ("s" . flyspell-mode)
    :map flyspell-mouse-map
    ("<mouse-3>" . flyspell-correct-word))
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-dictionary "en_US"))

(use-package markdown-ts-mode
  :ensure t)

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

(use-package consult-eglot
  :ensure t)

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
                                      (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
                                      (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
                                      (yaml "https://github.com/ikatyang/tree-sitter-yaml")
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
         ("\\.md\\'" . markdown-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.json\\'" . json-ts-mode))
       auto-mode-alist))

(provide 'post-init)

;;; post-init.el ends here
