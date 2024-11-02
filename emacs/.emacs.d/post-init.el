;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; General Settings

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
  (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default indent-tabs-mode nil))

;;; Get the environment variables set by zsh

(use-package exec-path-from-shell
  :if (memq window-system '(ns x))
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

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

(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

;;; Theme

(use-package ef-themes
  :ensure t
  :demand t
  :bind ("C-c t" . ef-themes-toggle)
  :config
  (setq ef-themes-to-toggle '(ef-dark ef-light)
        ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-headings
        '((0 . (variable-pitch light 1.3))
          (1 . (variable-pitch light 1.2))
          (2 . (variable-pitch regular 1.1))
          (3 . (variable-pitch regular 1.05))
          (4 . (variable-pitch regular 1.0))
          (5 . (variable-pitch 1.0))
          (6 . (variable-pitch 1.0))
          (7 . (variable-pitch 1.0))
          (agenda-date . (semilight 1.0))
          (agenda-structure . (variable-pitch light 1.3))
          (t . (variable-pitch 1.1)))))

;;; Icons

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; Autodark  - follows the system dark/light mode

(use-package auto-dark
  :if (memq window-system '(ns x))
  :ensure t
  :init
  (auto-dark-mode)
  :config
  (setq auto-dark-themes '((ef-dark) (ef-light)))
  (when (memq window-system '(ns))
    (setq auto-dark-allow-osascript t)))

;;; Mode-Line

(use-package emacs
  :ensure nil
  :config
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  (:propertize
                   (""
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    mode-line-window-dedicated)
                   display (min-width (6.0)))
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (project-mode-line project-mode-line-format)
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-format-right-align
                  mode-line-misc-info
                  mode-line-end-spaces)))

;;; Various

(use-package spacious-padding
  :ensure t
  :demand t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode))

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1))
  

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
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

;;; Org Mode

(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/Documents/org/"))
  :bind ("C-c l" . org-store-link)
  :config
  (setq org-ellipsis " ▾")
  (setq org-src-window-setup 'current-window)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 1))
          (nil . (:maxlevel . 1))))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-use-fast-todo-selection 'expert))

(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
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
  :bind (("C-c A" . org-agenda)
         ("C-c a" . (lambda ()
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

;;; Languages

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

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

(use-package consult-eglot
  :ensure t)

(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :bind (:map eglot-mode-map
              ("C-c C-c a" . eglot-code-actions)
              ("C-c C-c f" . eglot-format-buffer)
              ("C-c C-c e" . consult-flymake)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c h" . eldoc)
              ("C-c C-c s" . consult-eglot-symbols))
  :hook ((( rust-mode rust-ts-mode
            go-mode go-ts-mode
            c-mode c-ts-mode
            c++-mode c++-ts-mode)
          . eglot-ensure))
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))



(provide 'post-init)

;;; post-init.el ends here
