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
  (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 160)
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default indent-tabs-mode nil))

;;; Get correct env variables on macOS

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;;; Visuals

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

;;; Various

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
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-define-global-key-bindings nil))

;;; Org Mode

(use-package calendar
  :ensure nil
  :init
  (setq diary-file (expand-file-name "~/Documents/org/diary"))
  :commands (calendar)
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-date-style 'iso)
  (setq calendar-time-zone-style 'numeric)
  (setq calendar-time-display-form
        '( 24-hours ":" minutes
           (when time-zone (format "(%s)" time-zone))))
  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0100")
  (setq calendar-daylight-time-zone-name "+0200"))

(use-package appt
  :ensure nil
  :commands (appt-activate)
  :config
  (with-eval-after-load 'org-agenda
    (appt-activate 1)
    (org-agenda-to-appt)))

(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/Documents/org/"))
  :bind ("C-c l" . org-store-link)
  :config
  (setq org-ellipsis " ↴")
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time))

(use-package org-agenda
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files `(,org-directory))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-include-diary t))

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
                    "%?")
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
                    "%?")
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

;;; Completion

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult-eglot
  :ensure t
  :bind (:map global-map
              ("M-s M-s" . consult-eglot-symbols)))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (:map global-map
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

;;; Languages

(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-indent-local-mode)
  :config
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  (electric-indent-mode -1))

(use-package text-mode
  :ensure nil
  :hook (text-mode . turn-on-auto-fill))

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
  :functions (eglot-ensure)
  :commands (eglot)
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c f" . eglot-format-buffer)
              ("C-c c r" . eglot-rename)
              ("C-c c h" . eldoc))
  :hook ((( rust-mode rust-ts-mode
            go-mode go-ts-mode
            c-mode c-ts-mode
            c++-mode c++-ts-mode)
          . eglot-ensure))
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshuqtdown t))

(provide 'post-init)

;;; post-init.el ends here
