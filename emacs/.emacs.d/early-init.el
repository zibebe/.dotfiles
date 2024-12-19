;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(defvar zibebe-emacs-var-dir (expand-file-name "var" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" zibebe-emacs-var-dir))
(setq user-emacs-directory zibebe-emacs-var-dir)

(defun zibebe/display-startup-time ()
  "Display startup time of emacs"
  (message "Emacs loaded in %s with %d garbage collections."
           (emacs-init-time "%.2f seconds")
           gcs-done))

;; Just for the benches
(add-hook 'emacs-startup-hook 'zibebe/display-startup-time)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.  NEVER tell a beginner to disable any of these.  They
;; are helpful.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar zibebe-emacs--file-name-handler-alist file-name-handler-alist)
(defvar zibebe-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist zibebe-emacs--file-name-handler-alist
                  vc-handled-backends zibebe-emacs--vc-handled-backends)))

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
