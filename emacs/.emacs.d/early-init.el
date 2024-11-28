;;; User defined functions

(defun zibebe/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (emacs-init-time "%.2f seconds")
           gcs-done))

(add-hook 'emacs-startup-hook 'zibebe/display-startup-time)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; Do not show any graphical elements by default
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Initializes the package cache
(setq package-enable-at-startup t)


