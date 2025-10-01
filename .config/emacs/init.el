(load-file "~/.config/emacs/package.el")
(load-file "~/.config/emacs/organize.el")
(load-file "~/.config/emacs/completion.el")
(load-file "~/.config/emacs/meow.el")
(add-to-list 'load-path "~/.config/emacs/emacs-progmode")

;; sensible defaults
(load-file "~/.config/emacs/sensible-defaults.el")
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)

;; stop annoying defaults
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)
(setq visible-bell t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq next-line-add-newlines t)
(scroll-bar-mode -1)
(setq use-dialog-box nil)


;; change emacs autosaves to something less annoying
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "~/.emacs-saves"))))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; theme
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(use-package ef-themes)
(load-theme 'ef-cyprus)

;; smoothscroll
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 40.0)

;; line numbers
(add-hook 'prog-mode-hook (lambda ()
                            (scroll-bar-mode -1)
                            (display-line-numbers-mode)))

;; nice bufferline
(unless (package-installed-p 'lambda-line)
  (package-vc-install "https://github.com/Lambda-Emacs/lambda-line"))
(use-package lambda-line
  :custom
  (lambda-line-position 'bottom)
  (lambda-line-abbrev t)
  (lambda-line-hspace " ")
  (lambda-line-prefix t)
  (lambda-line-prefix-padding nil)
  (lambda-line-status-invert nil)
  (lambda-line-display-group-start "[")
  (lambda-line-display-group-end "]")
  (lambda-line-gui-ro-symbol  " 󰙗")
  (lambda-line-gui-mod-symbol " 󰏩")
  (lambda-line-gui-rw-symbol  " 󰏩")
  (lambda-line-vc-symbol "  ")
  (lambda-line-space-top +.3)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.3))
(lambda-line-mode)

;; (use-package doom-modeline
;;   :config
;;   :init (doom-modeline-mode 1))

;; better font
(set-frame-font "FantasqueSansM Nerd Font 14" nil t)
(setq default-frame-alist '((font . "FantasqueSansM Nerd Font 14")))
(set-face-attribute 'mode-line nil :weight 'bold)
(setq flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))
;; (set-face-attribute 'lambda-line-display-group-end nil :weight 'regular)

;; rainbow delimiters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package solaire-mode)
(solaire-global-mode +1)

;; give emacs even MORE ram
(setq gc-cons-threshold 200000000)
(setq read-process-output-max (* 3 (* 1024 1024))) ;; 3mb
(setq lsp-idle-delay 0.500)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch-mode ((t (:family "Adwaita Sans")))))

(use-package magit)

(use-package vterm)
(use-package toggle-term
  :bind (("M-o f" . toggle-term-find)
         ("M-o t" . toggle-term-term)
         ("M-o v" . toggle-term-vterm)
         ("M-o a" . toggle-term-eat)
         ("M-o s" . toggle-term-shell)
         ("M-o e" . toggle-term-eshell)
         ("M-o i" . toggle-term-ielm)
         ("M-o o" . toggle-term-toggle))
  :config
    (setq toggle-term-size 40)
    (setq toggle-term-switch-upon-toggle t))
(custom-set-variables)

;; set special buffers here
(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-function
      (lambda (buffer &optional args)
        (tab-switch (buffer-name buffer))
        (switch-to-buffer buffer)))
