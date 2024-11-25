(load-file "~/.config/emacs/package.el")
(load-file "~/.config/emacs/organize.el")
(load-file "~/.config/emacs/completion.el")
(load-file "~/.config/emacs/meow.el")
(load-file "~/.config/emacs/ocaml.el")
(add-to-list 'load-path "~/.config/emacs/emacs-progmode")

;; Doom Flatwhite Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy nil)
 '(backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path '(nil "src"))
 '(custom-safe-themes
   '("00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729" "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "87b82caf3ade09282779733fb6de999d683caf4a67a1abbee8b8c8018a8d9a6b" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873" default))
 '(electric-indent-mode nil)
 '(indent-tabs-mode nil)
 '(line-move-visual t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   '(xhair vline hl-line+ v-term libvterm vterm toggle-term magit f macsql-sqlite emacsql-sqlite org-roam surround rainbow-delimiters tuareg-mode tuareg eglot-booster ocp-indent centaur-tabs simple-modeline eldoc-box lsp-ui ob-rust haskell-mode haskell-snippets nushell-ts-mode nushell-mode ace-window framemove projectile yasnippet nerd-icons-corfu orderless adwaita-dark-theme solaire-mode solaire neotree org-modern org-fragtog meson-mode vala-mode sly scheme-complete markdown-mode lsp-scheme vertico marginalia marginalia-mode vertigo olivetti spacious-padding god-mode all-the-icons doom-themes rust-mode ef-themes meow-tree-sitter))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(visible-bell t)
 '(warning-suppress-log-types '((comp))))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

;; (load-theme 'doom-monokai-classic)
(use-package ef-themes)
(load-theme 'ef-summer)

;; smoothscroll
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 40.0)

;; visual bell
(setq visible-bell t)

;; no bars
(menu-bar-mode 0)
(tool-bar-mode 0)
;;(setq default-frame-alist '((undecorated . t)))

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;change emacs autosaves to something less anoying
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

;; line numbers
(add-hook 'prog-mode-hook (lambda ()
                           (setq display-line-numbers-type 'relative)
                           (display-line-numbers-mode)))

;; treesitter
(require 'treesit)
(add-to-list
 'treesit-language-source-alist
 '(bash "https://github.com/tree-sitter/tree-sitter-bash.git"))

(add-to-list
 'treesit-language-source-alist
 '(rust "https://github.com/tree-sitter/tree-sitter-rust.git"))

(add-to-list
 'treesit-language-source-alist
 '(zig "https://github.com/maxxnino/tree-sitter-zig.git"))

(add-to-list
 'treesit-language-source-alist
 '(elisp "https://github.com/Wilfred/tree-sitter-elisp.git"))

(add-to-list
 'treesit-language-source-alist
 '(commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"))

(add-to-list
 'treesit-language-source-alist
   '((ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")))

(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)))

(use-package sly)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq default-frame-alist '((undecorated . t)))

;; spacious padding
;; (use-package spacious-padding
;;  :ensure t
;;  :hook (after-init . spacious-padding-mode))
;; (require 'spacious-padding)

(when (display-graphic-p)
   (tool-bar-mode -1)
   (scroll-bar-mode -1))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

;; better font
(set-frame-font "BlexMonoNerd Font 13" nil t)

;; rainbow delimiters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package neotree)
(setq neo-theme (if (display-graphic-p) 'icons))

(use-package solaire-mode)
(solaire-global-mode +1)

;; Project managament
(use-package projectile)
(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "<C-p>") 'projectile-command-map)


;; user mappings
;; emacs
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c d") 'flymake-show-project-diagnostics)
(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)


;; Buffer bar
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-gray-out-icons 'buffer)

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "*Messages*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

;; Performance zone

;; Set it to big number(100mb) like most of the popular starter kits like Spacemacs/Doom/Prelude, etc do:
(setq gc-cons-threshold 200000000)
;; Follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: "My suggestion is to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch. If even a 100-fold increase doesn't help, there's some deeper problem with the Lisp code which produces so much garbage, or maybe GC is not the reason for slowdown." Source: <https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/>
(setq read-process-output-max (* 3 (* 1024 1024))) ;; 3mb
(setq lsp-idle-delay 0.500)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch-mode ((t (:family "Arimo Nerd Font")))))

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
