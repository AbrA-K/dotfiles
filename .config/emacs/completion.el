(use-package compat) ;; this fixes some version issues for me
;; default corfu config
(use-package corfu
  ;; Optional customizations
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-echo-documentation t)
  (corfu-echo-mode t)
  (corfu-preselect 'prompt)
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :bind
  (:map corfu-map ("C-<return>" . corfu-insert-separator))

  :init
  (global-corfu-mode))


(unless (package-installed-p 'eglot-booster)
  (package-vc-install "https://github.com/jdtsmith/eglot-booster"))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(use-package markdown-mode)

(use-package yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;; `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Option 1: Specify explicitly to use Orderless for Eglot
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))

(use-package nerd-icons-corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; A few more useful configurations...
(use-package emacs  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package eglot)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider))

(toggle-truncate-lines t)

;; better minibuffer compleation
(use-package vertico)
(use-package marginalia)
(vertico-mode)
(marginalia-mode)

;; parinfer and it's fixes
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode lisp-mode)
(setq parinfer-rust-disable-troublesome-modes t)
(setq parinfer-rust-check-before-enable 'defer)

;; zig mode
(use-package zig-mode)

;; ocaml
(use-package ocp-indent)
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))
;; To easily change opam switches within a given Emacs session, you can
;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
;; and use one of its "OPSW" menus.

(use-package tuareg)

;; --------------------------- DEBUGGING ---------------------------
(use-package dap-mode)
(dap-auto-configure-mode)

;; rust
(require 'dap-gdb-lldb)
(dap-gdb-lldb-setup)
(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))

;; ocaml
(require 'dap-ocaml)
(require 'dap-codelldb)
;; (dap-register-debug-template "OCaml Debug Template"
;;                              (list :type "ocaml.earlybird"
;;                                    :request "launch"
;;                                    :name "OCaml::Run"
;;                                    :program nil
;;                                    :target nil
;;                                    :arguments "debug"))
