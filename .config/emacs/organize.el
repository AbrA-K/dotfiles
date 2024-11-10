(add-hook 'org-mode-hook #'valign-mode)
;; Load and configure org-mode
(use-package org
  :ensure t
  :config
  ;; Ensure org-mode is loaded before setting org-format-latex-options
  (with-eval-after-load 'org
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))))

;; olivetti
(use-package olivetti
  :hook (org-mode . olivetti-mode))

(setq org-indent-indentation-per-level 1)
(setq-default org-startup-indented t
              org-pretty-entities t
              org-modern-replace-stars "󰉫󰉬󰉭󰉮󰉯"
              org-use-sub-superscripts "{}"
              org-hide-emphasis-markers t
              org-startup-with-inline-images t
              org-modern-hide-stars nil
              org-image-actual-width '(400))


(use-package org-appear
  :hook
  (org-mode . org-appear-mode))


;; LaTeX previews
(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
        (plist-put org-format-latex-options :scale 3)
        (plist-put org-format-latex-options :foreground 'auto)
        (plist-put org-format-latex-options :background 'auto)))



(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-indent nil)
  (org-modern-keyword nil)
  (org-modern-checkbox nil))

(setq org-modern-replace-stars "󰉫󰉬󰉭󰉮󰉯")
(setq org-modern-fold-stars '(("󰉱" . "󰉫")
                              ("󰉱" . "󰉬")
                              ("󰉱" . "󰉭")
                              ("󰉱" . "󰉮")
                              ("󰉱" . "󰉯")
                              ("󰉱" . "󰉰")))
;; Increase line spacing
(setq-default line-spacing 3)

(custom-set-faces
 '(variable-pitch-mode ((t (:family "Arimo Nerd Font")))))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))
(setq org-babel-lisp-eval-fn #'sly-eval)

;; org-roam
(use-package org-roam)
(setq org-roam-directory (file-truename "~/SimsalA-org"))
(org-roam-db-autosync-mode)

;; no long filenamess
