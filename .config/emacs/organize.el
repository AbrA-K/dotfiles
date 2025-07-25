(use-package all-the-icons)
(use-package org
  :ensure t
  :config
  ;; Ensure org-mode is loaded before setting org-format-latex-options
  (with-eval-after-load 'org
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))))

(use-package valign)
(add-hook 'org-mode-hook #'valign-mode)

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
        (plist-put org-format-latex-options :scale 1.2)
        (plist-put org-format-latex-options :foreground 'auto)
        (plist-put org-format-latex-options :background 'auto)))



(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-indent nil)
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table-horizontal 1.0))

(setq org-modern-replace-stars "󰉫󰉬󰉭󰉮󰉯")
(setq org-modern-fold-stars '(("󰉱" . "󰉫")
                              ("󰉱" . "󰉬")
                              ("󰉱" . "󰉭")
                              ("󰉱" . "󰉮")
                              ("󰉱" . "󰉯")
                              ("󰉱" . "󰉰")))
;; Increase line spacing
(setq-default line-spacing 3)
(setq truncate-lines t)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))


;; org-roam
(use-package org-roam)
(setq org-roam-directory (file-truename "~/SimsalA-org"))
(setq org-agenda-diary-file (file-truename "~/SimsalA-org/diary.org"))
(org-roam-db-autosync-mode)

;; no long filenamess
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

(setq org-directory "~/SimsalA-org")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

(use-package auctex)

;; make drag and drop cooler
(use-package org-download)
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-download-image-dir (concat org-roam-directory "/image-download"))
