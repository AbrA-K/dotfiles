(use-package meow)

;; better window navigation
(use-package ace-window)
;; (when (fboundp 'windmove-default-keybindings))
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)
(global-set-key (kbd "C-s-<left>")  'windmove-swap-states-left)
(global-set-key (kbd "C-s-<right>") 'windmove-swap-states-right)
(global-set-key (kbd "C-s-<up>")    'windmove-swap-states-up)
(global-set-key (kbd "C-s-<down>")  'windmove-swap-states-down)

;; add/remove parenthesies (how 2 spell?)
(use-package surround)
(define-prefix-command 'surround-keymap)
(define-key surround-keymap (kbd "s") #'surround-insert)
(define-key surround-keymap (kbd "d") #'surround-delete)

;; org roam command map
(define-prefix-command 'org-roam-command-map)

(define-key org-roam-command-map (kbd "f") #'org-roam-node-find)
(define-key org-roam-command-map (kbd "i") #'org-roam-node-insert)
(define-key org-roam-command-map (kbd "d") #'org-roam-dailies-capture-today)
(define-key org-roam-command-map (kbd "t") #'org-roam-buffer-toggle)
(define-key org-roam-command-map (kbd "g") #'org-roam-graph)
(define-key org-roam-command-map (kbd "c") #'org-roam-capture)
(define-key org-roam-command-map (kbd "b") #'org-roam-switch-to-buffer)

(global-set-key (kbd "C-c n") 'org-roam-command-map)

;; actual meow config
(setq aw-dispatch-always t)

(require 'eldoc-box)
(defun meow-setup ()
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-iso)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwertz)
  (setq meow-keypad-self-insert-undefined nil)

  (meow-thing-register 'angle
                       '(pair (";") (":"))
                       '(pair (";") (":")))

  (setq meow-char-thing-table
        '((?r . round)
          (?q . square)
          (?c . curly)
          (?a . angle)
          (?s . string)
          (?p . paragraph)
          (?l . line)
          (?b . buffer)))

  (meow-leader-define-key
   '("p" . projectile-command-map)
   '("w" . ace-window)
   '("e" . eglot)
   '("o" . org-roam-command-map)
   '("k" . eldoc-box-help-at-point)
   '("t" . toggle-term-vterm)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("-" . meow-keypad-describe-key)
   '("_" . meow-cheatsheet))

  (meow-normal-define-key
    ;; expansion
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)

    ;; movement
    '("g" . meow-prev)
    '("r" . meow-next)
    '("n" . meow-left)
    '("t" . meow-right)

    '("k" . meow-search)
    '("K" . meow-reverse)
    '("ß" . meow-replace)
    '("j" . meow-visit)
    '("J" . surround-keymap)

    ;; expansion
    '("G" . meow-prev-expand)
    '("R" . meow-next-expand)
    '("N" . meow-left-expand)
    '("T" . meow-right-expand)

    '("h" . meow-back-word)
    '("H" . meow-back-symbol)
    '("f" . meow-next-word)
    '("F" . meow-next-symbol)

    '("u" . meow-mark-word)
    '("U" . meow-mark-symbol)
    '("i" . meow-line)
    '("I" . meow-goto-line)
    '("v" . meow-block)
    '("x" . meow-join)
    '("o" . meow-grab)
    '("O" . meow-pop-grab)
    '("m" . meow-swap-grab)
    '("M" . meow-sync-grab)
    '("q" . meow-cancel-selection)
    '("Q" . meow-pop-selection)

    '("ö" . meow-till)
    '("ü" . meow-find)

    '("," . meow-beginning-of-thing)
    '("." . meow-end-of-thing)
    '("–" . meow-inner-of-thing)
    '("•" . meow-bounds-of-thing)

    ;; editing
    '("a" . meow-kill)
    '("e" . meow-change)
    '("w" . meow-delete)
    '("ä" . meow-save)
    '("p" . meow-yank)
    '("P" . meow-yank-pop)

    '("l" . meow-insert)
    '("L" . meow-open-above)
    '("c" . meow-append)
    '("C" . meow-open-below)

    '("s" . undo-only)
    '("S" . undo-redo)

    '("z" . open-line)
    '("Z" . split-line)

    '("<" . indent-rigidly-left-to-tab-stop)
    '(">" . indent-rigidly-right-to-tab-stop)

    ;; ignore escape
    '("<escape>" . ignore)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
(add-hook 'meow-insert-exit-hook 'corfu-quit)
