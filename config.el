;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Cédric Da Fonseca"
      user-mail-address "captain.spof@gmail.com")

(setq gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024)) ;; 1mb

(use-package! gcmh
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024))  ; 100mb
  (gcmh-mode 1))

(defvar daf/localleader-key "SPC ç"
  "The localleader prefix key, for major-mode specific commands.")

(setq-default vterm-shell (executable-find "fish"))

(setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "󰇴-\\1"))
   '(("" . "\\`+?evilem--?motion-\\(.*\\)")     . (nil . "󰱯-\\1"))))

(setq +workspaces-switch-project-function #'dired)

(setq bookmark-default-file (expand-file-name "local/bookmarks" doom-user-dir)
      projectile-known-projects-file (expand-file-name "local/projectile.projects" doom-user-dir))

(setq-default scroll-margin 3)

(pixel-scroll-precision-mode 1)

(defun filter-mwheel-always-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
   coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (apply orig args)
    (setq mwheel-coalesce-scroll-events t)))

(defun filter-mwheel-never-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
   non-coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (setq mwheel-coalesce-scroll-events nil)
    (apply orig args)))

                                        ; Don't coalesce for high precision scrolling
(advice-add 'pixel-scroll-precision :around #'filter-mwheel-never-coalesce)

                                        ; Coalesce for default scrolling (which is still used for horizontal scrolling)
                                        ; and text scaling (bound to ctrl + mouse wheel by default).
;; (advice-add 'mwheel-scroll          :around #'filter-mwheel-always-coalesce)
;; (advice-add 'mouse-wheel-text-scale :around #'filter-mwheel-always-coalesce)

(setq mouse-wheel-tilt-scroll t)

(after! tramp
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

;; (after! tramp (advice-add 'doom--recentf-file-truename-fn :override
;; (defun my-recent-truename (file &rest _args)
;; (if (or (not (file-remote-p file)) (equal "sudo" (file-remote-p file 'method)))
;; (abbreviate-file-name (file-truename (tramp-file-local-name file)))
;; file))))

(setq calendar-week-start-day     1
      calendar-time-zone-style    'numeric
      calendar-date-style         'european
      calendar-time-display-form
      '( 24-hours ":" minutes
         (when time-zone (format "(%s)" time-zone))))

;; FIXME
(map! :after (evil evil-org org)
      :map org-agenda-mode-map

      [return] #'org-agenda-goto
      [S-return] #'org-agenda-switch-to)

(setq +zen-text-scale 0)

(setq default-major-mode 'text-mode)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(when (display-graphic-p)
  (toggle-frame-maximized))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(map! :map minibuffer-local-map doom-leader-alt-key #'doom/leader)

(setq eros-eval-result-prefix "⇒ ") ; default =>

;; (setq tab-width 4)

(map!
 :leader
 :nv "\"" #'+popup/toggle)

(setq +doom-quit-messages
      (delete (seq-find (lambda (elmt) (string-match-p "baka" elmt)) +doom-quit-messages) +doom-quit-messages))

(map!
 :map 'override
 :v "v" #'er/expand-region
 :v "V" #'er/contract-region)

;;;###autoload
(defmacro daf/repeat-map! (map-name keys-alist &optional docstring)
  "A helper macro to create keymaps for repeatable actions.

MAP-NAME is the variable name for the sparse keymap created, and KEYS-ALIST, is
an association list of functions to keys, where each function is called after
the associated key is pressed after the repeatable action is triggered."
  `(defvar ,map-name
     (let ((map (make-sparse-keymap)))
       (dolist (cmd ,keys-alist)
         (define-key map (cdr cmd) (car cmd))
         (put (car cmd) 'repeat-map ',map-name))
       map)
     ,docstring))

(add-hook 'after-init-hook 'repeat-mode)

(daf/repeat-map! daf-window-resize-repeat-map
                 '((+evil-window-increase-height-by-three . "+")
                   (+evil-window-increase-height-by-three . "=")
                   (+evil-window-decrease-height-by-three . "-")
                   (+evil-window-increase-width-by-five   . "»")
                   (+evil-window-increase-width-by-five   . ">")
                   (+evil-window-decrease-width-by-five   . "«")
                   (+evil-window-decrease-width-by-five   . "<"))
                 "Repeatable map for window resizing")

;;;###autoload
(defun daf/window-toggle-lock-size ()
  "Lock/unlock the current window size."
  (interactive)
  (let ((window (get-buffer-window)))
    (cond ((or (window-size-fixed-p window)
               (window-size-fixed-p window t))
           (daf/window-unlock-size window))
          (t
           (daf/window-lock-size window)))))

;;;###autoload
(defun daf/window-lock-size (&optional window)
  "Lock the current window size."
  (interactive)
  (let ((window (or window (get-buffer-window))))
    (message "locking current window size")
    (window-preserve-size window t t)))

;;;###autoload
(defun daf/window-unlock-size (&optional window)
  "Unlock the current window size."
  (interactive)
  (let ((window (or window (get-buffer-window))))
    (message "unlocking current window size")
    (window-preserve-size window t nil)))

;;;###autoload
(defun daf/window-shrink-and-lock ()
  "Shrink and lock the current window size."
  (interactive)
  (let* ((window  (get-buffer-window))
         (curr-h  (window-height window))
         (curr-w  (window-width window))
         (delta-h    (- 5 curr-h))
         (delta-w    (- 5 curr-w)))
    (save-excursion
      (save-selected-window (select-window window)
                            (enlarge-window delta-w delta-h)
                            (daf/window-lock-size window)))))

(map! :leader
      (:prefix "w"
       :desc "daf/toggle-lock" "," #'daf/window-toggle-lock-size
       :desc "daf/shrink"      "." #'daf/window-shrink-and-lock))

(defvar daf/dark-theme  'doom-gruvbox)
(defvar daf/light-theme 'ef-eagle)

(setq everforest-hard-dark-cyan    "#83c092"
      everforest-hard-dark-blue    "#7fbbb3"
      everforest-hard-dark-purple  "#d699b6"
      everforest-hard-dark-green   "#a7c080"
      everforest-hard-dark-red     "#e67e80"
      everforest-hard-dark-orange  "#e69875"
      everforest-hard-dark-yellow  "#ddbc7f"
      everforest-hard-dark-gray    "#323c41"
      everforest-hard-dark-silver  "#9da9a0"
      everforest-hard-dark-black   "#2b3339")

(custom-theme-set-faces! 'doom-gruvbox
  `(org-level-1     :foreground ,everforest-hard-dark-green)
  `(org-level-2     :foreground ,everforest-hard-dark-red)
  `(org-level-3     :foreground ,everforest-hard-dark-purple)
  `(org-level-4     :foreground ,everforest-hard-dark-orange)
  `(org-level-5     :foreground ,everforest-hard-dark-blue)
  `(org-level-6     :foreground ,everforest-hard-dark-silver)
  `(org-level-7     :foreground ,everforest-hard-dark-cyan)
  `(org-level-8     :foreground ,everforest-hard-dark-yellow)
  `(hl-line         :background ,(doom-lighten (doom-color 'base3) 0.10)))

;; (nano-theme-set-light-tinted)
;; (customize-set-variable 'modus-themes-common-palette-overrides
                        ;; `(
                          ;; ;; Make the mode-line borderless
                          ;; ;; (bg-mode-line-active       ,nano-color-subtle)
                          ;; ;; (fg-mode-line-active       fg-main)
                          ;; ;; (bg-mode-line-inactive     ,nano-color-highlight)
                          ;; ;; (fg-mode-line-active       fg-dim)
                          ;; ;; (bg-search-lazy            ,nano-color-popout)
                          ;; ;; (border-mode-line-active   ,nano-color-subtle)
                          ;; ;; (border-mode-line-inactive bg-main)
                          ;; (doom-modeline-panel :background)
                          ;; ))

;; (custom-theme-set-faces! 'modus-operandi-tinted
  ;; `(pulsar-magenta :background ,nano-color-popout)
  ;; `(doom-modeline-bar :background ,nano-color-foreground)
  ;; `(doom-modeline-panel :background ,nano-color-popout :foreground ,nano-color-foreground)
  ;; `(org-modern-tag :background ,nano-color-faded)
  ;; `(eros-result-overlay-face :background ,nano-color-faded))

(defun daf/toggle-themes ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) daf/dark-theme)
      (progn
        (disable-theme daf/dark-theme)
        (load-theme daf/light-theme t))
    (progn
      (disable-theme daf/light-theme)
      (load-theme daf/dark-theme t))))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Theme" :mvn "t" #'daf/toggle-themes))

(setq doom-font                (font-spec :family "Maple Mono NF" :size 12.0)
      doom-variable-pitch-font (font-spec :family "Maple Mono NF")
      doom-symbol-font         (font-spec :family "JuliaMono")
      doom-emoji-font          (font-spec :family "Twitter Color Emoji"))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-heigth t))

(setq fancy-splash-images
      (list
       (expand-file-name "misc/splash-images/ferris.svg" doom-user-dir)
       (expand-file-name "misc/splash-images/angrytrex.png" doom-user-dir)
       (expand-file-name "misc/splash-images/happystego.png" doom-user-dir)))

(setq fancy-splash-image (nth (random (length fancy-splash-images)) fancy-splash-images))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file"            :ne "f" #'find-file
        :desc "Recent files"         :ne "r" #'consult-recent-file
        :desc "Restore last session" :ne "R" #'doom/restart-and-restore
        :desc "Doom config dir"      :ne "C" #'doom/open-private-config
        :desc "Open config.org"      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
        :desc "Open dotfile"         :ne "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)"         :ne "n" #'org-roam-node-find
        :desc "Switch buffer"        :ne "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ne "B" #'consult-buffer
        :desc "IBuffer"              :ne "i" #'ibuffer
        :desc "Projects"             :ne "p" #'projectile-switch-project
        :desc "Set theme"            :ne "t" #'consult-theme
        :desc "GTD engage"           :ne "z" #'org-gtd-engage
        :desc "Quit"                 :ne "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings"     :ne "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))

(map! :leader :desc "Dashboard" "D" #'+doom-dashboard/open)

(advice-add #'doom-modeline-segment--modals :override #'ignore)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(setq doom-modeline-position-column-line-format '(""))

(map!
 :leader (:prefix ("t" . "toggle")
          :desc "Doom Modeline" "m" #'hide-mode-line-mode))

(global-hide-mode-line-mode 1)

;;;###autoload
(defun daf/turn-on-mode-line ()
  (if hide-mode-line-mode
      (hide-mode-line-mode -1)))

(add-hook 'lsp-after-open-hook 'daf/turn-on-mode-line)

(map! :leader
      (:prefix ("TAB" . "workspace")
       :desc "Switch workspace" :mvn "TAB" #'+workspace/switch-to
       :mvn "»" #'+workspace/switch-right
       :mvn "«" #'+workspace/switch-left
       :desc "Display tab bar"  :mvn "."   #'+workspace/display))

(map! :n "C-t" nil
      :n "g»" #'+workspace/switch-right
      :n "g«" #'+workspace/switch-left)

(map!
 :leader
 :prefix ("t" . "toggle")
 "SPC" #'daf/show-whitespaces)

(defun daf/show-whitespaces ()
  (interactive)
  (global-whitespace-mode 'toggle)
  (revert-buffer))

(map!
 :prefix "g"
 :n ")" #'+vc-gutter/next-hunk
 :n "(" #'+vc-gutter/previous-hunk)

(daf/repeat-map! daf-navigate-hunk-repeat-map
                 '((+vc-gutter/next-hunk     . ")")
                   (+vc-gutter/previous-hunk . "("))
                 "Repeatable map for navigating hunks")

(setq consult-narrow-key "«")

(use-package! embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package! consult
  :when (modulep! :completion vertico)
  :defer t
  :init
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any))
  (setq consult-preview-key
        '(:debounce 0.4 any))
  (add-to-list 'consult-preview-allowed-hooks
               'global-org-modern-mode-check-buffers)
  (add-to-list 'consult-preview-allowed-hooks
               'global-hl-todo-mode-check-buffers)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark
   :preview-key 'any)
  (when (modulep! :config default)
    (consult-customize
     +default/search-project +default/search-other-project
     +default/search-project-for-symbol-at-point
     +default/search-cwd +default/search-other-cwd
     +default/search-notes-for-symbol-at-point
     +default/search-emacsd
     :preview-key 'any))
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window))

(defun noct-consult-line-evil-history (&rest _)
  "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
  (when (and (bound-and-true-p evil-mode)
             (eq evil-search-module 'evil-search))
    (let ((pattern (car (orderless-pattern-compiler (car consult--line-history)))))
      (add-to-history 'evil-ex-search-history pattern)
      (setq evil-ex-search-pattern (list pattern t t))
      (setq evil-ex-search-direction 'forward)
      (when evil-ex-search-persistent-highlight
        (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

(advice-add #'consult-line :after #'noct-consult-line-evil-history)

(map!
 :leader
 (:prefix ("s" . "search")
  :desc "Search .emacs.d"       "E" #'+default/search-emacsd
  :desc "Jump to errors"        "e" #'consult-flymake
  :desc "Jump to global marks"  "R" #'consult-global-mark
  :desc "Search macros"         "q" #'consult-kmacro)
 (:prefix ("h" . "help")
  :desc "(Wo)Man" "W" #'consult-man))

(map!
 :map org-mode-map
 :leader
 (:prefix ("s" . "search")
  :desc "Jump to org headings" "." #'consult-org-heading))

(map! [remap describe-bindings] #'embark-bindings
      "C-," #'embark-act
      "M-," #'embark-dwim
      "C-?" #'embark-bindings)

(after! embark
  (eval-when-compile
    (defmacro daf/embark-ace-action (fn)
      `(defun ,(intern (concat "daf/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (daf/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (daf/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (daf/embark-ace-action bookmark-jump)))

(after! vertico
  (vertico-multiform-mode)

  (setq vertico-mouse-mode             't
        vertico-multiform-commands     '((consult-line buffer))
        vertico-multiform-categories   '((consult-grep buffer))
        vertico-buffer-display-action  '(display-buffer-in-side-window
                                          (side . left)
                                          (window-width . 0.4))))

(autoload #'consult--read "consult")

;;;###autoload
(defun +vertico/projectile-completion-fn (prompt choices)
  "Given a PROMPT and a list of CHOICES, filter a list of files for
`projectile-find-file'."
  (interactive)
  (consult--read
   choices
   :prompt prompt
   :sort nil
   :add-history (thing-at-point 'filename)
   :category 'file
   :history '(:input +vertico/find-file-in--history)))

(setq projectile-completion-system '+vertico/projectile-completion-fn)

(defvar-local consult-toggle-preview-orig nil)

(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function   consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function   #'ignore)))

;; Bind to `vertico-map' or `selectrum-minibuffer-map'
(after! vertico
  (define-key vertico-map (kbd "M-o c") #'consult-toggle-preview))

(map!
 :map vertico-map
 "C-n" #'vertico-next-group
 "C-p" #'vertico-previous-group)

(after! evil
  (setq evil-disable-insert-state-bindings t
        evil-want-fine-undo 'yes))

(after! evil
  (setq evil-split-window-below  t
        evil-vsplit-window-right t))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (dired-jump))

(after! evil
  (setq evil-ex-substitute-global t))

(after! evil
  (setq +evil-want-o/O-to-continue-comments nil))

(after! evil
  (setq global-evil-surround-mode 1))

(map!
 :map evil-normal-state-map
 :after evil
 ("q" #'evil-quit)
 ("Q" #'evil-record-macro))

(map! (:after evil-easymotion
              (:prefix (",")
               :desc "Go to word"            :nv "SPC" #'avy-goto-word-0
               :desc "Go to line"            :nv "l" #'avy-goto-line
               :desc "Go to char with timer" :nv ","   (cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))))

(map! (:after evil-easymotion
       :m "gé" evilem-map
       (:map evilem-map
             "é" (cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))))

;;;###autoload
(defun +evil-window-increase-width-by-five (count)
  "wrapper call associated function by step of five"
  (interactive "p")
  (evil-window-increase-width (+ count 5)))

;;;###autoload
(defun +evil-window-decrease-width-by-five (count)
  "wrapper call associated function by step of five"
  (interactive "p")
  (evil-window-decrease-width (+ count 5)))

;;;###autoload
(defun +evil-window-increase-height-by-three (count)
  "wrapper call associated function by step of three"
  (interactive "p")
  (evil-window-increase-height (+ count 3)))

;;;###autoload
(defun +evil-window-decrease-height-by-three (count)
  "wrapper call associated function by step of three"
  (interactive "p")
  (evil-window-decrease-height (+ count 3)))

(map! (:map evil-window-map
            "+" #'+evil-window-increase-height-by-three
            "-" #'+evil-window-decrease-height-by-three
            "«" #'+evil-window-decrease-width-by-five
            "<" #'+evil-window-decrease-width-by-five
            ">" #'+evil-window-increase-width-by-five
            "»" #'+evil-window-increase-width-by-five))

;;;###autoload
(defun daf/toggle-window-enlargen (&optional window)
  "Toggle window enlarging. Run again to winner-undo."
  (interactive)
  (setq window (window-normalize-window nil))
  (cond
   ((or (> (window-max-delta window nil nil nil nil nil window-resize-pixelwise) 0)
        (> (window-max-delta window t nil nil nil nil window-resize-pixelwise) 0))
    (doom/window-enlargen))
   (t (winner-undo))))

(map! :ni "C-=" #'daf/toggle-window-enlargen)

(after! evil
  (map!
   :n "z <tab>" #'+fold/toggle))

(map!
 (:prefix ("ç" . "daf")
  :n "ç" #'rotate-text
  :n "r" #'rotate-text))

(after! rotate-text
  (add-to-list 'rotate-text-words '("info" "warning" "error"))
  (add-to-list 'rotate-text-words '("enabled" "disabled")))

(setq undo-fu-allow-undo-in-region 't)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun daf/dired-open-file ()
  "In Dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(map! :leader
      (:prefix ("d" . "Dired")
       :desc "Dired"                       "." #'dired
       :desc "Dired jump to current"       "d" #'dired-jump
       :desc "fd input to Dired"           "f" #'fd-dired
       :desc "Dired into project root"     "p" #'project-dired
       :desc "Open Dired in another frame" "D" #'dired-other-window))

(after! dired
  (map! :leader
        :prefix ("t" . "toggle")
        :desc "Side bar" :mvn "d" #'dirvish-side)
  (map! :after dirvish
        :map dirvish-mode-map
        :n "s" #'dired-previous-line
        :n "N" #'dirvish-narrow)
  (map!
   :map dired-mode-map
   :n "g."    #'dired-omit-mode
   :n "c"     #'dired-up-directory
   :n "s"     #'dired-previous-line
   :n "M-RET" #'daf/dired-open-file
   :n "R"     #'dired-do-rename
   :n "L"     #'dired-do-copy
   :n "r"     #'dired-find-file))

(use-package! aweshell
  :defer t
  :commands (aweshell-new aweshell-dedicated-open))

(use-package! vterm
  :config
  (setf (alist-get "woman" vterm-eval-cmds nil nil #'equal)
        '((lambda (topic)
            (woman topic))))
  (setf (alist-get "magit-status" vterm-eval-cmds nil nil #'equal)
        '((lambda (path)
            (magit-status path))))
  (setf (alist-get "dired" vterm-eval-cmds nil nil #'equal)
        '((lambda (dir)
            (dired dir))))
  (set-popup-rules!
    '(("^\\*doom:vterm.*"
       :slot 1 :vslot -2
       :actions (+popup-display-buffer-stacked-side-window-fn)
       :side bottom :width 0.5 :height 0.55 :quit 'other :ttl nil))))

(when (modulep! :term vterm)
  ;; Use monospaced font faces in current buffer
  (defun +vterm-mode-setup ()
    "Sets a fixed width (monospace) font in current buffer"
    (set (make-local-variable 'buffer-face-mode-face)
         '(:family "MonaspiceKr Nerd Font Propo"))
    (face-remap-add-relative 'fixed-pitch)
    (buffer-face-mode t))

  (add-hook 'vterm-mode-hook #'+vterm-mode-setup))

(when (modulep! :term vterm)
  (defun +vterm/split-right ()
    "Create a new vterm window to the right of the current one."
    (interactive)
    (let* ((ignore-window-parameters t)
           (dedicated-p (window-dedicated-p)))
      (select-window (split-window-horizontally))
      (+vterm/here default-directory)))

  (defun +vterm/split-below ()
    "Create a new vterm window below the current one."
    (interactive)
    (let* ((ignore-window-parameters t)
           (dedicated-p (window-dedicated-p)))
      (select-window (split-window-vertically))
      (+vterm/here default-directory))))

(map! :leader
      :when (modulep! :term vterm)
      (:prefix ("e" . "(e)shell")
       :desc "toggle eshell popup"           "E" #'+eshell/toggle
       :desc "open eshell here"              "e" #'+eshell/here
       :desc "open eshell in project root"   "p" #'project-eshell
       :desc "eshell below"                  "K" #'+eshell/split-below
       :desc "eshell right"                  "V" #'+eshell/split-right
       :desc "toggle vterm popup"            "T" #'+vterm/toggle
       :desc "open vterm here"               "t" #'+vterm/here
       :desc "vterm below"                   "k" #'+vterm/split-below
       :desc "vterm right"                   "v" #'+vterm/split-right))

(map!
 (:after flycheck
         (:map flycheck-mode-map
               "M-n" #'flycheck-next-error
               "M-p" #'flycheck-previous-error)))

(map!
 (:after flymake
         (:map flymake-mode-map
               "M-n" #'flymake-goto-next-error
               "M-p" #'flymake-goto-prev-error)))

(map!
 (:when (modulep! :tools lookup)
   :n "z?" #'define-word-at-point))

(use-package! magit
  :ensure nil
  :init
  (map!
   :after magit
   :map magit-mode-map
   :n "$"   #'magit-process-buffer
   :n "g»"  #'+workspace/switch-right
   :n "g«"  #'+workspace/switch-left
   :n "C-t" #'magit-section-forward-sibling
   :n "C-s" #'magit-section-backward-sibling))


(after! gv
  (put 'buffer-local-value 'byte-obsolete-generalized-variable nil))

;; FIXME <- oh the irony
;;(use-package! magit-todos
;;:after magit
;;:config
;;(magit-todos-mode 1))

(dolist (char '(?⏩ ?⏪ ?❓ ?⏸))
  (set-char-table-range char-script-table char 'symbol))

(map!
 :leader
 (:prefix ("i" . "insert")
  :desc "Nerd Icons" "n" #'nerd-icons-insert))

(map! :leader
      (:prefix ("r" . "rss")
       :desc "Elfeed"          "r" #'=rss
       :desc "Update feeds"    "u" #'elfeed-update
       :desc "Open elfeed.org" "." (cmd!
                                    (find-file
                                     (expand-file-name "elfeed.org" org-directory)))))

(after! org
  (setq org-directory "~/Sync/Org/")
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"    ; A task that needs doing & is ready to do
           "NEXT(n)"    ; The next task to do
           "REVIEW(r)"  ; A task being tested/reviewed
           "WAIT(w)"    ; Something external is holding up this task
           "HOLD(h)"    ; This task is paused/on hold because of me
           "MAYBE(m)"   ; A task that need might be droped
           "SOMEDAY(s)" ; A task without a precise timebox
           "FIXME(f)"
           "|"
           "DONE(d)"    ; Task successfully completed
           "DROP(D)")   ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"     ; A task that needs doing
           "[-](S)"     ; Task is in progress
           "[?](W)"     ; Task is being held up or paused
           "|"
           "[X](D)")    ; Task was completed
          (sequence
           "TOREAD(l)"
           "READING(L)"
           "|"
           "READ(R)")
          (sequence
           "|"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"     . +org-todo-active)
          ("[?]"     . +org-todo-onhold)
          ("NEXT"    . +org-todo-active)
          ("WAIT"    . +org-todo-onhold)
          ("REVIEW"  . +org-todo-onhold)
          ("HOLD"    . +org-todo-onhold)
          ("MAYBE"   . +org-todo-onhold)
          ("SOMEDAY" . +org-todo-onhold)
          ("NO"      . +org-todo-cancel)
          ("DROP"    . +org-todo-cancel))))

  ;; (setq org-todo-keywords
        ;; '((sequence
           ;; "TODO(t)"
           ;; "NEXT(n)"
           ;; "REVIEW(r)"
           ;; "WAIT(w)"
           ;; "HOLD(h)"
           ;; "MAYBE(m)"
           ;; "SOMEDAY(s)"
           ;; "FIXME(f)"
           ;; "|"
           ;; "DONE(d)"
           ;; "DROP(D)")
          ;; (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          ;; (sequence "TOREAD(l)" "READING(L)" "|" "READ(R)")
          ;; (sequence "|" "YES(y)" "NO(N)")))

(use-package! org-tempo
  :after org
  :demand t)
(setq org-structure-template-alist
      '(("s" . "src")
        ("e" . "src emacs-lisp")
        ("E" . "src emacs-lisp :results value code :lexical t")
        ("t" . "src emacs-lisp :tangle packages.el")
        ("c" . "comment")
        ("C" . "center")
        ("n" . "note")
        ("x" . "example")
        ("X" . "export")
        ("v" . "verse")
        ("q" . "quote")))

(defun individual-visibility-source-blocks ()
  "Fold some blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map
   (lambda ()
     (let ((case-fold-search t))
       (when (and
              (save-excursion
                (beginning-of-line 1)
                (looking-at org-block-regexp))
              (cl-assoc
               ':hidden
               (cl-third
                (org-babel-get-src-block-info))))
         (org-hide-block-toggle))))))

(add-hook
 'org-mode-hook
 (function individual-visibility-source-blocks))

(after! org
  (setq org-use-speed-commands
        (lambda ()
          (and (looking-at org-outline-regexp)
               (looking-back "^\**")))))

(map!
 :after (evil org evil-org)
 :map  evil-org-mode-map
 :nvm "gc" nil
 :nvm "((" #'evil-org-backward-sentence
 :nvm "))" #'evil-org-forward-sentence)

(after! org
  (setq org-auto-align-tags nil
        org-catch-invisible-edits 'show-and-error
        org-ellipsis " "  ;; other option for reference ⮧
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t
        org-insert-heading-respect-content t
        org-pretty-entities t
        org-special-ctrl-a/e t
        org-tags-column 0))

(custom-set-faces!
  '(org-ellipsis :family "JuliaMono"))

(defun daf/hide-line-numbers ()
  (display-line-numbers-mode 0))

(add-hook 'org-mode-hook 'daf/hide-line-numbers)

(custom-set-faces!
  '(org-document-title :height 1.35)
  '(org-level-1 :weight extra-bold :height 1.30)
  '(org-level-2 :weight bold :height 1.20)
  '(org-level-3 :weight bold :height 1.15)
  '(org-level-4 :weight semi-bold :height 1.13)
  '(org-level-5 :weight semi-bold :height 1.10)
  '(org-level-6 :weight semi-bold :height 1.07)
  '(org-level-7 :weight semi-bold :height 1.05)
  '(org-level-8 :weight semi-bold :height 1.03)
  '(org-level-9 :weight semi-bold))

;; (add-hook 'org-mode-hook #'+org-pretty-mode)
(setq doom-themes-org-fontify-special-tags nil)
(setq org-priority-highest ?A
      org-priority-lowest  ?D
      org-priority-faces
      '((?A . 'nerd-icons-red)
        (?B . 'nerd-icons-orange)
        (?C . 'nerd-icons-yellow)
        (?D . 'nerd-icons-green)))

(appendq! +ligatures-extra-symbols
          (list :list_property "∷"
                :em_dash       "—"
                :arrow_right   "→"
                :arrow_left    "←"
                :arrow_lr      "↔"
                :scheduled     ""
                :properties    "󰻋"
                :end           "⌞⌟"
                :priority_a    #("" 0 1 (face nerd-icons-red))
                :priority_b    #("⚑" 0 1  (face nerd-icons-orange))
                :priority_c    #("⚑" 0 1  (face nerd-icons-yellow))))

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
    :list_property "::"
    :em_dash       "---"
    :arrow_right   "->"
    :arrow_left    "<-"
    :arrow_lr      "<->"
    :scheduled     "SCHEDULED:"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"))

(remove-hook! 'org-load-hook #'+org-init-habit-h)

(after! org
  (setq org-agenda-files
        (mapcar 'file-truename
                (file-expand-wildcards
                 (expand-file-name "*.org" org-directory)))
        ;; org-agenda-tags-column 20
        org-agenda-include-diary t
        org-agenda-block-separator ?─
        org-agenda-hide-tags-regexp ".*"      ;; hide tags in org-agenda
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-done t
        org-agenda-span 1
        org-agenda-start-day "+0d"
        org-log-into-drawer t
        org-habit-graph-column 55
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⁣←── now ─────────────────"
        org-agenda-prefix-format
        '(
          (agenda . "  %?-2i %t ")
          (todo   . " %i %-12:c")
          (habits . " %i %-12:c")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c"))

        org-agenda-category-icon-alist
        `(
          ("Family"
           ,(list (nerd-icons-octicon
                   "nf-oct-people"
                   :v-adjust 0.005)) nil nil :ascent center)
          ("Home"
           ,(list (nerd-icons-codicon
                   "nf-cod-home"
                   :v-adjust 0.005)) nil nil :ascent center)
          ("Health"
           ,(list (nerd-icons-mdicon
                   "nf-md-medical_bag"
                   :v-adjust 0.005)) nil nil :ascent center)
          ("Emacs"
           ,(list (nerd-icons-sucicon
                   "nf-custom-emacs"
                   :v-adjust 0.005)) nil nil :ascent center)
          ("KDE"
           ,(list (nerd-icons-flicon
                   "nf-linux-kde"
                   :v-adjust 0.005)) nil nil :ascent center)
          ("NixOS"
           ,(list (nerd-icons-flicon
                   "nf-linux-nixos"
                   :v-adjust 0.005)) nil nil :ascent center)
          ("Knowledge"
           ,(list
             (nerd-icons-faicon
              "nf-fa-database"
              :height 0.8)) nil nil :ascent center)
          ("habit"
           ,(list
             (nerd-icons-mdicon
              "nf-md-calendar_sync"
              :height 0.9)) nil nil :ascent center)
          ("tasks"
           ,(list
             (nerd-icons-codicon
              "nf-cod-tasklist"
              :height 0.9)) nil nil :ascent center)
          ("Personal"
           ,(list
             (nerd-icons-codicon
              "nf-cod-person"
              :height 0.9)) nil nil :ascent center))))

(map! :leader
      :after org
      :desc "Agenda" "A" #'org-agenda)

(defun daf/org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened."
  (logos-focus-mode))

(add-hook 'org-agenda-mode-hook 'daf/org-agenda-open-hook)

(map! :after org
      :map org-mode-map
      :n "C-t" #'org-next-visible-heading
      :n "C-s" #'org-previous-visible-heading)

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (η #'org-save-all-org-buffers))

(use-package! org-super-agenda
  :defer t
  :after (org-agenda)
  :commands org-super-agenda-mode

  :config
  (setq org-super-agenda-group-property-name "project-id"
        org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(
                            (:auto-group t
                             :order 10)
                            (:name "Next to do"
                             :todo "NEXT"
                             :order 2)
                            (:name "Due Today"
                             :deadline today
                             :order 3)
                            (:name "Important"
                             :tag "Important"
                             :priority "A"
                             :order 6)
                            (:name "Overdue"
                             :deadline past
                             :scheduled past
                             :face error
                             :order 7)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Issues"
                             :tag "issue"
                             :order 12)
                            (:name "Back Burner"
                             :order 14
                             :todo "SOMEDAY")))
                         (org-agenda-list)))))))

  :init
  (org-super-agenda-mode))

(after! evil-org-agenda
  (setq org-super-agenda-header-map evil-org-agenda-mode-map))

(use-package! org-modern
  :hook (org-mode . global-org-modern-mode)
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo))
  (setq
   org-modern-fold-stars '(("◐" . "◓") ("◑" . "◒") ("◐" . "◓"))
   org-modern-table-vertical 1
   org-modern-table-horizontal 0.2
   org-modern-footnote
   (cons nil (cadr org-script-display))
   org-modern-block-fringe nil
   org-modern-todo-faces
   '(("TODO"    :inverse-video t :inherit org-todo)
     ("[-]"     :inverse-video t :inherit +org-todo-active)
     ("NEXT"    :inverse-video t :inherit +org-todo-active)
     ("HOLD"    :inverse-video t :inherit +org-todo-onhold)
     ("WAIT"    :inverse-video t :inherit +org-todo-onhold)
     ("REVIEW"  :inverse-video t :inherit +org-todo-onhold)
     ("MAYBE"   :inverse-video t :inherit +org-todo-onhold)
     ("SOMEDAY" :inverse-video t :inherit +org-todo-onhold)
     ("[?]"     :inverse-video t :inherit +org-todo-onhold)
     ("DROP"    :inverse-video t :inherit +org-todo-cancel)
     ("NO"      :inverse-video t :inherit +org-todo-cancel))
   org-modern-block-name
   '((t . t)
     ("src"     "» " "«")
     ("example" "󰶻⸺ " "󰶺")
     ("note" "» 󰎛" "«")
     ("comment" "» " "«")
     ("quote"   "󰝗" "󰉾")
     ("export"  "⏩" "⏪"))
   ;; org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule (make-string 36 ?─)
   org-modern-keyword
   '((t                  . t)
     ("title"            . "𝙏")
     ("subtitle"         . "𝙩")
     ("author"           . "𝘼")
     ("email"            . "")
     ("date"             . "𝘿")
     ("property"         . "⎈")
     ("options"          . #("󰘵" 0 1 (display (height 0.75))))
     ("startup"          . "⏻")
     ("macro"            . "𝓜")
     ("bind"             . "󰌷")
     ("bibliography"     . "")
     ("cite_export"      . "⮭")
     ("glossary_sources" . "󰒻")
     ("include"          . "⇤")
     ("setupfile"        . "⇚")
     ("html_head"        . "🅷")
     ("html"             . "🅗")
     ("call"             . "󰜎")
     ("name"             . "⁍")
     ("header"           . "›")
     ("caption"          . "☰")
     ("results"          . "⥱"))))

;; (add-to-list 'font-lock-extra-managed-props 'display)
;; (font-lock-add-keywords 'org-mode
;; `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
;; (1 `(face nil
;; display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
;; prepend))) t)

(use-package! org-modern-indent
  :defer t
  :after org-modern
  :hook
  (org-indent-mode . org-modern-indent-mode))

(require 'org-mouse)

(setq org-gtd-update-ack "3.0.0")
(use-package! org-gtd
  :ensure t
  :demand t
  :after org
  :defer t

  :custom
  (org-gtd-directory org-directory)
  (org-gtd-areas-of-focus '("Home" "Personal" "Work" "Family" "Health" "Emacs" "NixOS"))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-edna-use-inheritance t)

  :config
  (setq org-gtd-default-file-name "tasks.org")
  (org-edna-load)

  :init
  (map! :leader
        (:prefix ("z" . "org-gtd")
         :desc "Agenda"            "a" #'org-agenda
         :desc "Capture"           "c" #'org-gtd-capture
         :desc "Clarify item"      "C" #'org-gtd-clarify-item
         :desc "Process inbox"     "i" #'org-gtd-process-inbox
         :desc "Organize"          "o" #'org-gtd-organize
         :desc "Engage"            "e" #'org-gtd-engage
         :desc "Engage"            "z" #'org-gtd-engage
         :desc "Show all next"     "n" #'org-gtd-show-all-next
         :desc "Stuck projects"    "s" #'org-gtd-review-stuck-projects
         :desc "Set area of focus" "f" #'org-gtd-area-of-focus-set-on-item-at-point
         :desc "Open tasks.org"    "t" (cmd! (find-file
                                              (expand-file-name "tasks.org" org-directory))))))

;; FIXME
(after! (evil org evil-org)
  (map!
   :after org-gtd
   :map org-gtd-clarify-map
   :desc "Organize" :m "o" #'org-gtd-organize
   (:prefix ("ç" . "daf")
            "o" #'org-gtd-organize)))

(use-package! org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis   t   ;; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ;; Show links
        org-appear-autosubmarkers t)) ;; Show sub- and superscripts

(use-package! org-books
  :defer t

  :config
  (setq org-books-file (expand-file-name "Perso/book.org" org-directory)))

(after! org
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq
   org-capture-templates
   `(
     ("i" "Inbox" entry  (file "inbox.org") ,(concat "* TODO %?\n"
                                                     "/Entered on/ %U"))
     ("p" "Project" entry (file "projects.org")
      "* %? [%] :project: \n:PROPERTIES: \n:TRIGGER: next-sibling todo!(NEXT) scheduled!(copy)\n:ORDERED: t \n:DATE_ADDED: %u\n:END:\n** Information :info:\n** Notes :notes:\n** Tasks :tasks:\n")
     ("x" "Protocol" entry (file+headline ,(concat org-directory "bookmarks.org") "Bookmarks")
      "** %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     ("L" "Protocol Link" entry (file+headline ,(concat org-directory "bookmarks.org") "Bookmarks")
      "** %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
     ("T" "Todo" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
      "** [ ] %?\n")
     ("t" "Todo" entry (file+headline ,(concat org-directory "todo.org") "Inbox")
      "** [ ] %?\n"))))

(use-package! org-noter
  :defer t)

(use-package! org-now
  :defer t
  :custom
  (org-now-default-cycle-level 'nil)
  :hook (org-now . (lambda () (setq mode-line-format nil)))
  :hook (org-now . (lambda () (face-remap-add-relative 'org-level-1 '(:height 100))))
  :hook (org-now . (lambda () (face-remap-add-relative 'org-level-2 '(:height 120))))
  :hook (org-now . (lambda () (face-remap-add-relative 'org-level-3 '(:height 120))))

  :config
  (setq org-now-location (list (expand-file-name "tasks.org" org-directory) "Actions"))
  (set-popup-rules!
    '(("^\\*org-now"
       :actions (display-buffer-in-side-window)
       :slot 10 :vslot -1 :side right :size +popup-shrink-to-fit :quit nil)))
  :init
  (map!
   :prefix daf/localleader-key
   :n "n" #'org-now
   :n "ç" #'org-now))

(use-package! org-remark
  :defer t
  :init
  (map! :g "C-c n m" #'org-remark-mark
        (:after org-remark
                (:map org-remap-mode-map
                      (:prefix "C-c n"
                       :g "o" #'org-remark-open
                       :g "]" #'org-remark-view-next
                       :g "[" #'org-remark-view-previous
                       :g "r" #'org-remark-remove)))))

(setq org-roam-directory org-directory)

(use-package! image-popup
  :defer t
  :init
  (map!
   :map org-mode-map
   (:prefix ("ç" . "daf")
    :n "i" #'image-popup-display-image-at-point)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme  . t)))

(use-package! spacious-padding
  :ensure t
  :defer
  :hook (after-init . spacious-padding-mode))

(use-package! affe
  :after orderless
  :config
  ;; We always want hidden files and we don't really want to search inside the
  ;; git blobs or the syncthing versions
  (let ((base "rg --color=never --hidden --glob !*.git/ --glob !*.stversions/"))
    (setq affe-find-command (format "%s --files" base))
    (setq affe-dir-command "fd --color=never --type directory")
    (setq affe-grep-command (format "%s --null --max-columns=1000 --no-heading --line-number -v ^$ ." base)))

  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler)
  (setq affe-highlight-function #'orderless-highlight-matches)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(after! avy
  (setf (alist-get ?d avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?D avy-dispatch-alist) 'avy-action-kill-whole-line))

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(after! avy
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line))

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(after! avy
  (setf (alist-get ?m avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?M avy-dispatch-alist) 'avy-action-teleport-whole-line))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(after! avy
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package! blamer
  :defer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :italic t)))
  :init
  (map!
   :leader
   (:prefix ("g" . "git")
    :desc  "Blamer posframe commit info" "," #'blamer-show-posframe-commit-info
    :desc  "Blamer mode"                 ";" #'blamer-mode)))

(use-package! circadian
  :ensure t
  :init

  (add-hook 'circadian-after-load-theme-hook
            #'(lambda (theme)
                (message "Setting circadian theme.")
                (cond ((eq doom-theme daf/dark-theme)
                       (message "Setting dark modeline theme."))
                       ;; (nano-theme-set-dark)
                      ((eq doom-theme daf/light-theme)
                       (message "Setting light modeline theme.")))
                       ;; (nano-theme-set-light-tinted)
                ;; (nano-faces)
                ;; (nano-theme)
                (doom/reload-theme)))

  :config
  (setq circadian-themes `(("7:30" . ,daf/light-theme)
                           ("19:30" . ,daf/dark-theme)))
  (circadian-setup))

(use-package! ef-themes
  :defer t
  :config
  (setq
   ;; ef-themes-variable-pitch-ui t
   ef-themes-italic-constructs t
   ef-themes-bold-constructs t
   ef-themes-mixed-fonts t)

  (defun daf/ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
     (setq hl-todo-keyword-faces
           `(("HOLD" . ,yellow)
             ("TODO" . ,red)
             ("NEXT" . ,blue)
             ("THEM" . ,magenta)
             ("PROG" . ,cyan-warmer)
             ("OKAY" . ,green-warmer)
             ("DONT" . ,yellow-warmer)
             ("DROP" . ,red-warmer)
             ("FAIL" . ,red-warmer)
             ("BUG" . ,red-warmer)
             ("DONE" . ,green)
             ("NOTE" . ,blue-warmer)
             ("KLUDGE" . ,cyan)
             ("HACK" . ,cyan)
             ("TEMP" . ,red)
             ("FIXME" . ,red-warmer)
             ("XXX+" . ,red-warmer)
             ("REVIEW" . ,red)
             ("DEPRECATED" . ,yellow))))))
;;(add-hook 'ef-themes-post-load-hook #'daf/ef-themes-hl-todo-faces)

(use-package! embrace
  :defer t
  :init
  (map!
   (:prefix ("," . "daf")
            (:prefix ("e" . "embrace")
             :desc "Commander" :nv  "e" #'embrace-commander
             :desc "Add"       :nv  "a" #'embrace-add
             :desc "Change"    :nv  "c" #'embrace-change
             :desc "Delete"    :nv  "d" #'embrace-delete)))

  :config
  (after! evil-embrace
    (setq evil-embrace-show-help-p t))
  (setq embrace-show-help-p t))

;; (use-package! fontaine
;;   :config

;;   ;; Iosevka Comfy is my highly customised build of Iosevka with
;;   ;; monospaced and duospaced (quasi-proportional) variants as well as
;;   ;; support or no support for ligatures:
;;   ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
;;   ;;
;;   ;; Iosevka Comfy            == monospaced, supports ligatures
;;   ;; Iosevka Comfy Fixed      == monospaced, no ligatures
;;   ;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
;;   ;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
;;   ;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
;;   ;; Iosevka Comfy Motion     == monospaced, supports ligatures, fancier glyphs
;;   ;; Iosevka Comfy Motion Duo == as above, but quasi-proportional
;;   (setq fontaine-presets
;;         '((smaller
;;            :default-family "Iosevka Comfy Wide Fixed"
;;            :default-height 90
;;            :variable-pitch-family "Iosevka Comfy Wide Duo")
;;           (small
;;            :default-family "Iosevka Comfy Wide Fixed"
;;            :default-height 100
;;            :variable-pitch-family "Iosevka Comfy Wide Duo")
;;           (regular
;;            :default-height 120)
;;           (large
;;            :default-weight semilight
;;            :default-height 150
;;            :bold-weight extrabold)
;;           (larger
;;            :default-weight semilight
;;            :default-height 160
;;            :bold-weight extrabold)
;;           (code-demo
;;            :default-family "Iosevka Comfy Fixed"
;;            :default-weight semilight
;;            :default-height 190
;;            :variable-pitch-family "Iosevka Comfy Duo"
;;            :bold-weight extrabold)
;;           (presentation
;;            :default-weight semilight
;;            :default-height 220
;;            :bold-weight extrabold)
;;           (legally-blind
;;            :default-weight semilight
;;            :default-height 260
;;            :bold-weight extrabold)
;;           (merriweather
;;            :default-family "Merriweather"
;;            :variable-pitch-family "Merriweather"
;;            :default-height 150)
;;           (iosevka-nerd-font
;;            :default-family "Iosevka Nerd Font")
;;           (sarasa
;;            :default-family "Sarasa Term J"
;;            :variable-pitch-family "Sarasa Term Slab TC")
;;           (ibm-plex-sans
;;            :default-family "IBM Plex Sans")
;;           (ibm-plex-mono
;;            :default-family "IBM Plex Mono")
;;           (t
;;            ;; I keep all properties for didactic purposes, but most can be
;;            ;; omitted.  See the fontaine manual for the technicalities:
;;            ;; <https://protesilaos.com/emacs/fontaine>.
;;            :default-family "Iosevka Comfy"
;;            :default-weight regular
;;            :default-height 120
;;            :fixed-pitch-family nil      ; falls back to :default-family
;;            :fixed-pitch-weight nil      ; falls back to :default-weight
;;            :fixed-pitch-height 1.0
;;            :fixed-pitch-serif-family nil ; falls back to :default-family
;;            :fixed-pitch-serif-weight nil ; falls back to :default-weight
;;            :fixed-pitch-serif-height 1.0
;;            :variable-pitch-family "Iosevka Comfy Motion Duo"
;;            :variable-pitch-weight nil
;;            :variable-pitch-height 1.0
;;            :bold-family nil             ; use whatever the underlying face has
;;            :bold-weight bold
;;            :italic-family nil
;;            :italic-slant italic
;;            :line-spacing nil)))

;;   ;; Set last preset or fall back to desired style from `fontaine-presets'.
;;   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;;   ;; The other side of `fontaine-restore-latest-preset'.
;;   (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

;;   ;; Persist font configurations while switching themes (doing it with
;;   ;; my `modus-themes' and `ef-themes' via the hooks they provide).
;;   (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
;;     (add-hook hook #'fontaine-apply-current-preset))

;;   :init
;;   (map! :leader
;;         (:prefix ("ç" . "daf")
;;                      "F" #'fontaine-set-face-font
;;                      "f" #'fontaine-set-preset)))

(use-package! indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))

  :hook (prog-mode . indent-bars-mode)

  :init
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Indent Bars" :mvn "i" #'indent-bars-mode)))

(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

(use-package! jinx
  :defer t
  :init
  (add-hook 'doom-init-ui-hook #'global-jinx-mode)
  (map!
   :leader
   (:prefix ("t" . "toggle")
    :desc "Spell checker"  "s"  #'jinx-mode))
  :config
  ;; Use my custom dictionary
  (setq jinx-languages "en fr"
        global-jinx-modes '(text-mode conf-mode))
  ;; Extra face(s) to ignore
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  (after! ispell
    (global-set-key [remap ispell-word] #'jinx-correct))
  (after! evil-commands
    (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
    (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))
  ;; I prefer for `point' to end up at the start of the word,
  ;; not just after the end.
  (advice-add 'jinx-next :after (lambda (_) (left-word))))

(use-package! languagetool
  :defer t
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-correction-language "en-US"  ;; 'auto' seems to target "en", which isn't working as well as 'en-US'
        languagetool-console-command "/etc/profiles/per-user/daf/share/languagetool-commandline.jar"
        languagetool-server-command "/etc/profiles/per-user/daf/share/languagetool-server.jar")
  :init
  (map!
   (:prefix ("z~" . "languagetool")
    :n "b" #'languagetool-correct-buffer
    :n "c" #'languagetool-correct-at-point
    :desc "set language" :n "l" #'(lambda() (interactive) (languagetool-set-language (completing-read "lang: " '("fr" "en-US"))))
    :n "~" #'languagetool-check)))

(use-package! litable
  :custom
  (litable-list-file (expand-file-name "litable-lists.el" doom-cache-dir))
  :init
  (map! :localleader
        :map emacs-lisp-mode-map
        "l" #'litable-mode))

(use-package! logos
  :defer t
  :hook (logos-focus-mode . (lambda () (olivetti-mode 1)))
  :config
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos--page-delimiter))
          (org-mode . ,(format "\\(^\\*\\{1,3\\} +\\|^-\\{5\\}$\\|%s\\)" logos--page-delimiter))))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line nil
                logos-hide-buffer-boundaries t
                logos-outlines-are-pages t
                logos-hide-fringe t
                logos-variable-pitch t
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :init
  (map! :leader
        (:prefix "t"
         :desc "Logos" "L" #'logos-focus-mode)))

(use-package! mlscroll
  :init
  (setq mlscroll-right-align t)
  :config
  (mlscroll-mode 1))
(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check buffer-position time)))

(use-package! modus-themes
  :init
  (setq
   ;; modus-themes-variable-pitch-ui t
   modus-themes-mixed-fonts t
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t))

(use-package! nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . mixed-pitch-mode)
  :hook (nov-mode . visual-line-mode)
  :hook (nov-mode . visual-fill-column-mode)
  :hook (nov-mode . hide-mode-line-mode)
  :hook (nov-mode . (lambda () (hl-line-mode -1)))
  :hook (nov-mode . (lambda ()
                      (set (make-local-variable 'scroll-margin) 1)))

  :config
  (setq visual-fill-column-center-text t
        nov-text-width t
        nov-variable-pitch t))

(defun daf/scroll-bottom-line-to-top ()
  (interactive)
  (evil-window-bottom)
  (evil-scroll-line-to-top (line-number-at-pos))
  (+nav-flash/blink-cursor))
(defun daf/scroll-top-line-to-bottom ()
  (interactive)
  (evil-window-top)
  (evil-scroll-line-to-bottom (line-number-at-pos))
  (+nav-flash/blink-cursor))
:init
(map!
 :map nov-mode-map
 :n "T" #'daf/scroll-bottom-line-to-top
 :n "S" #'daf/scroll-top-line-to-bottom)

(map!
 :leader (:prefix ("t" . "toggle")
          :desc "Obvious" "O" #'obvious-mode))

(use-package! olivetti
  :defer t
  :custom
  (olivetti-body-width 0.6)
  :config
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 80)
  :init
  (map! :leader
        (:prefix "t"
         :desc "Olivetti" "o" #'olivetti-mode)))

(use-package! paren-face
  :config
  (global-paren-face-mode 1))

;;;###autoload
(defun daf/puni-kill-line ()
  "Kill a line forward while keeping expressions balanced.
If nothing can be deleted, kill backward. If still nothing can be
deleted, kill the pairs around point."
  (interactive)
  (let ((bounds (puni-bounds-of-list-around-point)))
    (if (eq (car bounds) (cdr bounds))
        (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
          (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
      (if (eq (point) (cdr bounds))
          (puni-backward-kill-line)
        (puni-kill-line)))))

(use-package! puni
  :init
  (map!
   :map puni-mode-map
   (:prefix ("," . "daf")
    :nv "TAB" #'delete-indentation
    :nv "v"   #'puni-expand-region
    :nv "s"   #'puni-squeeze
    :nv "/"   #'puni-split
    :nv "S"   #'puni-splice
    :nv "r"   #'puni-raise
    :nv "t"   #'puni-transpose
    (:prefix ("w"  . "wrap")
     :nv "p" #'puni-wrap-round
     :nv "(" #'puni-wrap-round
     :nv "b" #'puni-wrap-square
     :nv "[" #'puni-wrap-square
     :nv "c" #'puni-wrap-curly
     :nv "{" #'puni-wrap-curly
     :nv "a" #'puni-wrap-angle
     :nv "«" #'puni-wrap-angle)
    :nv "d" #'daf/puni-kill-line
    :nv "D" #'puni-backward-kill-line
    :nv "C" #'puni-convolute
    :nv "p" #'puni-backward-sexp-or-up-list
    :nv "P" #'puni-beginning-of-sexp
    :nv "n" #'puni-forward-sexp-or-up-list
    :nv "N" #'puni-end-of-sexp
    :nv "<" #'puni-slurp-backward
    :nv ">" #'puni-slurp-forward
    :nv "«" #'puni-slurp-backward
    :nv "»" #'puni-slurp-forward))
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package! pulsar
  :ensure t
  :defer t
  :config
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-up)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'evil-ex-search-next)
  (add-to-list 'pulsar-pulse-functions 'evil-ex-search-previous)
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)
  (custom-set-faces!
    `(pulsar-magenta :background ,everforest-hard-dark-purple))
  (pulsar-global-mode 1)
  :init
  (map!
   :leader
   (:prefix ("t" . "toggle")
    :desc "Pulse line" "." #'pulsar-pulse-line))
  :hook
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red)))

(use-package! rainbow-mode
  :hook prog-mode
  :init
  (map!
   :leader (:prefix ("t" . "toggle")
            :desc "Rainbow mode"  "R" #'rainbow-mode)))

(map! :map evil-window-map
      "SPC" #'rotate-layout)

(use-package! tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  (setq tempel-path (expand-file-name "templates/*" doom-user-dir))

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package! tempel-collection)

(use-package! verb
  :ensure t
  :defer t
  :config
  (setq verb-json-use-mode 'json-mode)
  (defun graphql-to-json (rs)
    ;; Modify RS and return it (RS is a request specification, type `verb-request-spec')
    (oset rs body (replace-regexp-in-string "\n" "" (format-message "{\"query\": \"%s\"}" (oref rs body))))
    rs)

  :init
  (map! :leader
        (:prefix ("v" . "verb")
         :map verb-mode-map
         :desc "send request"              "V" #'verb-send-request-on-point-other-window
         :desc "send request other window" "v" #'verb-send-request-on-point-other-window-stay
         :desc "re-send request"           "r" #'verb-re-send-request
         :desc "export curl request"       "c" #'verb-export-request-on-point-curl
         (:prefix ("h" . "verb help")
          :desc "show sent request"        "r" #'verb-show-request
          :desc "show headers"             "h" #'verb-toggle-show-headers
          :desc "show vars"                "v" #'verb-show-vars
          :desc "show logs"                "l" #'verb-show-log
          :desc "set var"                  "s" #'verb-set-var
          :desc "unset vars"               "u" #'verb-unset-vars))))

(set-popup-rules!
  '(("^\\*HTTP Response.*" :quit t :side right :size 0.4 :modeline nil)))

(use-package! vundo
  :defer t
  :init
  (evil-define-key* 'normal 'global "U" #'vundo))

(use-package! zoom
  :ensure t
  :defer t
  ;; :hook (doom-first-input . zoom-mode)

  :init
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Zoom mode" "=" #'zoom-mode))
  (setq zoom-mode nil)

  :config
  (setq zoom-size '(0.7 . 0.7)
        zoom-ignored-major-modes         '(dired-mode vterm-mode help-mode
                                           helpful-mode rxt-help-mode
                                           help-mode-menu org-mode)
        zoom-ignored-buffer-names        '("*doom:scratch*" "*info*"
                                           "*helpful variable: argv*")
        zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*")))

(use-package! lsp-mode
  :ensure t)

(use-package! lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package! nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(after! savehist
  (add-to-list 'savehist-additional-variables 'evil-markers-alist)
  (add-hook! 'savehist-save-hook
    (kill-local-variable 'evil-markers-alist)
    (dolist (entry evil-markers-alist)
      (when (markerp (cdr entry))
        (setcdr entry (cons (file-truename (buffer-file-name (marker-buffer (cdr entry))))
                            (marker-position (cdr entry)))))))
  (add-hook! 'savehist-mode-hook
    (setq-default evil-markers-alist evil-markers-alist)
    (kill-local-variable 'evil-markers-alist)
    (make-local-variable 'evil-markers-alist)))

(use-package! evil-fringe-mark
  :defer t
  :after evil
  :config
  ;; Use right fringe
  (setq-default right-fringe-width 16)
  (setq-default evil-fringe-mark-side 'right-fringe))
:init
(global-evil-fringe-mark-mode 1)

(use-package! org-wild-notifier
  ;; The add-hook enables the mode after init
  :defer t
  :init
  (add-hook 'doom-post-init-hook #'org-wild-notifier-mode t)
  :config
  (setq alert-default-style 'notification))

(use-package! nano-modeline
  :custom
  (fringes-outside-margins t))

(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
(add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
(add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
(add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
(add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
(add-hook 'term-mode-hook            #'nano-modeline-term-mode)
(add-hook 'vterm-mode-hook           #'daf/nano-modeline-vterm-mode)
(add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
(add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

(add-hook 'doom-load-theme-hook      #'daf/set-nano-modeline-face)

(defun daf/set-nano-modeline-face ()
  (let ((nano-modeline-face
         (if (eq (face-attribute 'header-line :background) 'unspecified)
             'mode-line
           'header-line)))

    (custom-set-faces!
      `(nano-modeline-active
        :foreground ,(face-attribute 'default :foreground)
        :background ,(face-attribute nano-modeline-face :background)
        :box nil)
      '(nano-modeline-inactive
        :inherit 'mode-line-inactive
        :box nil))))

(defun daf/nano-modeline-vterm-mode ()
  "Nano line for term mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status ">_") " ")
           '((nano-modeline-default-directory) " "
             (nano-modeline-window-dedicated))))

;;;###autoload
(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))
