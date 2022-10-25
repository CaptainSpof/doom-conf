;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Cédric Da Fonseca"
      user-mail-address "captain.spof@gmail.com")

(setq-default scroll-margin 3)

(setq gcmh-high-cons-threshold most-positive-fixnum
      max-specpdl-size 100000)
(setq gc-cons-threshold (* 100 1024 1024)) ;; REVIEW: might be overkill

(defvar daf/localleader-key "SPC ç"
  "The localleader prefix key, for major-mode specific commands.")

(setq bookmark-default-file (expand-file-name "local/bookmarks" doom-user-dir)
     projectile-known-projects-file (expand-file-name "local/projectile.projects" doom-user-dir))

(setq +workspaces-switch-project-function #'dired)

(setq +zen-text-scale 0)

(after! tramp
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "⫚-\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "⋔-\\1"))))

(map!
 (:map 'override
  :v "v" #'er/expand-region
  :v "V" #'er/contract-region))

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(after! embark
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump)))

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
                   (+evil-window-increase-width-by-five . "»")
                   (+evil-window-increase-width-by-five . ">")
                   (+evil-window-decrease-width-by-five . "«")
                   (+evil-window-decrease-width-by-five . "<"))
                 "Repeatable map for window resizing")

;;;###autoload
(defun daf/window-toggle-lock-size ()
  "Lock/unlock the current window size."
  (interactive)
  (let ((window (get-buffer-window)))
    (cond ((or (window-size-fixed-p window)
               (window-size-fixed-p window t))
           (daf/window-unlock-size))
          (t
           (daf/window-lock-size)))))

;;;###autoload
(defun daf/window-lock-size ()
  "Lock the current window size."
  (interactive)
  (window-preserve-size window t t)
  (message "locking current window size"))

;;;###autoload
(defun daf/window-unlock-size ()
  "Unlock the current window size."
  (interactive)
  (window-preserve-size window t nil)
  (message "unlocking current window size"))

;;;###autoload
(defun daf/window-shrink-and-lock ()
  "Shrink and lock the current window size."
  (interactive)
  (let* ((window (get-buffer-window))
         (curr-h  (window-height window))
         (curr-w  (window-width window))
         (delta-h    (- 5 curr-h))
         (delta-w    (- 5 curr-w)))
    (save-excursion
      (save-selected-window (select-window window)
                            (enlarge-window delta-w delta-h)
                            (daf/window-lock-size)))))

(map! :leader
        (:prefix "w"
         :desc "daf/toggle-lock" "," #'daf/window-toggle-lock-size
         :desc "daf/shrink" "." #'daf/window-shrink-and-lock))

(after! company
  (setq
   company-show-quick-access 'left
   company-quick-access-keys '("b" "é" "p" "o" "w")
   company-quick-access-modifier 'control
   company-dabbrev-other-buffers t)

  (set-company-backend! 'prog-mode '(company-capf company-dabbrev company-dabbrev-code)))

(map! [remap describe-bindings] #'embark-bindings
      "C-," #'embark-act)

(after! vertico
  (vertico-multiform-mode)

  (setq vertico-multiform-commands
        '((consult-line buffer)))
  (setq vertico-multiform-categories
        '((consult-grep buffer)))

  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . left)
          (window-width . 0.3))))

(setq doom-theme 'ef-duo-light)
(setq ef-themes-to-toggle '(ef-duo-light ef-tritanopia-dark))

(set-face-foreground 'window-divider (face-background 'header-line))

(setq fancy-splash-image (expand-file-name "misc/splash-images/ferris.svg" doom-user-dir))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file"            :ne "f" #'find-file
        :desc "Recent files"         :ne "r" #'consult-recent-file
        :desc "Restore last session" :ne "R" #'doom/restart-and-restore
        :desc "Config dir"           :ne "C" #'doom/open-private-config
        :desc "Open config.org"      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
        :desc "Open dotfile"         :ne "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)"         :ne "n" #'org-roam-node-find
        :desc "Switch buffer"        :ne "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ne "B" #'consult-buffer
        :desc "IBuffer"              :ne "i" #'ibuffer
        :desc "Projects"             :ne "p" #'project-switch-project
        :desc "Set theme"            :ne "t" #'consult-theme
        :desc "Quit"                 :ne "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings"     :ne "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))

(map! :leader :desc "Dashboard" "D" #'+doom-dashboard/open)

;; An evil mode indicator is redundant with cursor shape
(advice-add #'doom-modeline-segment--modals :override #'ignore)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(map! :leader
      (:prefix-map ("TAB" . "workspace")
       :desc "Switch workspace" :mvn "TAB" #'+workspace/switch-to
       :desc "Display tab bar" :mvn "." #'+workspace/display))

(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (dired-jump))

(after! evil
  (setq evil-ex-substitute-global t))

(after! evil
  (setq +evil-want-o/O-to-continue-comments nil))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map "q" 'evil-quit)
  (define-key evil-motion-state-map (kbd "Q") 'evil-record-macro))

(map! (:after evil-easymotion
              (:prefix (",")
               :n "," (cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))))

(map! (:after evil-easymotion
       :m "gé" evilem-map
       (:map evilem-map
             "é" (cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))))

(defun +evil-window-increase-width-by-five (count)
  "wrapper call associated function by step of five"
  :repeat nil
  (interactive "p")
  (evil-window-increase-width (+ count 5)))

(defun +evil-window-decrease-width-by-five (count)
  "wrapper call associated function by step of five"
  :repeat nil
  (interactive "p")
  (evil-window-decrease-width (+ count 5)))

(defun +evil-window-increase-height-by-three (count)
  "wrapper call associated function by step of three"
  :repeat nil
  (interactive "p")
  (evil-window-increase-height (+ count 3)))

(defun +evil-window-decrease-height-by-three (count)
  "wrapper call associated function by step of three"
  :repeat nil
  (interactive "p")
  (evil-window-decrease-height (+ count 3)))


(map! (:map evil-window-map
            "+" #'+evil-window-increase-height-by-three
            "-" #'+evil-window-decrease-height-by-three
            "«" #'+evil-window-decrease-width-by-five
            "<" #'+evil-window-decrease-width-by-five
            ">" #'+evil-window-increase-width-by-five
            "»" #'+evil-window-increase-width-by-five))

(after! evil
  (map!
   :n "z <tab>" #'+fold/toggle))

(map!
 (:prefix ("ç" . "daf")
  :n "r" #'rotate-text))

(after! rotate-text
  (add-to-list 'rotate-text-words '("info" "warning" "error")))

(setq undo-fu-allow-undo-in-region 't)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(map! :leader
      (:prefix-map ("d" . "dired")
       :desc "Dired"                       "." #'dired
       :desc "Dired jump to current"       "d" #'dired-jump
       :desc "fd input to dired"           "f" #'fd-dired
       :desc "Dired into project root"     "p" #'project-dired
       :desc "open dired in another frame" "D" #'dired-other-window))

;; (map! :leader
;;       :prefix-map ("t" . "toggle")
;;       :desc "Side bar" :mvn "s" #'dirvish-side)

;; (map! :after dirvish
;;       :map dirvish-mode-map
;;       :n "S" #'dirvish-narrow
;;       :n "F" #'dirvish-layout-toggle)

(after! dired
  (map!
   :map dired-mode-map
   :n "c" #'dired-up-directory
   ;; :n "t" #'evil-next-line ;; HACK
   :n "r" #'dired-find-file))

;; Use monospaced font faces in current buffer
(defun +vterm-mode-setup ()
  "Sets a fixed width (monospace) font in current buffer"
  (setq buffer-face-mode-face '(:family "IBM Plex Mono" :height 110))
  (face-remap-add-relative 'fixed-pitch)
  (buffer-face-mode))

(add-hook 'vterm-mode-hook #'+vterm-mode-setup)

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
    (+vterm/here default-directory)))

(map! :leader
      (:prefix-map ("e" . "(e)shell")
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
 (:when (modulep! :tools lookup)
   :n "z?" #'define-word-at-point))

;; (setenv "LSP_USE_PLISTS" "1")
;; (setq lsp-use-plists "true")

(setq org-directory "~/Sync/Org/"
      org-agenda-files (directory-files-recursively "~/Sync/Org/" "\\.org$"))

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "NEXT(n)" "REVIEW(r)" "WAIT(w)" "HOLD(h)" "MAYBE(m)" "IDEA(i)" "|" "DONE(d)" "KILL(k)" "DROP(D)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(N)")))

(custom-set-faces
 '(org-code ((t (:inherit ef-themes-fixed-pitch :foreground "#9f4a00" :slant italic))))))

(use-package! org-mouse)

(setq org-hide-emphasis-markers t
      org-fontify-quote-and-verse-blocks t ;; make quotes blocks /italic/
      org-ellipsis " ↩ ")

;; hide line numbers in olivetti mode
(defun daf/hide-line-numbers ()
  (display-line-numbers-mode 0))

(add-hook 'org-mode-hook 'daf/hide-line-numbers)

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

(add-hook 'org-mode-hook 'org-appear-mode)

(after! org
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq org-capture-templates `(
                                ("x" "Protocol" entry (file+headline ,(concat org-directory "bookmarks.org") "Bookmarks")
                                 "** %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline ,(concat org-directory "bookmarks.org") "Bookmarks")
                                 "** %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
                                ("e" "Epic" entry (file ,(concat org-directory "Voilà/epics.org"))
                                 "* TODO %?\n** Description\n** [%] Tasks\n")
                                ("i" "Issue" entry (file ,(concat org-directory "Voilà/issues.org"))
                                 "* TODO %?\n** Description\n** [%] Tasks\n")
                                ("v" "Voilà note" entry (file+headline ,(concat org-directory "Voilà/notes.org") "Inbox")
                                 "** [ ] %?\n")
                                ("t" "Todo" entry (file+headline ,(concat org-directory "todo.org") "Inbox")
                                 "** [ ] %?\n")
                                )))

(use-package! org-now
  :custom
  (org-now-default-cycle-level 'nil)
  :hook (org-now . (lambda () (setq mode-line-format nil)))
  :hook (org-now . (lambda () (face-remap-add-relative 'org-level-1 '(:height 100))))
  :hook (org-now . (lambda () (face-remap-add-relative 'org-level-2 '(:height 130))))
  :hook (org-now . (lambda () (face-remap-add-relative 'org-level-3 '(:height 130))))

  :config
  (setq org-now-location (list (expand-file-name "Voilà/notes.org" org-directory) "Inbox"))
   (set-popup-rules!
    '(("^\\*org-now"
       :actions (display-buffer-in-side-window)
       :slot 10 :vslot -1 :side right :size +popup-shrink-to-fit :quit nil)))
  :init
  (map!
   :prefix daf/localleader-key
   :n "n" #'org-now))

(after! org
  (use-package! org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star '("●" "◉" "○" "◆" "◈" "◇" "✤" "✿" "✜" "▶" "▷" "●" "◉" "○" "◆" "◈" "◇" "✤" "✿" "✜")
          org-modern-table-vertical 1
          org-modern-table-horizontal 0.2
          org-modern-list '((?+ . "+")
                            (?- . "–")
                            (?* . "•"))
          org-modern-todo-faces

          '(("TODO" :inverse-video t :inherit org-todo)
            ("PROJ" :inverse-video t :inherit +org-todo-project)
            ("[-]"  :inverse-video t :inherit +org-todo-active)
            ("NEXT"  :inverse-video t :inherit +org-todo-active)
            ("HOLD" :inverse-video t :inherit +org-todo-onhold)
            ("WAIT" :inverse-video t :inherit +org-todo-onhold)
            ("REVIEW" :inverse-video t :inherit +org-todo-onhold)
            ("MAYBE" :inverse-video t :inherit +org-todo-onhold)
            ("[?]"  :inverse-video t :inherit +org-todo-onhold)
            ("KILL" :inverse-video t :inherit +org-todo-cancel)
            ("DROP" :inverse-video t :inherit +org-todo-cancel)
            ("NO"   :inverse-video t :inherit +org-todo-cancel))
          org-modern-footnote
          (cons nil (cadr org-script-display))
          org-modern-block-fringe nil
          org-modern-block-name
          '((t . t)
            ("src" "»" "«")
            ("example" "»–" "–«")
            ("quote" "❝" "❞")
            ("export" "⏩" "⏪"))
          org-modern-progress nil
          org-modern-priority nil
          org-modern-horizontal-rule (make-string 36 ?─)
          org-modern-keyword
          '((t . t)
            ("title" . "𝙏")
            ("subtitle" . "𝙩")
            ("author" . "𝘼")
            ("email" . #("" 0 1 (display (raise -0.14))))
            ("date" . "𝘿")
            ("property" . "☸")
            ("options" . "⌥")
            ("startup" . "⏻")
            ("macro" . "𝓜")
            ("bind" . #("" 0 1 (display (raise -0.1))))
            ("include" . "⇤")
            ("setupfile" . "⇚")
            ("name" . "⁍")
            ("header" . "›")
            ("caption" . "☰")
            ("RESULTS" . "🠶")))
    (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo))))

(use-package! org-modern-indent
  :hook
  (org-indent-mode . org-modern-indent-mode))

(setq org-roam-directory "~/Sync/Org")

(use-package! image-popup
  :init
  (map!
   :map org-mode-map
   :prefix daf/localleader-key
   :n "i" #'image-popup-display-image-at-point))

(use-package! blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :italic t)))
  :init
  (map!
   :leader
   (:prefix-map ("g" . "git")
    :desc  "Blamer posframe commit info" "," #'blamer-show-posframe-commit-info
    :desc  "Blamer mode"                 ";" #'blamer-mode)))

(use-package! burly
  :config
  (setq burly-bookmark-prefix "# ")

  (defun +burly-before-quit ()
    (message "burly: saving session")
    (burly-bookmark-windows (format "## last session %s"
                                    (format-time-string "%Y-%m-%d %H:%M")))
    't))

(defvar prompt-y-n-q '((?y "y" (lambda () 't))
                       (?n "n" (lambda () nil))
                       (?q "q" (lambda () (+burly-before-quit)))))

(defun daf-quit-choose (prompt)
  (let ((choice (read-char-choice (format "%s y/n/q (save session)" prompt)
                                   (mapcar #'car prompt-y-n-q))))
    (funcall (nth 2 (assoc choice prompt-y-n-q)))))

(defun daf/doom-quit-p (&optional prompt)
  (or (not (ignore-errors (doom-real-buffer-list)))
      (daf-quit-choose (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))

(defun +daf/doom-quit-fn (&rest _)
  (daf/doom-quit-p
   (format "%s  %s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default))
           "Really quit Emacs?")))

(setq confirm-kill-emacs #'+daf/doom-quit-fn)

(use-package! elogcat
  :config
  (defun daf/elogcat-set-tail ()
    "Add a limit of line to the command"
    (interactive)
    (setq elogcat-logcat-command (concat elogcat-logcat-command " -T 50")))

  (defun daf/elogcat-set-include-filter-pid ()
    "Try to determine a PID from an input, and set it as a filter"
    (interactive)
    (elogcat-set-include-filter (substring
                                 (shell-command-to-string
                                  (format "adb shell ps | grep -F %s | tr -s '[:space:]' ' ' | cut -d' ' -f2" (read-string "app namespace: ")))
                                 0 -1)))
  :init
  (map! :map elogcat-mode-map
        :localleader
        "i" #'elogcat-set-include-filter
        "I" #'elogcat-clear-include-filter
        "x" #'elogcat-set-exclude-filter
        "X" #'elogcat-clear-exclude-filter
        "p" #'daf/elogcat-set-include-filter-pid
        "t" #'daf/elogcat-set-tail
        "g" #'elogcat-show-status
        "m" #'elogcat-toggle-main
        "s" #'elogcat-toggle-system
        "e" #'elogcat-toggle-events
        "r" #'elogcat-toggle-radio
        "k" #'elogcat-toggle-kernel
        "c" #'elogcat-erase-buffer))

(setq emojify-display-style 'unicode)
(setq emojify-emoji-styles '(unicode))

(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀"
    ;; I just want to see this as text
    "©" "™")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))

(use-package! justl)

(defun daf/consult-just ()
  "Prompt a list of just recipes from the project. Run the selected candidate."
  (interactive)
  (let (recipes)
    (setq recipes (justl--get-recipies-with-desc (justl--find-justfiles (projectile-project-root))))
    (justl--exec justl-executable (list (completing-read "Choose an action: " recipes)))))

(use-package! languagetool
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

(use-package olivetti
  :custom
  (olivetti-body-width 0.6)
  :config
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 80)
  :init
    (map! :leader
        (:prefix "t"
         :desc "Olivetti" "o" #'olivetti-mode)))

(use-package! logos
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

(use-package! magit-pretty-graph
  :after magit
  :init
  (setq magit-pg-command
        (concat "git --no-pager log"
                " --topo-order --decorate=full"
                " --pretty=format:\"%H%x00%P%x00%an%x00%ar%x00%s%x00%d\""
                " -n 2000")) ;; Increase the default 100 limit

  (map! :localleader
        :map (magit-mode-map)
        :desc "Magit pretty graph" "p" (cmd! (magit-pg-repo (magit-toplevel)))))

(use-package! ef-themes
  :config
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-headings           ; read the manual's entry of the doc string
        '((0 . (variable-pitch regular 1.5)) ; absence of weight means `bold'
          (1 . (variable-pitch regular 1.4))
          (2 . (variable-pitch regular 1.3))
          (3 . (variable-pitch regular 1.2))
          (4 . (variable-pitch regular 1.1))
          (5 . (variable-pitch regular 1.1))
          (6 . (variable-pitch regular 1.1))
          (7 . (variable-pitch regular 1.1))
          (t . (variable-pitch regular 1.1))))
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
              ("DEPRECATED" . ,yellow)))))

  (add-hook 'ef-themes-post-load-hook #'daf/ef-themes-hl-todo-faces)
  :init
  (map! :leader
        (:prefix-map ("t" . "toggle")
         :desc "Toggle ef-themes" :mvn "t" #'ef-themes-toggle)))

(use-package! modus-themes
  :config
  (setq modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch regular 1.5)) ; absence of weight means `bold'
          (1 . (variable-pitch regular 1.4))
          (2 . (variable-pitch regular 1.3))
          (3 . (variable-pitch regular 1.2))
          (4 . (variable-pitch regular 1.1))
          (5 . (variable-pitch regular 1.1))
          (6 . (variable-pitch regular 1.1))
          (7 . (variable-pitch regular 1.1))
          (t . (variable-pitch regular 1.1)))))

(use-package! fontaine
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
  ;;
  ;; Iosevka Comfy            == monospaced, supports ligatures
  ;; Iosevka Comfy Fixed      == monospaced, no ligatures
  ;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
  ;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
  ;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
  ;; Iosevka Comfy Motion     == monospaced, supports ligatures, fancier glyphs
  ;; Iosevka Comfy Motion Duo == as above, but quasi-proportional
  (setq fontaine-presets
        '((smaller
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 90
           :variable-pitch-family "Iosevka Comfy Wide Duo")
          (small
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 100
           :variable-pitch-family "Iosevka Comfy Wide Duo")
          (regular
           :default-height 120)
          (large
           :default-weight semilight
           :default-height 150
           :bold-weight extrabold)
          (larger
           :default-weight semilight
           :default-height 160
           :bold-weight extrabold)
          (code-demo
           :default-family "Iosevka Comfy Fixed"
           :default-weight semilight
           :default-height 190
           :variable-pitch-family "Iosevka Comfy Duo"
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (legally-blind
           :default-weight semilight
           :default-height 260
           :bold-weight extrabold)
          (merriweather
           :default-family "Merriweather"
           :variable-pitch-family "Merriweather"
           :default-height 150)
          (ibm-plex-sans
           :default-family "IBM Plex Sans")
          (ibm-plex-mono
           :default-family "IBM Plex Mono")
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 120
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; Persist font configurations while switching themes (doing it with
  ;; my `modus-themes' and `ef-themes' via the hooks they provide).
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

(use-package! popper
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq popper-display-control nil)
  (setq popper-group-function #'popper-group-by-projectile)
  :init
  (map!
   :nv "C-<tab>" #'popper-cycle
   :leader
   (:prefix-map (">" . "popper")
    :desc "Toggle latest popup"   "p" #'popper-toggle-latest
    :desc "Toggle popup type"     "t" #'popper-toggle-type
    :desc "Kill last popup"       "k" #'popper-kill-latest-popup
    :desc "Cycle popups"          "n" #'popper-cycle))
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          "^\\*just\\*"
          "^\\*HTTP Response.*\\*"
          "^\\*org-now\\*"
          "^\\*eshell.*\\*$"                eshell-mode
          "^\\*vterm.*\\*$"                 vterm-mode
          "^\\*dedicated vterm.*\\*$"
          "^\\*flycheck-list-errors.*\\*$"  flycheck-error-list-mode
          "^\\*ibuffer.*\\*$"               ibuffer-mode
          "^\\*helpful-comand.*\\*$"        helpful-mode
          "^\\*helpful-variable.*\\*$"      helpful-mode
          "^\\*helpful-callable.*\\*$"      helpful-mode
          )))

(daf/repeat-map! daf-popper-cycle-repeat-map
                 '((popper-cycle            . "n")
                   (popper-cycle            . "C-<tab>")
                   (popper-toggle-type       . "T")
                   (popper-kill-latest-popup . "k")
                   (popper-toggle-latest     . "p"))
                 "Repeatable map for cycling through popups")

;; Use puni-mode globally and disable it for term-mode.
(use-package! puni
  :config

  ;; custom function from the wiki
  (defun daf/puni-kill-line ()
    "Kill a line forward while keeping expressions balanced.
If nothing can be deleted, kill backward.  If still nothing can be
deleted, kill the pairs around point."
    (interactive)
    (let ((bounds (puni-bounds-of-list-around-point)))
      (if (eq (car bounds) (cdr bounds))
          (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
            (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
        (if (eq (point) (cdr bounds))
            (puni-backward-kill-line)
          (puni-kill-line)))))
  :init
  (map!
   :map puni-mode-map
   (:prefix ("," . "puni")
    :nv "v" #'puni-expand-region
    :nv "s" #'puni-squeeze
    :nv "t" #'puni-transpose
    :nv "d" #'daf/puni-kill-line
    :nv "D" #'puni-backward-kill-line
    :nv "C" #'puni-beginning-of-sexp
    :nv "B" #'puni-beginning-of-sexp
    :nv "b" #'puni-backward-sexp
    :nv "c" #'puni-backward-sexp
    :nv "r" #'puni-forward-sexp
    :nv "f" #'puni-forward-sexp
    :nv "R" #'puni-end-of-sexp
    :nv "F" #'puni-end-of-sexp
    :nv "<" #'puni-slurp-backward
    :nv ">" #'puni-slurp-forward
    :nv "«" #'puni-slurp-backward
    :nv "»" #'puni-slurp-forward))
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(map! :map evil-window-map
      "SPC" #'rotate-layout)

(use-package! multi-vterm
  :custom
  ;; (multi-vterm-buffer-name "Terminal")
  (multi-vterm-dedicated-buffer-name "dedicated vterminal")
  (multi-vterm-dedicated-window-side 'bottom)

  :config
  (set-popup-rules!
    '(("^\\*vterm.*"
       :slot 1 :vslot -2 :actions (+popup-display-buffer-stacked-side-window-fn) :side bottom :width 0.5 :height 0.55 :quit 'other :ttl nil)
      ("^\\*dedicated vterminal.*"
       :slot 2 :vslot -2 :actions (+popup-display-buffer-stacked-side-window-fn) :side bottom :width 0.5 :height 0.55 :quit 'other :ttl nil)))

  (evil-define-key 'normal vterm-mode-map (kbd "C-d") #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ";c")  #'multi-vterm-project)
  (evil-define-key 'normal vterm-mode-map (kbd ";n")  #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ";p")  #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd ";r")  #'multi-vterm-rename-buffer)
  (evil-define-key 'normal vterm-mode-map (kbd ";;")  #'multi-vterm-dedicated-select)

  :init
  (map! :leader
        (:prefix-map ("o" . "open")
         :desc "Dedicated terminal" "t" #'multi-vterm-dedicated-toggle
         :desc "Dedicated terminal here" "T" #'multi-vterm-project))

  (map! (:map vterm-mode-map
         :localleader
         :desc "Create" "c" #'multi-vterm-project
         :desc "Rename" "r" #'multi-vterm-rename-buffer
         :desc "Select" "," #'multi-vterm-dedicated-select
         :desc "Previous" "p" #'multi-vterm-prev
         :desc "Next" "n" #'multi-vterm-next)))

(use-package! nov
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

(use-package! vundo
  :unless (modulep! +tree)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  :config
  (when (modulep! :editor evil)
    (map! :map vundo-mode-map
          [remap doom/escape] #'vundo-quit))
  :init
  (evil-define-key* 'normal 'global "U" #'vundo)
  :defer t)

(use-package! verb
  :config
  (setq verb-json-use-mode 'json-mode)
  (defun graphql-to-json (rs)
    ;; Modify RS and return it (RS is a request specification, type `verb-request-spec')
    (oset rs body (replace-regexp-in-string "\n" "" (format-message "{\"query\": \"%s\"}" (oref rs body))))
    rs)

  :init
  (map!
   :leader
   (:prefix-map ("v" . "verb")
    :desc "send request"              "V" #'verb-send-request-on-point-other-window
    :desc "send request other window" "v" #'verb-send-request-on-point-other-window-stay
    :desc "re-send request"           "r" #'verb-re-send-request
    :desc "export curl request"       "c" #'verb-export-request-on-point-curl
    (:prefix-map ("h" . "verb help")
     :desc "show sent request" "r" #'verb-show-request
     :desc "show headers"      "h" #'verb-toggle-show-headers
     :desc "show vars"         "v" #'verb-show-vars
     :desc "show logs"         "l" #'verb-show-log
     :desc "set var"           "s" #'verb-set-var
     :desc "unset vars"        "u" #'verb-unset-vars))))

(set-popup-rules!
 '(("^\\*HTTP Response.*" :quit t :side right :size 0.4 :modeline nil)))

;; (after! lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
;;                     :major-modes '(nix-mode)
;;                     :server-id 'nix)))

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(load! "book-mode")

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
  :after evil
  :config
  ;; Use right fringe
  (setq-default right-fringe-width 16)
  (setq-default evil-fringe-mark-side 'right-fringe))
:init
(global-evil-fringe-mark-mode 1)

(use-package! beacon
  :config
  (setq beacon-color 0.2)
  (setq beacon-size 20)
  (setq beacon-blink-delay 0.2)
  (setq beacon-blink-duration 0.2)
  ;; (setq beacon-blink-when-focused t)
  (setq beacon-blink-when-point-moves-vertically t)

  :init
  (beacon-mode 1))

(use-package! pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.06)
  (setq pulsar-iterations 20)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-red)


  ;; (setq pulsar-pulse-functions
  ;;     '(isearch-repeat-forward
  ;;       isearch-repeat-backward
  ;;       recenter-top-bottom
  ;;       move-to-window-line-top-bottom
  ;;       reposition-window
  ;;       bookmark-jump
  ;;       other-window
  ;;       delete-window
  ;;       delete-other-windows
  ;;       forward-page
  ;;       backward-page
  ;;       scroll-up-command
  ;;       scroll-down-command
  ;;       evil-next-match
  ;;       evil-scroll-line-to-top
  ;;       evil-scroll-line-to-center
  ;;       evil-scroll-line-to-bottom
  ;;       evil-window-move-left
  ;;       evil-window-move-right
  ;;       evil-window-move-up
  ;;       evil-window-move-down
  ;;       evil-window-left
  ;;       evil-window-right
  ;;       evil-window-up
  ;;       evil-window-down
  ;;       evil-window-vsplit
  ;;       evil-window-split
  ;;       evil-ex-search-forward
  ;;       evil-search-next
  ;;       evil-search-previous
  ;;       evil-ex-search-backward
  ;;       evil-ex-search-next
  ;;       evil-ex-search-previous
  ;;       evil-goto-line
  ;;       evil-goto-first-line
  ;;       evil-goto-last-line
  ;;       windmove-right
  ;;       windmove-left
  ;;       windmove-up
  ;;       windmove-down
  ;;       windmove-swap-states-right
  ;;       windmove-swap-states-left
  ;;       windmove-swap-states-up
  ;;       windmove-swap-states-down
  ;;       tab-new
  ;;       tab-close
  ;;       tab-next
  ;;       org-next-visible-heading
  ;;       org-previous-visible-heading
  ;;       org-forward-heading-same-level
  ;;       org-backward-heading-same-level
  ;;       outline-backward-same-level
  ;;       outline-forward-same-level
  ;;       outline-next-visible-heading
  ;;       outline-previous-visible-heading
  ;;       outline-up-heading))

  ;; (setq pulsar-pulse-functions
  ;;       '(
  ;;         evil-scroll-line-to-top
  ;;         evil-scroll-line-to-center
  ;;         evil-scroll-line-to-bottom

  ;;         ))


  ;; integration with the `consult' package:
  ;; (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  ;; (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  ;; (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  ;; (add-hook 'doom-switch-window-hook #'pulsar-pulse-line)
  ;; (add-hook 'evil-jumps-post-jump-hook #'pulsar-pulse-line)
  ;; (advice-add #'evil-window-top    :after #'pulsar-pulse-line)
  ;; (advice-add #'evil-window-middle :after #'pulsar-pulse-line)
  ;; (advice-add #'evil-window-bottom :after #'pulsar-pulse-line)
  ;; (advice-add #'what-cursor-position :after #'pulsar-pulse-line)

  ;; (add-hook! '(imenu-after-jump-hook
  ;;              better-jumper-post-jump-hook
  ;;              counsel-grep-post-action-hook
  ;;              dumb-jump-after-jump-hook)
  ;;            #'pulsar-pulse-line)


  ;; (pulsar-global-mode 1)
  )
