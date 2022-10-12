;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Cédric Da Fonseca"
      user-mail-address "captain.spof@gmail.com")

(setq-default scroll-margin 3)

(setq gcmh-high-cons-threshold most-positive-fixnum
      max-specpdl-size 100000)
(setq gc-cons-threshold (* 100 1024 1024)) ;; REVIEW: might be overkill

(defvar daf/localleader-key "SPC ç"
  "The localleader prefix key, for major-mode specific commands.")

;; (setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "⫚-\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "⋔\\1"))))

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

(daf/repeat-map! my-window-resize-repeat-map
                 '((evil-window-increase-height . "+")
                   (evil-window-increase-height . "=")
                   (evil-window-decrease-height . "-")
                   (evil-window-increase-width . ">")
                   (evil-window-decrease-width . "<"))
                 "Repeatable map for window resizing")

(after! company
  (setq
   company-show-quick-access 'left
   company-quick-access-keys '("b" "é" "p" "o" "w")
   company-quick-access-modifier 'control
   company-dabbrev-other-buffers t)

  (set-company-backend! 'prog-mode '(company-capf company-dabbrev company-dabbrev-code)))

(map! [remap describe-bindings] #'embark-bindings
      "C-," #'embark-act)

(setq doom-theme 'ef-duo-light)
(setq ef-themes-to-toggle '(ef-duo-light ef-night))

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
  (map!
   :n "z <tab>" #'+fold/toggle))

;; (use-package! lispyville
;;   :when (modulep! :editor evil)
;;   :hook (lispy-mode . lispyville-mode)
;;   :init
;;   (setq lispyville-key-theme
;;         '((operators normal)
;;           c-w
;;           (prettify insert)
;;           (atom-movement t)
;;           slurp/barf-lispy
;;           commentary
;;           additional
;;           additional-insert))
;;   :config
;;   (lispyville-set-key-theme)
;;   (add-hook! 'evil-escape-inhibit-functions
;;     (defun +lispy-inhibit-evil-escape-fn ()
;;       (and lispy-mode (evil-insert-state-p)))))

(setq undo-fu-allow-undo-in-region 't)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; Use monospaced font faces in current buffer
(defun +vterm-mode-setup ()
  "Sets a fixed width (monospace) font in current buffer"
  (setq buffer-face-mode-face '(:family "IBM Plex Mono" :height 120))
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

(setq org-directory "~/Sync/Org/"
      org-agenda-files (directory-files-recursively "~/Sync/Org/" "\\.org$"))

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
  (use-package! org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-modern-table-vertical 1
          org-modern-table-horizontal 0.2
          org-modern-list '((43 . "➤")
                            (45 . "–")
                            (42 . "•"))
          org-modern-todo-faces

          '(("TODO" :inverse-video t :inherit org-todo)
            ("PROJ" :inverse-video t :inherit +org-todo-project)
            ("STRT" :inverse-video t :inherit +org-todo-active)
            ("[-]"  :inverse-video t :inherit +org-todo-active)
            ("HOLD" :inverse-video t :inherit +org-todo-onhold)
            ("WAIT" :inverse-video t :inherit +org-todo-onhold)
            ("[?]"  :inverse-video t :inherit +org-todo-onhold)
            ("KILL" :inverse-video t :inherit +org-todo-cancel)
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

;; (use-package! fancy-dabbrev
;;   :hook
;;   (prog-mode . fancy-dabbrev-mode)
;;   (org-mode . fancy-dabbrev-mode)
;;   :config
;;   ;; (setq fancy-dabbrev-preview-delay 0.1)
;;   (setq fancy-dabbrev-preview-context 'before-non-word)
;;   ;; Let dabbrev searches ignore case and expansions preserve case:
;;   (setq dabbrev-case-distinction nil)
;;   (setq dabbrev-case-fold-search t)
;;   (setq dabbrev-case-replace nil)
;;   (add-hook 'minibuffer-setup-hook (lambda () (fancy-dabbrev-mode 0)))
;;   (add-hook 'minibuffer-exit-hook (lambda () (fancy-dabbrev-mode 1))))

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
  :config
  (setq olivetti-body-width 90))

(use-package! logos
  :after org-mode
  :config
  ;; ;; This is the default value for the outlines:
  ;; (setq logos-outline-regexp-alist
  ;;       `((emacs-lisp-mode . "^;;;+ ")
  ;;         (org-mode . "^\\*+ +")
  ;;         (markdown-mode . "^\\#+ +")
  ;;         (t . ,(if (boundp 'outline-regexp) outline-regexp logos--page-delimiter))))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :init
  (map! :leader
        (:prefix "t"
         :desc "Logos" "L" #'logos-focus-mode))
  )

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
        '((0 . (variable-pitch regular 1.7))
          (1 . (variable-pitch regular 1.6))
          (2 . (variable-pitch regular 1.5))
          (3 . (variable-pitch regular 1.4))
          (4 . (variable-pitch regular 1.3))
          (5 . (variable-pitch regular 1.3)) ; absence of weight means `bold'
          (6 . (variable-pitch regular 1.2))
          (7 . (variable-pitch regular 1.1))
          (t . (variable-pitch regular 1.1))))
  (defun my-ef-themes-hl-todo-faces ()
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

  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)
  :init
  (map! :leader
        (:prefix-map ("t" . "toggle")
         :desc "Toggle ef-themes" :mvn "t" #'ef-themes-toggle)))

(use-package! modus-themes
  :config
  (setq modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.7))
          (1 . (variable-pitch light 1.6))
          (2 . (variable-pitch regular 1.5))
          (3 . (variable-pitch regular 1.4))
          (4 . (variable-pitch regular 1.3))
          (5 . (variable-pitch 1.2)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.1))
          (7 . (variable-pitch 1.0))
          (t . (variable-pitch 1.0)))))

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
           :default-height 100
           :variable-pitch-family "Iosevka Comfy Wide Duo")
          (small
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 120
           :variable-pitch-family "Iosevka Comfy Wide Duo")
          (regular
           :default-height 140)
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
           :default-height 140
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

(map! :map evil-window-map
      "SPC" #'rotate-layout)

(use-package! multi-vterm
  :custom
  (multi-vterm-buffer-name "Terminal")
  (multi-vterm-dedicated-window-side 'bottom)
  (multi-vterm-dedicated-buffer-name "Popup terminal")

  :config
  (map! :leader :desc "Dedicated terminal" "ot" #'multi-vterm-dedicated-toggle
        :leader :desc "Open terminal" "p!" #'multi-vterm-project)
  (map! (:map vterm-mode-map
         :localleader
         (:prefix ("m" . "Multi vterm")
          :desc "Create" "c" #'multi-vterm
          :desc "Previous" "p" #'multi-vterm-prev
          :desc "Next" "n" #'multi-vterm-next)))

  (set-popup-rules!
    '(("^\\*Terminal"
       :actions (display-buffer-in-side-window)
       :slot 2 :vslot -1 :side right :width 0.5 :quit nil)))

  (evil-define-key 'normal vterm-mode-map (kbd "C-d") #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")  #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")  #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")  #'multi-vterm-prev))

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
  (defun graphql-to-json (rs)
    ;; Modify RS and return it (RS is a request specification, type `verb-request-spec')
    (oset rs body (replace-regexp-in-string "\n" "" (format-message "{\"query\": \"%s\"}" (oref rs body))))
    rs)

  (defun json-to-json (rs)
    ;; Modify RS and return it (RS is a request specification, type `verb-request-spec')
    (message rs)
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

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(advice-add #'add-node-modules-path :override #'ignore)

(defun my-imenu-function ()
  (interactive)
  (imenu (imenu-choose-buffer-index "Jump to function: "
                                    (alist-get "Function"
                                               (imenu--make-index-alist)
                                               nil
                                               nil
                                               'string=))))
(defun my-imenu-class ()
  (interactive)
  (imenu (imenu-choose-buffer-index "Jump to class: "
                                    (alist-get "Class"
                                               (imenu--make-index-alist)
                                               nil
                                               nil
                                               'string=))))
