;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Cédric Da Fonseca"
      user-mail-address "captain.spof@gmail.com")

(setq-default scroll-margin 3)

(map!
 (:map 'override
   :v "v" #'er/expand-region
   :v "V" #'er/contract-region))

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq doom-theme 'modus-vivendi)

(setq fancy-splash-image (expand-file-name "misc/splash-images/ferris.svg" doom-private-dir))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file"            :ne "f" #'find-file
        :desc "Recent files"         :ne "r" #'consult-recent-file
        :desc "Restore last session" :ne "R" #'doom/restart-and-restore
        :desc "Config dir"           :ne "C" #'doom/open-private-config
        :desc "Open config.org"      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
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

(setq doom-modeline-persp-name t)

(map!
 (:when (modulep! :tools lookup)
   :n "z?" #'define-word-at-point))

(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (project-find-file))

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

(setq org-directory "~/Sync/Org/"
      org-agenda-files (directory-files-recursively "~/Sync/Org/" "\\.org$"))

(use-package! org-mouse)

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

(use-package! ef-themes
    :config
    (setq ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t
          ef-themes-headings ; read the manual's entry of the doc string
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch regular 1.7))
            (3 . (variable-pitch regular 1.6))
            (4 . (variable-pitch regular 1.5))
            (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
            (6 . (variable-pitch 1.3))
            (7 . (variable-pitch 1.2))
            (t . (variable-pitch 1.1)))))

(use-package! modus-themes
    :config
    (setq modus-themes-variable-pitch-ui t
          modus-themes-mixed-fonts t
          modus-themes-headings ; read the manual's entry of the doc string
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch regular 1.7))
            (3 . (variable-pitch regular 1.6))
            (4 . (variable-pitch regular 1.5))
            (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
            (6 . (variable-pitch 1.3))
            (7 . (variable-pitch 1.2))
            (t . (variable-pitch 1.1)))))

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
        '((small
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 100
           :variable-pitch-family "Iosevka Comfy Duo")  ;; FIXME: couldn't build derivation with Iosevka Comfy Wide Duo
          (regular
           :default-height 120)
          (large
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
