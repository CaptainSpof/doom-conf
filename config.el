(setq user-full-name "Cédric Da Fonseca"
      user-mail-address "dafonseca.cedric@gmail.com")

;; (setq auto-save-default t)
;; (auto-save-visited-mode 1)
;; (global-subword-mode 1)    ; Iterate through CamelCase words
(setq delete-by-moving-to-trash t)

(setq display-line-numbers-type nil)

(map! :n [tab] (general-predicate-dispatch nil
                 (and (featurep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 #'+fold/toggle
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
      :v [tab] (general-predicate-dispatch nil
                 (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                 #'yas-insert-snippet
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item))

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Rainbow mode" :mvn "R" #'rainbow-delimiters-mode))

(map! :n "g\"" #'counsel-evil-registers)

(setq evil-escape-key-sequence "jj")

(setq scroll-margin 3)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  ;; (+ivy/switch-buffer))
  (dired-jump))

(setq +ivy-buffer-preview t)

;; (after! org (set-popup-rule! "^Capture.*\\.org$" :side 'right :size .50 :select t :vslot 2 :ttl 3))
;; (after! org (set-popup-rule! "Dictionary" :side 'bottom :size .30 :select t :vslot 3 :ttl 3))
;; (after! org (set-popup-rule! "*eww*" :side 'right :size .30 :slect t :vslot 5 :ttl 3))
;; (after! org (set-popup-rule! "*deadgrep" :side 'bottom :height .40 :select t :vslot 4 :ttl 3))
;; (after! org (set-popup-rule! "\\Swiper" :side 'bottom :size .30 :select t :vslot 4 :ttl 3))
;; (after! org (set-popup-rule! "*Ledger Report*" :side 'right :size .30 :select t :vslot 4 :ttl 3))
;; (after! org (set-popup-rule! "*xwidget" :side 'right :size .50 :select t :vslot 5 :ttl 3))
;; (after! org (set-popup-rule! "*Org Agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
;; (after! org (set-popup-rule! "*Org ql" :side 'right :size .50 :select t :vslot 2 :ttl 3))

(setq custom-theme-directory "~/.config/doom/themes")

(setq doom-theme 'modus-operandi)  ;; Light
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;; (setq doom-theme 'doom-rouille) ;; Dark
;; (setq doom-theme 'kaolin-breeze)  ;; Light
;; (setq doom-theme 'apropospriate-light)  ;; Light

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(setq doom-modeline-buffer-file-name-style 'auto)
;; (setq doom-modeline-buffer-file-name-style 'relative-to-project)
;; (setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; (custom-set-faces!
;; '(mode-line :family "JetBrains Mono" :height 0.9)
;; '(mode-line :family "SauceCodePro Nerd Font")
;; '(mode-line-inactive :family "SauceCodePro Nerd Font"))

;; (with-current-buffer (get-buffer " *Echo Area 0*")
;;    (setq-local face-remapping-alist '((default (:height 0.9) variable-pitch))))

;; (line-number
;;  :inherit 'default
;;  :foreground base5 :distant-foreground nil
;;  :weight 'normal :italic nil :underline nil :strike-through nil)

;; (setq doom-font (font-spec :family "monospace" :size 14)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 15))
(setq doom-font (font-spec :family "Attribute Mono" :size 18))
(unless (find-font doom-font)
  (setq doom-font (font-spec :family "JetBrains Mono" :size 18 :weight 'light)))
(unless (find-font doom-font)
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)))

;; (setq prettify-symbols-alist '(("null" . "∅") ("compose" . "∘") ("() =>" . "λ")))
;; (setq +pretty-code-symbols-alist '((typescript-mode ("null" . "∅") ("compose" . "∘") ("() =>" . "λ")) (emacs-lisp-mode ("lambda" . "λ")) (org-mode ("#+end_quote" . "”") ("#+END_QUOTE" . "”") ("#+begin_quote" . "“") ("#+BEGIN_QUOTE" . "“") ("#+end_src" . "«") ("#+END_SRC" . "«") ("#+begin_src" . "»") ("#+BEGIN_SRC" . "»") ("#+name:" . "»") ("#+NAME:" . "»")) (t)))
(setq +pretty-code-symbols '(:name "»" :src_block "»" :src_block_end "«" :quote "“" :quote_end "”" :lambda "λ" :composition "∘" :null "∅" :pipe "" :dot "•"))

(defvar fancy-splash-image-template
  (expand-file-name "misc/splash-images/blackhole-lines-template.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")
(defvar fancy-splash-image-nil
  (expand-file-name "misc/splash-images/transparent-pixel.png" doom-private-dir)
  "An image to use at minimum size, usually a transparent pixel")

(setq fancy-splash-sizes
      `((:height 500 :min-height 50 :padding (0 . 4) :template ,(expand-file-name "misc/splash-images/blackhole-lines-0.svg" doom-private-dir))
        (:height 440 :min-height 42 :padding (1 . 4) :template ,(expand-file-name "misc/splash-images/blackhole-lines-0.svg" doom-private-dir))
        (:height 400 :min-height 38 :padding (1 . 4) :template ,(expand-file-name "misc/splash-images/blackhole-lines-1.svg" doom-private-dir))
        (:height 350 :min-height 36 :padding (1 . 3) :template ,(expand-file-name "misc/splash-images/blackhole-lines-2.svg" doom-private-dir))
        (:height 300 :min-height 34 :padding (1 . 3) :template ,(expand-file-name "misc/splash-images/blackhole-lines-3.svg" doom-private-dir))
        (:height 250 :min-height 32 :padding (1 . 2) :template ,(expand-file-name "misc/splash-images/blackhole-lines-4.svg" doom-private-dir))
        (:height 200 :min-height 30 :padding (1 . 2) :template ,(expand-file-name "misc/splash-images/blackhole-lines-5.svg" doom-private-dir))
        (:height 100 :min-height 24 :padding (1 . 2) :template ,(expand-file-name "misc/splash-images/emacs-e-template.svg" doom-private-dir))
        (:height 0   :min-height 0  :padding (0 . 0) :file ,fancy-splash-image-nil)))

(defvar fancy-splash-sizes
  `((:height 500 :min-height 50 :padding (0 . 2))
    (:height 440 :min-height 42 :padding (1 . 4))
    (:height 330 :min-height 35 :padding (1 . 3))
    (:height 200 :min-height 30 :padding (1 . 2))
    (:height 0   :min-height 0  :padding (0 . 0) :file ,fancy-splash-image-nil))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            (symbol-name doom-theme)
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as  ;described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (beginning-of-buffer)
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :file)
                                       (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&optional frame)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)

(map! :after terraform-mode
      :map terraform-mode-map
      :localleader
      :desc "terraform format" "f" #'terraform-format-buffer)

(global-set-key (kbd "<down-mouse-2>") 'strokes-do-stroke) ; Draw strokes with RMB
(setq strokes-use-strokes-buffer nil) ; Don't draw strokes to the screen
(setq strokes-file "~/.config/doom/strokes")

;; (after! lsp-rust
;;   (setq rustic-lsp-server 'rust-analyzer)
;;   (setq lsp-rust-server 'rust-analyzer))
;; (setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)

(map!
 :i "S-SPC" #'expand-abbrev)

(defun set-local-abbrevs (abbrevs)
  "Add ABBREVS to `local-abbrev-table' and make it buffer local.
ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
The `local-abbrev-table' will be replaced by a copy with the new abbrevs added,
so that it is not the same as the abbrev table used in other buffers with the
same `major-mode'."
  (let* ((bufname (buffer-name))
         (prefix (substring (md5 bufname) 0 (length bufname)))
         (tblsym (intern (concat prefix "-abbrev-table"))))
    (set tblsym (copy-abbrev-table local-abbrev-table))
    (dolist (abbrev abbrevs)
      (define-abbrev (eval tblsym)
        (cl-first abbrev)
        (cl-second abbrev)
        (cl-third abbrev)))
    (setq-local local-abbrev-table (eval tblsym))))

(setq aw-char-position 'top-left)
(setq aw-keys '(?j ?s ?a ?d ?h ?g ?f ?k ?l))

(custom-set-faces
 '(aw-leading-char-face
   ((t
     (:foreground "red" :bold t :height 1.5)))))

(map!
 :leader
 "j" #'ace-window)

(setq evil-move-cursor-back nil)

(setq evil-kill-on-visual-paste nil)

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map "q" 'evil-quit)
  (define-key evil-motion-state-map (kbd "Q") 'evil-record-macro))

(after! evil
  (map!
   :n "z <tab>" #'evil-toggle-fold))

(after! evil
  (setq evil-ex-substitute-global t)) ; I like my s/../.. to be global by default

(after! emojify
  (setq emojify-inhibit-major-modes '(dired-mode doc-view-mode debugger-mode pdf-view-mode image-mode help-mode ibuffer-mode magit-popup-mode magit-diff-mode ert-results-mode compilation-mode proced-mode mu4e-headers-mode deft-mode groovy-mode )))

(defun find-file-right (filename)
  (interactive)
  (split-window-right)
  (other-window 1)
  (find-file filename))

(defun find-file-below (filename)
  (interactive)
  (split-window-below)
  (other-window 1)
  (find-file filename))

(after! ivy
  (ivy-set-actions
   'project-find-file
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))

  (ivy-set-actions
   'counsel-projectile-find-file
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))

  (ivy-set-actions
   'projectile-find-file
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))

  (ivy-set-actions
   'counsel-find-file
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))

  (ivy-set-actions
   'counsel-recentf
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))

  (ivy-set-actions
   'counsel-buffer-or-recentf
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))

  (ivy-set-actions
   'ivy-switch-buffer
   '(("v" find-file-right "open right")
     ("s" find-file-below "open below")))
  )

(global-set-key "\C-s" 'swiper)

(setq projectile-project-search-path '("~/Projects/"))

(after! treemacs
  (setq doom-variable-pitch-font (font-spec :family "SauceCodePro Nerd Font" :size 14))
  (setq treemacs-width 30)
  ;; (setq treemacs--width-is-locked nil) ;; FIXME treemacs doesn't care for that it seems
  (treemacs-follow-mode t))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(map! :leader
      (:prefix-map ("d" . "dired")
       :desc "Dired"                       "d" #'dired
       :desc "Dired jump to current"       "j" #'dired-jump
       :desc "fd input to dired"           "f" #'fd-dired
       :desc "Dired into project root"     "p" #'project-dired
       :desc "open dired in another frame" "D" #'dired-other-window))

(after! dired
  (map!
   :map dired-mode-map
   :n "h" #'dired-up-directory
   :n "l" #'dired-find-file
   :localleader
   :desc "toggle hidden files" "." #'dired-dotfiles-toggle))

(after! eshell
  (add-hook 'eshell-directory-change-hook #'direnv-update-directory-environment))

(map! :leader
      (:prefix-map ("e" . "eshell")
       :desc "toggle eshell popup"           "E" #'+eshell/toggle
       :desc "open eshell here"              "e" #'+eshell/here
       :desc "open eshell in project root"   "p" #'project-eshell
       :desc "eshell below"                  "s" #'+eshell/split-below
       :desc "eshell right"                  "v" #'+eshell/split-right))

(map!
 :map eshell-mode-map
 :n "gd" #'prot/eshell-find-file-at-point
 :n "gD" #'prot/eshell-find-file-at-point-other-window
 :n "go" #'prot/eshell-put-last-output-to-buffer
 ;; :i "C-SPC C-SPC" #'company-shell
 :i "C-S-SPC" #'company-shell)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; TODO: add mapping to call prot function in normal mode

(declare-function ffap-file-at-point "ffap.el")

(defmacro prot/eshell-ffap (name doc &rest body)
  "Make commands for `eshell' find-file-at-point.
NAME is how the function is called.  DOC is the function's
documentation string.  BODY is the set of arguments passed to the
`if' statement to be evaluated when a file at point is present."
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((file (ffap-file-at-point)))
       (if file
           ,@body
         (user-error "No file at point")))))

(prot/eshell-ffap
 prot/eshell-insert-file-at-point
 "Insert (cat) contents of file at point."
 (progn
   (goto-char (point-max))
   (insert (concat "cat " file))
   (eshell-send-input)))

(prot/eshell-ffap
 prot/eshell-kill-save-file-at-point
 "Add to kill-ring the absolute path of file at point."
 (progn
   (kill-new (concat (eshell/pwd) "/" file))
   (message "Copied full path of %s" file)))

(prot/eshell-ffap
 prot/eshell-find-file-at-point
 "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
 (find-file file))

(prot/eshell-ffap
 prot/eshell-find-file-at-point-other-window
 "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
 (find-file-other-window file))


(prot/eshell-ffap
 prot/eshell-file-parent-dir
 "Open `dired' with the parent directory of file at point."
 (dired (file-name-directory file)))

(defun prot/eshell-put-last-output-to-buffer ()
  "Produce a buffer with output of last `eshell' command."
  (interactive)
  (let ((eshell-output (buffer-substring-no-properties
                        (eshell-beginning-of-output)
                        (eshell-end-of-output))))
    (with-current-buffer (get-buffer-create "*last-eshell-output*")
      (erase-buffer)
      (insert eshell-output)
      (switch-to-buffer-other-window (current-buffer)))))

(after! eshell-mode
  (set-company-backend! 'eshell-mode '(company-shell company-shell-env company-files)))

(map!
 :map proced-mode-map
 :n "r" #'proced-update
 :n "R" #'proced-renice)

(after! (ivy projectile)
  ;; HACK more actions for `projectile-find-other-file'
  (require 'counsel-projectile)
  (ivy-add-actions
   'projectile-completing-read
   (cdr counsel-projectile-find-file-action)))

(after! nov
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)
;; disables highlight line in nov mode
(add-hook 'nov-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'nov-mode-hook (lambda ()
                           (make-local-variable 'scroll-margin)
                           (setq scroll-margin 1))))

(defun daf/scroll-bottom-line-to-top ()
  (interactive)
  (evil-window-bottom)
  (evil-scroll-line-to-top (line-number-at-pos)))
(defun daf/scroll-top-line-to-bottom ()
  (interactive)
  (evil-window-top)
  (evil-scroll-line-to-bottom (line-number-at-pos)))

(defun daf/indent-now ()
  (interactive)
  (evil-ex "%!sed '/^$/{N;s/\\n/\\n  /;}'"))

(defun daf/condense-now ()
  (interactive)
  (evil-ex "%!sed '/^\\n*$/!b;N;//!D;:a;z;N;//ba'"))

(map!
 (:when (featurep! :tools lookup)
  :n  "z?"   #'define-word-at-point))

(setq org-directory "~/Sync/Org/")
(setq org-agenda-files (directory-files-recursively "~/Sync/Org/" "\\.org$"))

(setq org-log-done 'time)

(after! org
  (require 'org-mouse))

(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.15)
  '(outline-2 :weight bold :height 1.10)
  '(outline-3 :weight bold :height 1.09)
  '(outline-4 :weight semi-bold :height 1.04)
  '(outline-5 :weight semi-bold :height 1.02)
  '(outline-6 :weight semi-bold)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(after! org (setq org-hide-emphasis-markers t
                  org-hide-leading-stars t
                  org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))
                  org-ellipsis "▼"))

(after! org
  (use-package org-pretty-tags
    :config
    (setq org-pretty-tags-surrogate-strings
          `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
            ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
            ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
            ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
            ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
            ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
            ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
            ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
            ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
            ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
            ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
            ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
    (org-pretty-tags-global-mode)))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        ;; org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
        org-superstar-prettify-item-bullets t ))
(after! org
  (setq org-ellipsis " ▾ "
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))

;; (when (require 'org-superstar nil 'noerror)
;;   (setq org-superstar-headline-bullets-list '("◉" "●" "○")
;;         org-superstar-item-bullet-alist nil))

(setq org-fontify-quote-and-verse-blocks t)

(after! org
  (map!
   :map org-mode-map
   :n "M-j" #'org-metadown
   :n "M-k" #'org-metadup
   :n "g TAB" #'outline-show-subtree))

(after! org
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform)))

  (add-to-list 'org-capture-templates
               '("x" "Protocol" entry (file+headline ,(concat org-directory "bookmarks.org") "Bookmarks")
                 "* %^{Title}\nSource: %u, [[%:link]]\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
  (add-to-list 'org-capture-templates
               `("L" "Protocol Link" entry (file+headline ,(concat org-directory "bookmarks.org") "Bookmarks")
                 "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] %i %?\n")))

(setq org-annotate-file-storage-file "~/Sync/Org/annotations/annotations.org")

(setq org-roam-directory "~/Sync/Org")

(setq +org-roam-open-buffer-on-find-file 'nil)

;; (use-package! org-super-agenda
;;   :commands (org-super-agenda-mode))
;; (after! org-agenda
;;   (org-super-agenda-mode))

;; (setq org-agenda-skip-scheduled-if-done t
;;       org-agenda-skip-deadline-if-done t
;;       org-agenda-include-deadlines t
;;       org-agenda-block-separator nil
;;       org-agenda-tags-column 100 ;; from testing this seems to be a good value
;;       org-agenda-compact-blocks t)

;; (setq org-agenda-custom-commands
;;       '(("o" "Overview"
;;          ((agenda "" ((org-agenda-span 'day)
;;                       (org-super-agenda-groups
;;                        '((:name "Today"
;;                           :time-grid t
;;                           :date today
;;                           :todo "TODAY"
;;                           :scheduled today
;;                           :order 1)))))
;;           (alltodo "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '((:name "Next to do"
;;                            :todo "NEXT"
;;                            :order 1)
;;                           (:name "Important"
;;                            :tag "Important"
;;                            :priority "A"
;;                            :order 6)
;;                           (:name "Due Today"
;;                            :deadline today
;;                            :order 2)
;;                           (:name "Due Soon"
;;                            :deadline future
;;                            :order 8)
;;                           (:name "Overdue"
;;                            :deadline past
;;                            :face error
;;                            :order 7)
;;                           (:name "Assignments"
;;                            :tag "Assignment"
;;                            :order 10)
;;                           (:name "Issues"
;;                            :tag "Issue"
;;                            :order 12)
;;                           (:name "Emacs"
;;                            :tag "Emacs"
;;                            :order 13)
;;                           (:name "Projects"
;;                            :tag "Project"
;;                            :order 14)
;;                           (:name "Research"
;;                            :tag "Research"
;;                            :order 15)
;;                           (:name "To read"
;;                            :tag "Read"
;;                            :order 30)
;;                           (:name "Waiting"
;;                            :todo "WAITING"
;;                            :order 20)
;;                           (:name "University"
;;                            :tag "uni"
;;                            :order 32)
;;                           (:name "Trivial"
;;                            :priority<= "E"
;;                            :tag ("Trivial" "Unimportant")
;;                            :todo ("SOMEDAY" )
;;                            :order 90)
;;                           (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(setq global-org-pretty-table-mode t)

(defun advice:org-edit-src-code (&optional code edit-buffer-name)
  (interactive)
  (my:show-headers))
(advice-add 'org-edit-src-code :before #'advice:org-edit-src-code)

(defun advice:org-edit-src-exit ()
  (interactive)
  (my:hide-headers))

(defun daf/presentation-setup ()

  (setq my:org-src-block-faces 'org-src-block-faces)
  (hide-lines-show-all)
  (setq org-src-block-faces
        '(("emacs-lisp" (:background "cornsilk"))))
  (hide-lines-matching "#\\+BEGIN_\\(SRC\\|EXAMPLE\\|VERSE\\|QUOTE\\)")
  (hide-lines-matching "#\\+END_\\(SRC\\|EXAMPLE\\|VERSE\\|QUOTE\\)")
  ;; (hide-lines-matching "#\\+attr_html")

  (advice-add 'org-edit-src-exit :after #'advice:org-edit-src-exit)
;; Display images inline
(org-display-inline-images) ;; Can also use org-startup-with-inline-images

;; Scale the text.  The next line is for basic scaling:
(setq text-scale-mode-amount 3)
(text-scale-mode 1))
(hl-line-mode -1)

;; This option is more advanced, allows you to scale other faces too
;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
;;                                    (org-verbatim (:height 1.75) org-verbatim)
;;                                    (org-block (:height 1.25) org-block))))

(defun daf/presentation-end ()
  ;; Show the mode line again

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  (text-scale-mode 0)

  (hide-lines-show-all)
  ;; If you use face-remapping-alist, this clears the scaling:
  (setq-local face-remapping-alist '((default variable-pitch default))))

;; (use-package org-tree-slide
;;   :hook ((org-tree-slide-play . efs/presentation-setup)
;;          (org-tree-slide-stop . efs/presentation-end))
;;   :custom
;;   (org-tree-slide-slide-in-effect t)
;;   (org-tree-slide-activate-message "Presentation started!")
;;   (org-tree-slide-deactivate-message "Presentation finished!")
;;   (org-tree-slide-header t)
;;   (org-tree-slide-breadcrumbs " > ")
;;   (org-image-actual-width nil))

(after! org-tree-slide
  (add-hook 'org-tree-slide-play-hook (lambda () (daf/presentation-setup)))
  (add-hook 'org-tree-slide-stop-hook (lambda () (daf/presentation-end)))

  (setq org-src-block-faces 'my:org-src-block-faces)

  (setq org-tree-slide-skip-outline-level 0))

(after! org-tree-slide
  (map!
   :map org-tree-slide-mode-map
   :n "J" #'org-tree-slide-move-next-tree
   :n "K" #'org-tree-slide-move-previous-tree
   :n "<right>" #'org-tree-slide-move-next-tree
   :n "<left>" #'org-tree-slide-move-previous-tree))

(use-package! org-krita
  :defer
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))

;; (after! magit
;;     (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))

(after! magit
  (magit-todos-mode t))

(after! git-messenger

  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(map! :leader
      (:prefix-map ("g" . "git")
       :desc  "git-messenger popup" "," #'git-messenger:popup-message
       :desc  "git buffer log"      "d" #'magit-log-buffer-file))

(setq mu4e-attachment-dir "~/Downloads/")
(set-email-account! "CaptainSpof"
                    '(
                      (user-mail-address            . "captain.spof@gmail.com")
                      (user-full-name               . "Cédric Da Fonseca")
                      (smtpmail-smtp-user           . "captain.spof@gmail.com")
                      (smtpmail-smtp-server         . "smtp.gmail.com")
                      (smtpmail-smtp-service        . 465)
                      (smtpmail-stream-type         . ssl)
                      (smtpmail-default-smtp-server . "smtp.gmail.com")
                      (message-send-mail-function   . smtpmail-send-it)
                      (smtpmail-debug-info          . t)
                      (smtpmail-debug-verbose       . t)
                      (mu4e-compose-signature       . "Captain Spof")
                      (mu4e-update-interval         . 300)
                      (mu4e-attachment-dir          . "~/Downloads/")
                      (mu4e-sent-folder             . "/[Gmail]/Sent")
                      (mu4e-drafts-folder           . "/[Gmail]/Drafts")
                      (mu4e-trash-folder            . "/[Gmail]/Trash")
                      (mu4e-refile-folder           . "/[Gmail]/All Mail")
                      )
                    t)

(after! tramp
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(after! company
  ;; (require 'company-tabnine)
  ;; (add-to-list 'company-backends #'company-tabnine)
  ;; Trigger completion immediately.
  (setq company-idle-delay 2)
  ;; (setq company-global-modes '(not eshell-mode))

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t))

;; (after! company
;;   (setq company-require-match nil)            ; Don't require match, so you can still move your cursor as expected.
;;   (setq company-tooltip-align-annotations t)  ; Align annotation to the right side.
;;   (setq company-eclim-auto-save nil)          ; Stop eclim auto save.
;;   (setq company-dabbrev-downcase nil)         ; No downcase when completion.
;; )

(setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "⑂-\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(defvar emacs-anywhere--active-markdown nil
  "Whether the buffer started off as markdown.
Affects behaviour of `emacs-anywhere--finalise-content'")

(defun emacs-anywhere--finalise-content (&optional _frame)
  (when emacs-anywhere--active-markdown
    (fundamental-mode)
    (goto-char (point-min))
    (insert "#+OPTIONS: toc:nil\n")
    (rename-buffer "*EA Pre Export*")
    (org-export-to-buffer 'gfm ea--buffer-name)
    (kill-buffer "*EA Pre Export*"))
  (gui-select-text (buffer-string)))

(define-minor-mode emacs-anywhere-mode
  "To tweak the current buffer for some emacs-anywhere considerations"
  :init-value nil
  :keymap (list
           ;; Finish edit, but be smart in org mode
           (cons (kbd "C-c C-c") (cmd! (if (and (eq major-mode 'org-mode)
                                                (org-in-src-block-p))
                                           (org-ctrl-c-ctrl-c)
                                         (delete-frame))))
           ;; Abort edit. emacs-anywhere saves the current edit for next time.
           (cons (kbd "C-c C-k") (cmd! (setq ea-on nil)
                                       (delete-frame))))
  (when emacs-anywhere-mode
    ;; line breaking
    (turn-off-auto-fill)
    (visual-line-mode t)
    ;; DEL/C-SPC to clear (first keystroke only)
    (set-transient-map (let ((keymap (make-sparse-keymap)))
                         (define-key keymap (kbd "DEL")   (cmd! (delete-region (point-min) (point-max))))
                         (define-key keymap (kbd "C-SPC") (cmd! (delete-region (point-min) (point-max))))
                         keymap))
    ;; disable tabs
    (when (bound-and-true-p centaur-tabs-mode)
      (centaur-tabs-local-mode t))))

(defun ea-popup-handler (app-name window-title x y w h)
  (interactive)
  (set-frame-size (selected-frame) 80 12)
  ;; position the frame near the mouse
  (let* ((mousepos (split-string (shell-command-to-string "xdotool getmouselocation | sed -E \"s/ screen:0 window:[^ ]*|x:|y://g\"")))
         (mouse-x (- (string-to-number (nth 0 mousepos)) 100))
         (mouse-y (- (string-to-number (nth 1 mousepos)) 50)))
    (set-frame-position (selected-frame) mouse-x mouse-y))

  (set-frame-name (concat "Quick Edit ∷ " ea-app-name " — "
                          (truncate-string-to-width
                           (string-trim
                            (string-trim-right window-title
                                               (format "-[A-Za-z0-9 ]*%s" ea-app-name))
                            "[\s-]+" "[\s-]+")
                           45 nil nil "…")))
  (message "window-title: %s" window-title)

  (when-let ((selection (gui-get-selection 'PRIMARY)))
    (insert selection))

  ;; convert buffer to org mode if markdown
  (when emacs-anywhere--active-markdown
    (shell-command-on-region (point-min) (point-max)
                             "pandoc -f markdown -t org" nil t)
    (deactivate-mark) (goto-char (point-max)))

  ;; set major mode
  (org-mode)

  (advice-add 'ea--delete-frame-handler :before #'emacs-anywhere--finalise-content)

  ;; I'll be honest with myself, I /need/ spellcheck
  ;; (flyspell-buffer)

  (evil-insert-state) ; start in insert
  (emacs-anywhere-mode 1))

(add-hook 'ea-popup-hook 'ea-popup-handler)

(after! verb
    (map! :leader
        (:prefix-map ("v" . "verb")
        :desc "send request"              "V" #'verb-send-request-on-point-other-window
        :desc "send request other window" "v" #'verb-send-request-on-point-other-window-stay
        :desc "re-send request"           "r" #'verb-re-send-request
        (:prefix-map ("s" . "verb show")
            :desc "show sent request" "r" #'verb-show-request
            :desc "show headers"      "h" #'verb-toggle-show-headers
            :desc "show vars"         "v" #'verb-show-vars
            :desc "show logs"         "l" #'verb-show-logs
        ))))

(setq +zen-window-divider-size 0
      +zen-text-scale 0.4)

(after! nov
  (map!
   :map nov-mode-map
   :n "J" #'daf/scroll-bottom-line-to-top
   :n "K" #'daf/scroll-top-line-to-bottom))
