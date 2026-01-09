;;; input/ergol/+ergol.el -*- lexical-binding: t; -*-

;; Highlight non breaking spaces as error in prog modes only
;; REVIEW `nobreak-char-display' is defined in xdisp.c; will that work in non-X
;;        builds? From early observations in sway+pgtk, it does not.
(setq nobreak-char-display t)
(set-face-attribute 'nobreak-space nil :underline t)

;;
;;; Initializers

;; TODO Separate each package into their own hook, so users can
;;      enable/disable/add their own per-package remappings.

(defun +layout-remap-keys-for-ergol-h ()
  (setq avy-keys '(?a ?s ?e ?n ?, ?l ?r ?t ?i ?u)
        lispy-avy-keys '(?a ?s ?e ?n ?, ?l ?r ?t ?i ?v ?h ?c ?q ?o ?p ?w ?j ?d ?f ?g ?x))

  ;; :ui window-select settings, ignoring +numbers flag for now
  (after! ace-window
    (setq aw-keys '(?a ?s ?e ?n ?, ?l ?r ?t ?i ?u)))
  (after! switch-window
    (setq switch-window-shortcut-style 'qwerty
          switch-window-qwerty-shortcuts '("a" "s" "e" "n" "," "l" "r" "t" "i")))

  (map! :leader
        (:prefix-map ("c" . "code")
         :desc "Jump to documentation"     "T"  #'+lookup/documentation)))

(defun +layout-remap-evil-keys-for-ergol-h ()
  ;; "ts" would be a little too common for an evil escape sequence
  (setq evil-escape-key-sequence "gq")
  (setq evil-markdown-movement-bindings '((up . "t")
                                          (down . "r")
                                          (left . "l")
                                          (right . "i"))
        evil-org-movement-bindings '((up . "t")
                                     (down . "r")
                                     (left . "l")
                                     (right . "i")))
  (+layout-ergol-rotate-rt-bare-keymap '(read-expression-map))
  (+layout-ergol-rotate-bare-keymap '(evil-window-map))
  (+layout-ergol-rotate-evil-keymap)
  ;; Remap the visual-mode-map bindings if necessary
  ;; See https://github.com/emacs-evil/evil/blob/7d00c23496c25a66f90ac7a6a354b1c7f9498162/evil-integration.el#L478-L501
  ;; Using +layout-ergol-rotate-keymaps is impossible because `evil-define-minor-mode-key' doesn't
  ;; provide an actual symbol to design the new keymap with, and instead stuff the keymap in
  ;; an auxiliary-auxiliary `minor-mode-map-alist'-like variable.
  (after! evil-integration
    (when evil-respect-visual-line-mode
      (map! :map visual-line-mode-map
            :m "r"  #'evil-next-visual-line
            ;; _Not_ remapping gj and gk because they aren't remapped
            ;; consistently across all Emacs.
            :m "t"  #'evil-previous-visual-line
            :m "$"  #'evil-end-of-visual-line
            :m "g$" #'evil-end-of-line
            :m "V"  #'evil-visual-screen-line)))

  (map! :i "C-r" #'+default-newline
        (:when (modulep! :editor multiple-cursors)
         :prefix "gz"
         :nv "r"   #'evil-mc-make-cursor-move-next-line
         :nv "t"   #'evil-mc-make-cursor-move-prev-line
         ;; The old toggle mapping (t) is made available both on "T" for
         ;; mnemonics and "j" as a "classic" rotation
         :nv "T"   #'+multiple-cursors/evil-mc-toggle-cursors
         :nv "j"   #'+multiple-cursors/evil-mc-toggle-cursors))
  (after! treemacs
    (+layout-ergol-rotate-rt-bare-keymap '(evil-treemacs-state-map)))
  (after! (:or helm ivy vertico icomplete)
    (+layout-ergol-rotate-keymaps
     '(minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map
       read-expression-map))
    (+layout-ergol-rotate-bare-keymap
     '(minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map
       read-expression-map)))
  (after! ivy
    (+layout-ergol-rotate-bare-keymap '(ivy-minibuffer-map ivy-switch-buffer-map))
    (+layout-ergol-rotate-keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)))
  (after! helm
    (+layout-ergol-rotate-bare-keymap '(helm-map))
    (+layout-ergol-rotate-keymaps '(helm-map)))
  (after! helm-rg
    (+layout-ergol-rotate-bare-keymap '(helm-rg-map))
    (+layout-ergol-rotate-keymaps '(helm-rg-map)))
  (after! helm-files
    (+layout-ergol-rotate-bare-keymap '(helm-read-file-map))
    (+layout-ergol-rotate-keymaps '(helm-read-file-map)))
  (after! company
    (+layout-ergol-rotate-bare-keymap '(company-active-map company-search-map)))
  (after! evil-snipe
    (+layout-ergol-rotate-keymaps
     '(evil-snipe-local-mode-map evil-snipe-override-local-mode-map)))
  (after! eshell
    (add-hook 'eshell-first-time-mode-hook (lambda () (+layout-ergol-rotate-keymaps '(eshell-mode-map))) 99))
  (after! lsp-ui
    (+layout-ergol-rotate-rt-bare-keymap '(lsp-ui-peek-mode-map)))
  (after! org
    (defadvice! doom-ergol--org-completing-read (&rest args)
      "Completing-read with SPACE being a normal character, and C-c mapping left alone."
      :override #'org-completing-read
      (let ((enable-recursive-minibuffers t)
            (minibuffer-local-completion-map
             (copy-keymap minibuffer-local-completion-map)))
        (define-key minibuffer-local-completion-map " " 'self-insert-command)
        (define-key minibuffer-local-completion-map "?" 'self-insert-command)
        (apply #'completing-read args)))
    (+layout-ergol-rotate-bare-keymap '(org-capture-mode-map)))
  (after! (evil org evil-org)
    ;; FIXME: This map! call is being interpreted before the
    ;;   map! call in (use-package! evil-org :config) in modules/lang/org/config.el
    ;;   Therefore, this map! needs to be reevaluated to have effect.
    ;;   Need to find a way to call the code below after the :config block
    ;;   in :lang org code

    ;; Direct access for "unimpaired" like improvements
    (map! :map evil-org-mode-map
          ;; evil-org-movement bindings having "r" means
          ;; C-r gets mapped to `org-shiftup' in normal and insert state.
          :ni "C-r" nil))
  (after! (evil org evil-org-agenda)
    (+layout-ergol-rotate-bare-keymap '(org-agenda-keymap))
    (+layout-ergol-rotate-keymaps '(evil-org-agenda-mode-map)))
  (after! (evil info)
    ;; Without this, "t" stays mapped to 'Info-top-node (in the "global"
    ;; `Info-mode-map') and takes precedence over the evil command to go up one
    ;; line (remapped in `Info-mode-normal-state-map').
    (map! :map Info-mode-map
          "t" nil))

  
  ;; Start of the Magit zone
  ;;
  ;; The magit zone needs to be special because evil-collection and magit and
  ;; this module don't fully agree on the order with which features, keymaps,
  ;; overriding keymaps, and evil-collection-setup-hook are run.
  ;;
  ;; This is _probably_ more complex than what it needs to be, but the
  ;; interactions between all packages are so hard to track that a
  ;; trial-and-error approach has been used to arrive at this result.
  (after! (evil magit-section)
    (+layout-ergol-rotate-rt-bare-keymap
     '(magit-section-mode-map)))
  (after! (evil magit-log)
    (+layout-ergol-rotate-keymaps
     '(magit-log-read-revs-map
       magit-log-mode-map
       ;; NOTE: magit-cherry-mode could be moved of magit-log anyday, be
       ;; careful
       magit-cherry-mode-map)))
  (after! (evil magit-reflog)
    (+layout-ergol-rotate-keymaps
     '(magit-reflog-mode-map)))
  (after! (evil magit-status)
    (+layout-ergol-rotate-keymaps
     '(magit-status-mode-map
       magit-staged-section-map
       magit-unstaged-section-map
       magit-untracked-section-map)))
  (after! (evil magit-diff)
    (+layout-ergol-rotate-keymaps
     '(magit-diff-mode-map
       magit-diff-section-base-map)))
  (after! (evil magit-process)
    (+layout-ergol-rotate-keymaps
     '(magit-process-mode-map)))
  (after! (evil magit-refs)
    (+layout-ergol-rotate-keymaps
     '(magit-refs-mode-map)))
  (after! (evil magit-blob)
    (+layout-ergol-rotate-keymaps
     '(magit-blob-mode-map)))
  (after! (evil magit)
    (+layout-ergol-rotate-rt-bare-keymap
     '(magit-mode-map))
    (map! :map magit-mode-map
          "t" nil)
    (+layout-ergol-rotate-keymaps
     '(magit-mode-map)))
  ;; End of the Magit zone
  

  (after! evil-easymotion
    (+layout-ergol-rotate-bare-keymap '(evilem-map))))


;;
;;; Bootstrap

(+layout-remap-keys-for-ergol-h)
(when (modulep! :editor evil)
  (+layout-remap-evil-keys-for-ergol-h)
  (add-hook! 'evil-collection-setup-hook
    (defun +layout-ergol-rotate-evil-collection-keymap (_mode mode-keymaps &rest _rest)
      (+layout-ergol-rotate-keymaps mode-keymaps))))
