;; -*- no-byte-compile: t; -*-
;;; .config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;;
;; disabled packages
;;

(package! evil-escape :disable t)
(package! evil-snipe :disable t)

;;
;; cool new modes
;;

(package! nov)
(package! verb)

;;
;; org-mode
;;

(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "474ad84a8fe5377d67ab7e491e8e68dac6e37a11")
(package! org-pretty-tags :pin "40fd72f3e701e31813f383fb429d30bb88cee769")
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "474ad84a8fe5377d67ab7e491e8e68dac6e37a11")
(package! org-pretty-tags :pin "40fd72f3e701e31813f383fb429d30bb88cee769")

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(use-package! org-ol-tree
  :commands org-ol-tree)

(package! org-krita
  :recipe (:host github
           :repo "lepisma/org-krita"
           :files ("resources" "resources" "*.el" "*.el")))

;; for org-tree-slide
(package! hide-lines)
(package! olivetti)

(package! xterm-color :pin "d53a39a5af72cd340ebf686e59a37289b4cb6e8c")
(package! exec-path-from-shell)
(package! git-messenger)
;; (package! direnv)

;; tree-sitter & completion
;; (package! tree-sitter)
;; (package! tree-sitter-langs)
(package! company-fuzzy)

;;
;; currently trying
;;

(package! svg-tag-mode)
(package! s3ed)
(package! org-noter)
(package! zoom)
(package! jq-mode)
(package! speed-type)
;; (package! kubel)
(package! kubernetes)
(package! kubernetes-evil)
(package! demo-it)



;;
;; stuff I tried
;;

;; (package! magit-delta
;;   :recipe (:host github :repo "dandavison/magit-delta" :files ("magit-delta.el")))
;; (package! company-tabnine) ; tab9 autocomplete
;; (package! kaolin-themes)
;; (package! color-theme-sanityinc-tomorrow)
;; (package! apropospriate-theme)
;; (package! solarized-theme)
;; (package! nano-emacs
;;   :recipe (:host github :repo "rougier/nano-emacs"))
;; (package! systemd :pin "51c148e09a")
;; (package! org-super-agenda)
;; (package! burly)
;; (package! sublimity)
;; (package! nano-theme
;;   :recipe (:host github :repo "rougier/nano-theme"))
;; :repo "rougier/svg-tag-mode"
;;                        :fetcher github
;;                        :files ("svg-tag-mode.el")

;;
;; stuff I don't want to deal with anymore
;;

;; (package! groovy-mode)
