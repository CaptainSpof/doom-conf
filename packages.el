;; -*- no-byte-compile: t; -*-
;;; .config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! verb)
;; (package! company-tabnine) ; tab9 autocomplete
(package! groovy-mode)
;; (package! systemd :pin "51c148e09a")
(package! xterm-color :pin "d53a39a5af72cd340ebf686e59a37289b4cb6e8c")
(package! magit-delta
  :recipe (:host github :repo "dandavison/magit-delta" :files ("magit-delta.el")))
(package! exec-path-from-shell)
(package! git-messenger)
(package! company-fuzzy)
;; (package! org-super-agenda)
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "474ad84a8fe5377d67ab7e491e8e68dac6e37a11")
(package! org-pretty-tags :pin "40fd72f3e701e31813f383fb429d30bb88cee769")
;; (package! burly)
;; (package! sublimity)
(package! direnv)
;; (package! kaolin-themes)
;; (package! color-theme-sanityinc-tomorrow)
;; (package! apropospriate-theme)
;; (package! solarized-theme)
;; (package! nano-emacs
;;   :recipe (:host github :repo "rougier/nano-emacs"))
(package! nov)
(package! hide-lines)
(package! org-krita
  :recipe (:host github
           :repo "lepisma/org-krita"
           :files ("resources" "resources" "*.el" "*.el")))
