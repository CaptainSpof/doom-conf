;; -*- no-byte-compile: t; -*-
;;; .config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (when (featurep! :completion company)
  ;; (package! company-tabnine)
  ;; (package! company-fuzzy)
;; (package! company-tabnine ; tab9 autocomplete
;;   :recipe (:host github :repo "TommyX12/company-tabnine"
;;            :files ("company-tabnine.el" "fetch-binaries.sh")))
;; )
;; (use-package! company-tabnine
;;   :after company
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))
;; )

(package! verb)
(package! company-tabnine) ; tab9 autocomplete
(package! groovy-mode)
(package! systemd :pin "51c148e09a")
(package! xterm-color :pin "d53a39a5af72cd340ebf686e59a37289b4cb6e8c")
(package! magit-delta
  :recipe (:host github :repo "dandavison/magit-delta" :files ("magit-delta.el")))
(package! exec-path-from-shell)
(package! git-messenger)
(package! company-fuzzy)
(package! org-super-agenda :pin "3264255989021b8563ea42b5d26acbc2a024f14d")
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "474ad84a8fe5377d67ab7e491e8e68dac6e37a11")
(package! org-pretty-tags :pin "40fd72f3e701e31813f383fb429d30bb88cee769")
;; (package! burly)
;; (package! sublimity)
(package! direnv)
