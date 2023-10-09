(unpin! which-key)

;; (unpin! corfu)
;; (unpin! cape)
;; (unpin! corfu-terminal)
;; (unpin! corfu-doc-terminal)
;; (unpin! corfu-doc)
;; (unpin! kind-icon)

;; (package! doom-snippets :disable t)
;; (unpin! doom-snippets)

;; (unpin! lispyville)

(package! aweshell
  :recipe (:host github
           :repo "manateelazycat/aweshell"))

(package! eat) ;; https://codeberg.org/akib/emacs-eat

(package! langtool :disable t)

(unpin! flycheck)

(unpin! flymake)

(unpin! lsp-mode)

(unpin! magit)
(package! with-editor)

;; (unpin! org) ;; REVIEW: why did I unpin it?

(package! denote) ;; https://github.com/protesilaos/denote

(package! ob-mermaid) ;; https://github.com/

(package! org-appear) ;; https://github.com/awth13/org-appear

(package! org-cv
  :recipe (:host gitlab :repo "Titan-C/org-cv"))

(package! org-gtd)

(package! org-noter) ;; https://github.com/org-noter/org-noter

(package! org-quick-peek
  :recipe (:host github :repo "alphapapa/org-now"))

(package! org-modern) ;; https://github.com/minad/org-modern

(package! org-modern-indent
  :recipe (:host github :repo "jdtsmith/org-modern-indent"))

(package! org-remark) ;; https://github.com/nobiot/org-remark

(package! image-popup
  :recipe (:host gitlab :repo "OlMon/image-popup"))

;; (package! valign)

(package! svg-tag-mode) ;; https://github.com/rougier/svg-tag-mode

(package! blamer) ;; https://github.com/Artawower/blamer.el

;; (package! burly) ;; https://github.com/alphapapa/burly.el

(package! combobulate) ;; https://github.com/mickeynp/combobulate

;; (package! elogcat) ;; https://github.com/youngker/elogcat.el

(package! jq-mode) ;; https://github.com/ljos/jq-mode

(package! just-mode)
(package! justl)

(package! languagetool) ;; https://github.com/PillFall/languagetool.el

(package! olivetti) ;; https://github.com/rnkn/olivetti

(package! focus) ;; https://github.com/larstvei/Focus

(package! logos) ;; https://github.com/protesilaos/logos

(package! magit-pretty-graph
  :recipe (:host github
           :repo "georgek/magit-pretty-graph"))

(package! ef-themes) ;; https://github.com/protesilaos/ef-themes

(package! modus-themes) ;; https://github.com/protesilaos/modus-themes

;; (package! lambda-themes
;;   :recipe (:host github
;;            :repo "lambda-emacs/lambda-themes"))

(package! fontaine) ;; https://github.com/protesilaos/fontaine

(package! popper) ;; https://github.com/karthink/popper

(package! puni) ;; https://github.com/AmaiKinono/puni

(package! rotate) ;; https://github.com/daichirata/emacs-rotate

(package! tempel)
(package! tempel-collection)

(package! multi-vterm
  :recipe (:host github :repo "gagbo/multi-vterm" :branch "display_buffer"))

(package! nov) ;; https://depp.brause.cc/nov.el/

(package! nushell-mode :recipe (:host github :repo "mrkkrp/nushell-mode")) ;; https://github.com/mrkkrp/nushell-mode

(package! vundo) ;; https://github.com/casouri/vundo

(package! verb) ;; https://github.com/federicotdn/verb

(package! evil-fringe-mark)

;; (package! pulsar)
