;; (unpin! which-key)

;; (unpin! lispyville)

(package! langtool :disable t)

;; (unpin! flycheck)

(unpin! lsp-mode)

(package! org-appear) ;; https://github.com/awth13/org-appear

(package! org-modern)

(package! org-modern-indent
  :recipe (:host github :repo "jdtsmith/org-modern-indent"))

(package! image-popup
  :recipe (:host gitlab :repo "OlMon/image-popup"))

(package! blamer) ;; https://github.com/Artawower/blamer.el

(package! elogcat)

;; (package! fancy-dabbrev)

(package! languagetool) ;; https://github.com/PillFall/languagetool.el

(package! olivetti)

(package! focus)

(package! logos)

(package! magit-pretty-graph
  :recipe (:host github
           :repo "georgek/magit-pretty-graph"))

(package! ef-themes)

(package! modus-themes)

(package! fontaine)

(package! rotate)

(package! multi-vterm
  :recipe (:host github :repo "gagbo/multi-vterm" :branch "display_buffer"))

(package! nov)

(package! vundo)

(package! verb)

;; (package! ob-typescript)

(package! nano-theme)
