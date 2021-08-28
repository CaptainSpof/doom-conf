(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("f41454aa2e27ed24eb596ac46f89bc8f7e990ea987f20da8343c5799242eb998" default))
 '(package-selected-packages '(exec-path-from-shell company-tabnine))
 '(safe-local-variable-values
   '((eval progn
           (org-babel-goto-named-src-block "startup")
           (org-babel-execute-src-block)
           (org-set-visibility-according-to-property))
     (eval org-set-visibility-according-to-property)
     (org-confirm-babel-evaluate)))
 '(warning-suppress-log-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :bold t :height 1.5))))
 '(doom-modeline-buffer-modified ((t (:foreground "orange"))))
 '(mode-line ((t (:family "SauceCodePro Nerd Font"))))
 '(mode-line-inactive ((t (:family "SauceCodePro Nerd Font"))))
 '(outline-1 ((t (:weight extra-bold :height 1.15))))
 '(outline-2 ((t (:weight bold :height 1.1))))
 '(outline-3 ((t (:weight bold :height 1.09))))
 '(outline-4 ((t (:weight semi-bold :height 1.04))))
 '(outline-5 ((t (:weight semi-bold :height 1.02))))
 '(outline-6 ((t (:weight semi-bold))))
 '(outline-8 ((t (:weight semi-bold))))
 '(outline-9 ((t (:weight semi-bold)))))
