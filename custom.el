(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
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
 '(doom-modeline-buffer-modified ((t (:foreground "orange"))))
 '(mode-line ((t (:family "SauceCodePro Nerd Font"))))
 '(mode-line-inactive ((t (:family "SauceCodePro Nerd Font")))))
