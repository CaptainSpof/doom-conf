;;; input/layout/config.el -*- lexical-binding: t; -*-

(add-hook! 'doom-after-modules-config-hook
  (defun +layout-init-h ()
    (load! "+ergol")))
