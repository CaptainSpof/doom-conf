;;; input/ergol/autoload/ergol.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +layout-ergol-rotate-rt-bare-keymap (keymaps)
  "Rotate [jk] with [rt] in KEYMAP."
  (evil-collection-translate-key nil keymaps
    "r" "j"
    "R" "J"
    "t" "k"
    "T" "K"
    "j" "t"
    "J" "T"
    "k" "r"
    "K" "R"
    (kbd "C-r") (kbd "C-j")
    (kbd "C-t") (kbd "C-k")
    (kbd "C-j") (kbd "C-t")
    (kbd "C-k") (kbd "C-r")
    (kbd "M-r") (kbd "M-j")
    (kbd "M-t") (kbd "M-k")
    (kbd "M-j") (kbd "M-t")
    (kbd "M-k") (kbd "M-r")
    (kbd "C-S-r") (kbd "C-S-j")
    (kbd "C-S-t") (kbd "C-S-k")
    (kbd "C-S-j") (kbd "C-S-t")
    (kbd "C-S-k") (kbd "C-S-r")
    (kbd "M-S-r") (kbd "M-S-j")
    (kbd "M-S-t") (kbd "M-S-k")
    (kbd "M-S-j") (kbd "M-S-t")
    (kbd "M-S-k") (kbd "M-S-r")))

;;;###autoload
(defun +layout-ergol-rotate-li-bare-keymap (keymaps)
  "Rotate [hl] with [li] in KEYMAP."
  (evil-collection-translate-key nil keymaps
    "l" "h"
    "L" "H"
    "i" "l"
    "I" "L"
    (kbd "C-l") (kbd "C-h")
    (kbd "C-i") (kbd "C-l")
    (kbd "M-l") (kbd "M-h")
    (kbd "M-i") (kbd "M-l")
    (kbd "C-S-l") (kbd "C-S-h")
    (kbd "C-S-i") (kbd "C-S-l")
    (kbd "M-S-l") (kbd "M-S-h")
    (kbd "M-S-i") (kbd "M-S-l"))
  (evil-collection-translate-key nil keymaps
      "h" "i"
      "H" "I"
      (kbd "C-h") (kbd "C-i")
      (kbd "M-h") (kbd "M-i")
      (kbd "C-S-h") (kbd "C-S-i")
      (kbd "M-S-h") (kbd "M-S-i")))

;;;###autoload
(defun +layout-ergol-rotate-bare-keymap (keymaps)
  "Rotate [hjkl] with [lrti] in KEYMAP."
  (+layout-ergol-rotate-li-bare-keymap keymaps)
  (+layout-ergol-rotate-rt-bare-keymap keymaps))

;;;###autoload
(defun +layout-ergol-rotate-evil-keymap ()
  "Remap evil-{normal,operator,motion,...}-state-map to be more natural with
ergol keyboard layout."
  (evil-collection-translate-key nil
    '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)
    "l" "h"
    "L" "H"
    "r" "j"
    "R" "J"
    "t" "k"
    "T" "K"
    "i" "l"
    "I" "L"
    "j" "t"
    "J" "T"
    "k" "r"
    "K" "R")
  (evil-collection-translate-key nil
    '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)
    "h" "i"
    "H" "I"))

;; TODO: Use a macro to DRY +layout-ergol-rotate-{keymaps,minor-modes}

;;;###autoload
(defun +layout-ergol-rotate-keymaps (keymaps)
  "Remap evil-collection keybinds in KEYMAPS for ergol keyboard layouts."
  (evil-collection-translate-key '(normal motion visual operator) keymaps
    "l" "h"
    "L" "H"
    "r" "j"
    "R" "J"
    "t" "k"
    "T" "K"
    "i" "l"
    "I" "L"
    "j" "t"
    "J" "T"
    "k" "r"
    "K" "R"
    (kbd "C-l") (kbd "C-h")
    (kbd "C-L") (kbd "C-H")
    (kbd "C-r") (kbd "C-j")
    (kbd "C-R") (kbd "C-J")
    (kbd "C-t") (kbd "C-k")
    (kbd "C-T") (kbd "C-K")
    (kbd "C-i") (kbd "C-l")
    (kbd "C-I") (kbd "C-L")
    (kbd "C-j") (kbd "C-t")
    (kbd "C-J") (kbd "C-T")
    (kbd "C-k") (kbd "C-r")
    (kbd "C-K") (kbd "C-R")
    (kbd "M-l") (kbd "M-h")
    (kbd "M-L") (kbd "M-H")
    (kbd "M-r") (kbd "M-j")
    (kbd "M-R") (kbd "M-J")
    (kbd "M-t") (kbd "M-k")
    (kbd "M-T") (kbd "M-K")
    (kbd "M-i") (kbd "M-l")
    (kbd "M-I") (kbd "M-L")
    (kbd "M-j") (kbd "M-t")
    (kbd "M-J") (kbd "M-T")
    (kbd "M-k") (kbd "M-r")
    (kbd "M-K") (kbd "M-R")
    :destructive nil)
  (evil-collection-translate-key '(normal motion visual operator) keymaps
    "h" "i"
    "H" "I"
    (kbd "C-h") (kbd "C-i")
    (kbd "C-H") (kbd "C-I")
    (kbd "M-h") (kbd "M-i")
    (kbd "M-H") (kbd "M-I")
    :destructive nil)

  (evil-collection-translate-key '(insert) keymaps
    (kbd "M-l") (kbd "M-h")
    (kbd "M-L") (kbd "M-H")
    (kbd "M-r") (kbd "M-j")
    (kbd "M-R") (kbd "M-J")
    (kbd "M-t") (kbd "M-k")
    (kbd "M-T") (kbd "M-K")
    (kbd "M-i") (kbd "M-l")
    (kbd "M-I") (kbd "M-L")
    (kbd "M-j") (kbd "M-t")
    (kbd "M-J") (kbd "M-T")
    (kbd "M-k") (kbd "M-r")
    (kbd "M-K") (kbd "M-R")
    :destructive nil)
  (evil-collection-translate-key '(insert) keymaps
    (kbd "M-h") (kbd "M-i")
    (kbd "M-H") (kbd "M-I")
    :destructive nil))

;;;###autoload
(defun +layout-ergol-rotate-minor-modes (modes)
  "Remap evil-collection keybinds in MODES for ergol keyboard layouts."
  (evil-collection-translate-minor-mode-key '(normal motion visual operator) modes
    "l" "h"
    "L" "H"
    "r" "j"
    "R" "J"
    "t" "k"
    "T" "K"
    "i" "l"
    "I" "L"
    "j" "t"
    "J" "T"
    "k" "r"
    "K" "R"
    (kbd "C-l") (kbd "C-h")
    (kbd "C-L") (kbd "C-H")
    (kbd "C-r") (kbd "C-j")
    (kbd "C-R") (kbd "C-J")
    (kbd "C-t") (kbd "C-k")
    (kbd "C-T") (kbd "C-K")
    (kbd "C-i") (kbd "C-l")
    (kbd "C-I") (kbd "C-L")
    (kbd "C-j") (kbd "C-t")
    (kbd "C-J") (kbd "C-T")
    (kbd "C-k") (kbd "C-r")
    (kbd "C-K") (kbd "C-R")
    (kbd "M-l") (kbd "M-h")
    (kbd "M-L") (kbd "M-H")
    (kbd "M-r") (kbd "M-j")
    (kbd "M-R") (kbd "M-J")
    (kbd "M-t") (kbd "M-k")
    (kbd "M-T") (kbd "M-K")
    (kbd "M-i") (kbd "M-l")
    (kbd "M-I") (kbd "M-L")
    (kbd "M-j") (kbd "M-t")
    (kbd "M-J") (kbd "M-T")
    (kbd "M-k") (kbd "M-r")
    (kbd "M-K") (kbd "M-R")
    :destructive nil)
  (evil-collection-translate-minor-mode-key '(normal motion visual operator) modes
    "h" "i"
    "H" "I"
    (kbd "C-h") (kbd "C-i")
    (kbd "C-H") (kbd "C-I")
    (kbd "M-h") (kbd "M-i")
    (kbd "M-H") (kbd "M-I")
    :destructive nil)

  (evil-collection-translate-minor-mode-key '(insert) modes
    (kbd "M-l") (kbd "M-h")
    (kbd "M-L") (kbd "M-H")
    (kbd "M-r") (kbd "M-j")
    (kbd "M-R") (kbd "M-J")
    (kbd "M-t") (kbd "M-k")
    (kbd "M-T") (kbd "M-K")
    (kbd "M-i") (kbd "M-l")
    (kbd "M-I") (kbd "M-L")
    (kbd "M-j") (kbd "M-t")
    (kbd "M-J") (kbd "M-T")
    (kbd "M-k") (kbd "M-r")
    (kbd "M-K") (kbd "M-R")
    :destructive nil)
  (evil-collection-translate-minor-mode-key '(insert) modes
    (kbd "M-h") (kbd "M-i")
    (kbd "M-H") (kbd "M-I")
    :destructive nil))
