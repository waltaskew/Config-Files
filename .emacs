(add-to-list 'load-path "~/.emacs.d")

; save cursor position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; spell check for text files
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; haskell mode configuration
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.hs$"  . haskell-mode)
                ("\\.lhs$" . literate-haskell-mode))))

(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell." t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 
   (function
    (lambda ()
      (require 'inf-haskell)
      (setq haskell-program-name "ghci")
      (setq haskell-ghci-program-name "ghci"))))

;; Marmalade packages
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Clojure marmalade package
(when (not (package-installed-p 'clojure-mode))
  (package-install 'clojure-mode))
