(add-to-list 'load-path "~/.emacs.d")

; package.el repos
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; save cursor position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; spell check for text files
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; latex setup
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq latex-run-command "pdflatex")

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
