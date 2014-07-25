(add-to-list 'load-path "~/.emacs.d")

; package.el repos
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; I use version control actually
(setq make-backup-files nil)

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

;; javacript setup
(add-hook 'js-mode-hook '(lambda ()
    (local-set-key (kbd "RET") 'newline-and-indent)))

;; haskell mode configuration
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.hs$"  . haskell-mode)
                ("\\.lhs$" . literate-haskell-mode))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
