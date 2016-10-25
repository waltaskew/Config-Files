(add-to-list 'load-path "~/.emacs.d/elpa")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; symlinks are neat
(setq vc-follow-symlinks t)

;; evil mode
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(require 'evil)
(evil-mode 1)

; tabs are not neat
(setq-default indent-tabs-mode nil)

; auto-indent is cool
(electric-indent-mode t)

; I use version control actually
(setq make-backup-files nil)

; save cursor position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; solarized theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(setq frame-background-mode 'dark)
(load-theme 'solarized t)
(setq solarized-terminal-themed t)

;; spell check for text files
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook html-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; latex setup
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq latex-run-command "pdflatex")

;; go setup
(defun go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Display function signatures
  (go-eldoc-setup)
  ; Customize compile command to run go build, test and vet
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Load oracle
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))
(add-hook 'go-mode-hook 'go-mode-hook)

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

;; scheme setup
(setq scheme-program-name  "racket")
