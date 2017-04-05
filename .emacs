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

;; flycheck
(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

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

; smooth scrolling
(setq scroll-step 1)


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
iRepeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

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
           ; "go build -v && go test -v && go vet"))
           "go build -v "))
  (local-set-key (kbd "M-g") 'godef-jump)
  ; Load oracle
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))
  (global-set-key (kbd "C-c t") 'go-test-current-file)
(add-hook 'go-mode-hook 'go-mode-hook)

;; python setup
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook #'(lambda () (global-set-key (kbd "C-c t") 'pytest-directory)))

;; javacript setup
(add-hook 'js-mode-hook '(lambda ()
    (local-set-key (kbd "RET") 'newline-and-indent)))

;; haskell mode configuration
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.hs$"  . haskell-mode)
                ("\\.lhs$" . literate-haskell-mode))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'inf-haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'(lambda () (global-set-key (kbd "C-c C-c") 'haskell-process-load-file)))

;; scheme setup
(setq scheme-program-name  "racket")

;; ruby setup
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; internetting
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
