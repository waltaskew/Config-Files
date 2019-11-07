(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

; symlinks are neat
(setq vc-follow-symlinks t)

;; evil mode
(require 'evil)
(evil-mode 1)
; do not treat underscores as word boundaries
(modify-syntax-entry ?_ "w")

; auto-indent is cool
(electric-indent-mode t)

; use spaces for tabs always
(setq-default indent-tabs-mode nil)

; I use version control actually
(setq make-backup-files nil)

; save cursor position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

; smooth scrolling
(setq scroll-step 1)

; as decreed by the gods of unix
(setq require-final-newline t)
(setq mode-require-final-newline t)


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
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
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (global-set-key (kbd "C-c t") 'go-test-current-file))
(add-hook 'go-mode-hook 'go-mode-hook)

;; python setup
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook #'(lambda ()
				(global-set-key (kbd "C-c t")
						'pytest-directory)))
(add-hook 'python-mode-hook #'(lambda ()
				(global-set-key (kbd "C-c C-k")
						#'(lambda () (interactive)
						    (run-python "pyspark")))))

(require 'flycheck-cython)
(add-hook 'cython-mode-hook 'flycheck-mode)
(add-hook 'cython-mode-hook 'flycheck-prog-mode)
(add-hook 'cython-mode-hook #'(lambda ()
				(global-set-key (kbd "C-c t")
						'pytest-directory)))

;;; scala setup
(setq ensime-startup-notification nil)

;; load the ensime lisp code...
(add-to-list 'load-path "ENSIME_ROOT/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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

;; markdown setup
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'gfm-mode-hook 'flyspell-mode)

;; ruby setup
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; internetting
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode web-mode use-package pytest markdown-mode magit haskell-mode gotest go-guru go-eldoc flycheck-pyflakes flycheck-cython evil ensime dockerfile-mode cython-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
