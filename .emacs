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
(save-place-mode)

; smooth scrolling
(setq scroll-step 1)

; as decreed by the gods of unix
(setq require-final-newline t)
(setq mode-require-final-newline t)

; keep previous M-x shell behaviour of opening in current window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

; everyone loves a winner
(winner-mode)

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
(require 'flycheck)
(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-imports"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-flake8 'python-mypy t)

(defun python-key-bindings ()
  (local-set-key "\C-ca" 'pytest-all)
  (local-set-key "\C-cm" 'pytest-module)
  (local-set-key "\C-c." 'pytest-one)
  (local-set-key "\C-cd" 'pytest-directory)
  (local-set-key "\C-cpa" 'pytest-pdb-all)
  (local-set-key "\C-cpm" 'pytest-pdb-module)
  (local-set-key "\C-cp." 'pytest-pdb-one))

(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook #'python-key-bindings)
(add-hook
 'python-mode-hook
 #'(lambda ()
     (global-set-key (kbd "C-c C-k")
                     #'(lambda () (interactive)
                         ; (let ((python-shell-prompt-detect-enabled nil))
                         (let ((python-shell-prompt-detect-failure-warning nil))
                           (run-python "pyspark"))))))

(require 'flycheck-cython)
(add-hook 'cython-mode-hook 'flycheck-mode)
(add-hook 'cython-mode-hook 'flycheck-prog-mode)
(add-hook 'python-mode-hook #'python-key-bindings)
(add-hook 'python-mode-hook 'python-black-on-save-mode)

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'js2-mode-hook (lambda ()
                           (flycheck-mode t)
                           (when (executable-find "eslint")
                             (flycheck-select-checker 'javascript-eslint))))

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
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook
          #'(lambda ()
              ; (setq indent-tabs-mode nil)
              ; (setq tab-width 2)
              (setq evil-shift-width 2)))

;; saltstack setup
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; terraform setup
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; internetting
(require 'web-mode)
; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(js2-mode python-black ess inf-ruby ruby-end terraform-mode salt-mode anaconda-mode yaml-mode web-mode use-package pytest markdown-mode magit haskell-mode gotest go-guru go-eldoc flycheck-pyflakes flycheck-cython evil ensime dockerfile-mode cython-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
