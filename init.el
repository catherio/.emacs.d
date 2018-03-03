;;;;;;;;;;;;;;;;; GENERAL EMACS ;;;;;;;;;;;;;;;;;

;; Packages
(add-to-list 'load-path "~/.emacs.d/elfiles")

;; from .emacs
(package-initialize)
(if (not (require 'google nil t))
    (message "`google' not found")
  (add-hook 'python-mode-hook
  (lambda ()
	(kill-local-variable 'eldoc-documentation-function)))
  (setq google-show-python-mode-warning nil)
  (require 'google-pyformat)
;; Don't do it automatically on save though. This is very bad for open-source code that isn't googley.
;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     (unless (eq major-mode 'google3-build-mode)
;;       (add-hook 'before-save-hook 'google-pyformat nil t))))

  (require 'google-flymake) ;; not sure about this either
  (require 'pylint) ;; unclear!
  )

;; Visual line mode
; (ie, turn on word-wrapping, and make C-n etc. work on the lines you *see*)
(global-visual-line-mode 1)

;; Interactively DO things
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(load "ido-better-flex-0.2.el")
(ido-better-flex/enable)

;; MELPA stuff!
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
; now do M-x package-list-packages to go get packages
; which get installed in "elpa"
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; Autocomplete
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20160416.604")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20160416.604/dict")
; I'm not totally sure how many of these load-path are necessary and how elpa really works...
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160416.604/dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; Prettier startup: close scratch, etc.
;(setq initial-buffer-choice "~/Documents/Research/today.org")
(setq-default message-log-max nil)
;(kill-buffer "*Messages*")
(setq inhibit-startup-message t)
(desktop-save-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
	("5e48fc8203ef300d49e66f79c134216c6416d2cef7c5c8b04e8fa763ede83084" "3d0d87f3f219d96208690b5e62ad40c50d06743bdc8a1ae700ab75038c491ba1" "a8fab254f55e81f3c980c7d9d8cebcc35cd92148fe46f41d3b981af4458ff787" "71b3e7ac671a5f667d241b1eac29ea9154e3d8ccc830b2f2f4b35f950adce640" "8e66e7c93094056f6dbe0f938e26839d339a67c303b0ef700cf81d720b865d56" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
	(apparel-color-blend it "#002b36" 0.25)
	(quote
	 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
	(("#073642" . 0)
	 ("#546E00" . 20)
	 ("#00736F" . 30)
	 ("#00629D" . 50)
	 ("#7B6000" . 60)
	 ("#8B2C02" . 70)
	 ("#93115C" . 85)
	 ("#073642" . 100))))
 '(hl-bg-colors
   (quote
	("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
	("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
	("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
	(bracketed-paste fill-column-indicator web-mode virtualenv python-mode json-mode jedi-direx flycheck-pyflakes flycheck-pkg-config exec-path-from-shell doremi-mac doremi-frm doremi-cmd company-go)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(py-indent-paren-spanned-multilines-p nil)
 '(py-lhs-inbound-indent 0)
 '(safe-local-variable-values
   (quote
	((eval when
		   (fboundp
			(quote rainbow-mode))
		   (rainbow-mode 1))
	 (gofmt-command . "gofmt")
	 (gofmt-command . "goimports"))))
 '(smartrep-mode-line-active-bg (apparel-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#dc322f")
	 (40 . "#c37300")
	 (60 . "#b97d00")
	 (80 . "#b58900")
	 (100 . "#a18700")
	 (120 . "#9b8700")
	 (140 . "#948700")
	 (160 . "#8d8700")
	 (180 . "#859900")
	 (200 . "#5a942c")
	 (220 . "#439b43")
	 (240 . "#2da159")
	 (260 . "#16a870")
	 (280 . "#2aa198")
	 (300 . "#009fa7")
	 (320 . "#0097b7")
	 (340 . "#008fc7")
	 (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
	(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "nil"))))
 '(org-level-1 ((t (:foreground "black" :underline t))))
 '(org-level-2 ((t (:foreground "grey30"))))
 '(org-level-3 ((t (:foreground "grey50")))))

;; Orgmode
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords
       '((sequence "TODO" "IN PROGRESS" "STARTED" "|" "DONE" "CANCELED" "")))
     (setq org-todo-keyword-faces
           '(("IN PROGRESS" . (:foreground "blue" :weight bold))
             ("STARTED" . (:foreground "deep sky blue" :weight bold))
             ("CANCELED" . (:foreground "dark grey" :weight bold))))

;;;;;;;;;;;;;;;;; PROGRAMMING MODES ;;;;;;;;;;;;;;;;;

;; Python
(setq-default tab-width 4)
(setq-default python-indent 4)
;(add-hook 'python-mode-hook
;		  (function (lambda () (setq tab-width 4
;									 python-indent 4))))
(setq py-install-directory  "~/.emacs.d/elfiles/python-mode.el-6.2.2/")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

;; Python borrowed from jhamrick

; THINGS TO REMEMBER:
; C-c ? for help (from within a python file)
; C-c ! to start a shell
; C-c | to execute region
; C-c C-c to execute whole buffer
; C-x { and } to resize split windows
; To see auto-complete dropdowns, just keep typing!
; To find more python keybindings, just go looking for
;   "define-key" in python.el and there's lots!
; M-x pdb to start pdb
; C-x <space> to set debug point at that point in file
; import pdb; pdb.set_trace();
; pdb.runcall(f, arg, etc.)

; use IPython
; (setq-default py-shell-name "ipython")
; (setq-default py-which-bufname "IPython")
; (setq py-force-py-shell-name-p t)

; fix the prompt formatting? (This didn't work)
; (setq ansi-color-for-comint-mode t)
; (setq python-shell-interpreter "ipython"
;	  python-shell-interpreter-args "--simple-prompt -i")

; switch to the interpreter after executing code
; (setq py-shell-switch-buffers-on-execute-p t)
; (setq py-switch-buffers-on-execute-p t)
; (setq py-split-windows-on-execute-p nil)

; try to automagically figure out indentation
(setq py-smart-indentation t)

; Splitting windows horizontally by default
;(setq split-width-threshold nil) ; nil for vertical, 1 for horiztontal
; (setq-default py-split-windows-on-execute-function 'split-window-horizontally)

; Jedi, which works together with auto-complete
; (add-hook 'python-mode-hook 'jedi:setup)
; (setq jedi:complete-on-dot t)                 ; optional

; https://txt.arboreus.com/2013/02/21/jedi.el-jump-to-definition-and-back.html
;; don't use default keybindings from jedi.el; keep C-. free
;; (setq jedi:setup-keys nil)
;; (setq jedi:tooltip-method nil)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (defvar jedi:goto-stack '())
;; (defun jedi:jump-to-definition ()
;;   (interactive)
;;   (add-to-list 'jedi:goto-stack
;; 			   (list (buffer-name) (point)))
;;   (jedi:goto-definition))
;; (defun jedi:jump-back ()
;;   (interactive)
;;   (let ((p (pop jedi:goto-stack)))
;; 	(if p (progn
;; 			(switch-to-buffer (nth 0 p))
;; 			(goto-char (nth 1 p))))))
;; (add-hook 'python-mode-hook
;; 		  '(lambda ()
;; 			 (local-set-key (kbd "M-.") 'jedi:jump-to-definition)
;; 			 (local-set-key (kbd "M-,") 'jedi:jump-back)
;; 			 (local-set-key (kbd "C-c d") 'jedi:show-doc)
;;              (local-set-key (kbd "C-<tab>") 'jedi:complete)))

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Matlab
 (load "matlab.el")
 (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

;; Mac stuff
(set-keyboard-coding-system nil)

;; Backups??
(setq backup-directory-alist `(("." . "~/.saves")))

;; Go!
;;(add-to-list 'load-path "~/.emacs.d/elfiles")
;;(require 'go-mode-autoloads)

(add-hook 'before-save-hook #'gofmt-before-save)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
	  (progn (forward-char 1)
			 (just-one-space 0)
			 (backward-char 1)
			 (kill-line arg))
	(kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; Pychecker
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
(add-hook 'python-mode-hook 'flycheck-mode)

;; Fill column indicator
(add-to-list 'load-path "~/.emacs.d/fill-column-indicator-1.83")
(require 'fill-column-indicator)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq fci-rule-column 79)

;; Golang from gdb

(defun golang-roots ()
  (let ((goroot (getenv "GOROOT"))
		(gopath (getenv "GOPATH"))
		(roots '()))
	(when goroot
	  (setq roots (cons goroot roots)))
	(when gopath
	  (setq roots (append (split-string gopath ":") roots)))
	roots))

(defun golang-require (path feature)
  (let ((dir (member-if (lambda (dir) (file-directory-p (concat dir "/" path))) (golang-roots))))
	(when dir
	  (add-to-list 'load-path (expand-file-name (concat (car dir) "/" path)))
	  (require feature))))

;; Bracketed paste!!
(require 'bracketed-paste)
(bracketed-paste-enable)

;;;; TODO: make this import fancier
;; (load-file "~/emacs/go.tools/refactor/rename/rename.el")
;; (load-file "~/emacs/go-mode.el/go-mode.el")

;; (golang-require "misc/emacs" 'go-mode-load)
;; (golang-require "src/github.com/nsf/gocode/emacs/" 'go-autocomplete)
;; (golang-require "src/github.com/golang/lint/misc/emacs" 'golint)
;; (let ((oracle-dir (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle")))
;;   (when (file-directory-p oracle-dir)
;; 	(load-file (concat oracle-dir "/oracle.el"))
;; 	;; (add-hook 'go-mode-hook 'go-oracle-mode)
;; 	(setq go-oracle-command "oracle"))
;;   )

;; (add-hook 'go-mode-hook 'go-eldoc-setup)
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; go get -u github.com/dougm/goflymake
;; (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;; (require 'go-flycheck)

;; (defun my-go-mode-hook ()
;;   ;; (set (make-local-variable 'company-backends) '(company-go))
;; 										; (set (make-local-variable 'company-minimum-prefix-length) 0)
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (local-set-key (kbd "C-c C-r") 'go-rename)
;;   (local-set-key (kbd "C-c C-n") 'flycheck-next-error)
;;   (local-set-key (kbd "C-c C-p") 'flycheck-previous-error)
;;   (local-set-key (kbd "C-c C-l") 'flycheck-list-errors))
;; (when (fboundp 'gofmt-before-save)
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (add-to-list 'safe-local-variable-values '(gofmt-command . "goimports"))
;;   (add-to-list 'safe-local-variable-values '(gofmt-command . "gofmt"))
;;   (let ((goimports (string-trim (shell-command-to-string "which goimports"))))
;; 	(when (not (string-equal "" goimports))
;; 	  (setq gofmt-command goimports))))

;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; (require 'company)                                   ; load company mode
;; (require 'company-go)                                ; load company mode go backend

;; (add-hook 'go-mode-hook (lambda ()
;; 						  (set (make-local-variable 'company-backends) '(company-go))
;; 						  (company-mode)))

;; ;; Stupid shit hackathon
;; (add-to-list 'load-path "~/Dropbox/Projects/apparel")
;; (add-to-list 'custom-theme-load-path "~/Dropbox/Projects/apparel")
