;;;;;;;;;;;;;;;;; GENERAL EMACS ;;;;;;;;;;;;;;;;;

;; Packages
(add-to-list 'load-path "~/.emacs.d/elfiles")

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
 '(py-indent-paren-spanned-multilines-p nil)
 '(py-lhs-inbound-indent 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; (setq py-split-windows-on-execute-p nil)

; try to automagically figure out indentation
(setq py-smart-indentation t)

; Splitting windows horizontally by default
;(setq split-width-threshold nil) ; nil for vertical, 1 for horiztontal
(setq-default py-split-windows-on-execute-function 'split-window-horizontally)

; Jedi, which works together with auto-complete
;     to add documentation to the dropdown menus
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)                 ; optional

; jedi-direx to show python code as a tree
;(require 'jedi-direx) ; added to try to make this work
;(setq-default jedi-direx:hide-imports t)
;(eval-after-load "python"
;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
; ^^^^^ didn't work, trying something else
;(global-set-key [(control c) (t)]  'jedi-direx:switch-to-buffer)
;(add-hook 'jedi-mode-hook 'jedi-direx:setup)

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

;;;; TODO: make this import fancier
;; (load-file "~/emacs/go.tools/refactor/rename/rename.el")
;; (load-file "~/emacs/go-mode.el/go-mode.el")

;; (golang-require "misc/emacs" 'go-mode-load)
(golang-require "src/github.com/nsf/gocode/emacs/" 'go-autocomplete)
(golang-require "src/github.com/golang/lint/misc/emacs" 'golint)
(let ((oracle-dir (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle")))
  (when (file-directory-p oracle-dir)
	(load-file (concat oracle-dir "/oracle.el"))
	;; (add-hook 'go-mode-hook 'go-oracle-mode)
	(setq go-oracle-command "oracle"))
  )

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; go get -u github.com/dougm/goflymake
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
(require 'go-flycheck)

(defun my-go-mode-hook ()
  ;; (set (make-local-variable 'company-backends) '(company-go))
										; (set (make-local-variable 'company-minimum-prefix-length) 0)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-r") 'go-rename)
  (local-set-key (kbd "C-c C-n") 'flycheck-next-error)
  (local-set-key (kbd "C-c C-p") 'flycheck-previous-error)
  (local-set-key (kbd "C-c C-l") 'flycheck-list-errors))
(when (fboundp 'gofmt-before-save)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'safe-local-variable-values '(gofmt-command . "goimports"))
  (add-to-list 'safe-local-variable-values '(gofmt-command . "gofmt"))
  (let ((goimports (string-trim (shell-command-to-string "which goimports"))))
	(when (not (string-equal "" goimports))
	  (setq gofmt-command goimports))))

;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)

(require 'company)                                   ; load company mode
(require 'company-go)                                ; load company mode go backend

(add-hook 'go-mode-hook (lambda ()
						  (set (make-local-variable 'company-backends) '(company-go))
						                            (company-mode)))
