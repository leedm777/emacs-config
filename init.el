(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(delete-selection-mode t)
 '(editorconfig-mode t)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-git-gutter-mode t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(markdown-asymmetric-header t)
 '(package-selected-packages
   (quote
    (aggressive-indent auto-complete cider clojure-mode clojure-mode-extra-font-locking dockerfile-mode editorconfig git-commit git-gutter git-gutter-fringe gitignore-mode ido-ubiquitous ledger-mode magit magit-filenotify markdown-mode markdown-toc paredit projectile puppet-mode rainbow-delimiters smex solarized-theme tagedit terraform-mode with-editor yaml-mode)))
 '(require-final-newline t)
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-mode t nil nil "so mini-buffer commands are saved between session")
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;
;;; CSS
;;;

;; CSS is regular enough that aggressive indenting is okay
(add-hook 'css-mode-hook #'aggressive-indent-mode)

;;;
;;; Lisps
;;;

;; There are a lot of them
(defvar lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    eval-expression-minibuffer-setup-hook
    ielm-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook))

(dolist (h lisp-mode-hooks)
  ;; lisp mode with superpowers
  (add-hook h #'paredit-mode)
  ;; such regular languages can be aggressively indented
  (add-hook h #'aggressive-indent-mode)
  ;; rainbows AND UNICORNS
  (add-hook h #'rainbow-delimiters-mode))

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;
;;; Trailing whitespace
;;;

;; Usually I care about trailing whitespace, but only on modes which are file
;; editing modes
(defvar interactive-mode-hooks
  '(shell-mode-hook
    term-mode-hook
    compilation-mode-hook
    cidr-repl-mode-hook))
(dolist (h interactive-mode-hooks)
  (add-hook h (lambda () (set 'show-trailing-whitespace nil))))

;;
;; Navigation
;;

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)

;;
;; ui
;;

;; Solarized FTW
(load-theme 'solarized-dark t)

;; Maybe the ligatures will work someday...
;; Using default-frame-alist so it will work with emacsclient
(setq default-frame-alist '((font . "Fira Code-12")))
;; (set-default-font "Fira Code-12")

;;
;; editing
;;

;; Hippied-expand goodness
(global-set-key "\M- " 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; On macOS, C-left and C-right are used by the OS
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "<s-right>") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "<s-left>") 'paredit-forward-barf-sexp)))

;; On macOS, C-up and C-down are used by the OS
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (define-key cider-repl-mode-map (kbd "<s-up>") 'cider-repl-backward-input)
            (define-key cider-repl-mode-map (kbd "<s-down>") 'cider-repl-forward-input)))

;; On macOS, C-up and C-down are used by the OS
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "<s-up>") 'comint-previous-input)
            (define-key shell-mode-map (kbd "<s-down>") 'comint-next-input)))

;;
;; Clojure
;;
(require 'clojure-mode-extra-font-locking)

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;;
;; JavaScript
;;
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

;;
;; with-editor
;;
(require 'with-editor)
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'term-mode-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
