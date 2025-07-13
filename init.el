;;; -*- lexical-binding: t -*-

(if (version< emacs-version "29.3")
    ;; Code to execute if Emacs is too old
    (message "Your Emacs version (%s) is too old. Please upgrade." emacs-version))

;;
;; use-package
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; unnecessary in Emacs 27+
;;(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; unnecessary with use-package
;; (require 'use-package-ensure)
(setq use-package-always-ensure t)

;; If I need to profile my Emacs startup...
;; (use-package benchmark-init
;;   :init (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))

;;
;; Still a fan of Solarized...
;;
(use-package solarized-theme
  :if (display-graphic-p)
  :config (load-theme 'solarized-dark t))

;;
;; Changes all yes/no questions to y/n type
;;
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; Get focus on startup
;;
(select-frame-set-input-focus (selected-frame))

;;
;; And ligatures
;;
(use-package ligature
  :if (display-graphic-p)
  :config
  (ligature-set-ligatures
   't
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

;;
;; EditorConfig is awesome
;;
(use-package editorconfig
  :config (editorconfig-mode 1))

;;
;; Autocomplete
;;
(use-package company
  :bind (("C-SPC" . company-complete))
  :hook (after-init . global-company-mode))

;;
;; Even more autocomplete
;;
(use-package ido-completing-read+
  :init
  ;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
  (setq ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ;; Includes buffer names of recently opened files, even if they're not open now.
        ido-use-virtual-buffers t)
  :config
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-mode t)
  (ido-everywhere t))

;;
;; MOAR AUTO COMPLETE
;;
(use-package amx
  :bind ("M-x" . amx))

;;
;; Aggressively indent everything. Might need to scale this back over time.
;;
(use-package aggressive-indent
  :init (global-aggressive-indent-mode 1)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'nix-mode)
  ;; jsonnet-mode's formatting differs from jsonnetfmt command
  (add-to-list 'aggressive-indent-excluded-modes 'jsonnet-mode))

;;
;; Power tools for parenthesis.
;;
(use-package paredit
  :hook ((clojure-mode
          cider-repl-mode
          emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode)
         . enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)

;;
;; Make the parenthesis pretty.
;;
(use-package rainbow-delimiters)

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;
;; Draw pretty git status in the gutter.
;;
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;;
;; Keep the content in the scratch buffer, because we can.
;;
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;;
;; Allow emacs to use itself as an editor in its own shells.
;;
(use-package with-editor
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . with-editor-export-editor))

;;
;; Git integration; although I'm too much of a shell user to really like it.
;;
(use-package magit
  :bind ("C-x g" . magit-status))

;;
;; Various modes
;;
(use-package typescript-mode
  :mode "\\.tsx?\\'")
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
(use-package gitattributes-mode
  :ensure git-modes
  :defer t)
(use-package gitconfig-mode
  :ensure git-modes
  :defer t)
(use-package gitignore-mode
  :ensure git-modes
  :defer t)
(use-package js2-mode
  :mode ("\\.[cm]?js\\'"))
(use-package markdown-mode
  :mode "\\.md\\'"
  :config (setq markdown-asymmetric-header t))
(use-package markdown-toc
  :defer t)
(use-package php-mode
  :mode "\\.php\\'")
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;
;; Trying server mode again
;;
(server-start)

;;
;; Command-click since I don't have three button on my trackpad
;;
(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))


;; Wait until the next time I do Clojure for these
;;  - cider clojure-mode clojure-mode-extra-font-locking

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups")))
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(cursor-type 'bar)
 '(delete-selection-mode t)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'markdown-mode)
 '(initial-scratch-message nil)
 '(mouse-yank-at-point t)
 '(org-fold-catch-invisible-edits 'show)
 '(org-support-shift-select t)
 '(package-selected-packages nil)
 '(require-final-newline t)
 '(safe-local-variable-values '((js2-basic-offset . 2)))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-mode t nil nil "so mini-buffer commands are saved between session")
 '(select-enable-primary t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "nil" :slant normal :weight regular :height 120 :width normal)))))
