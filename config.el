;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
(require 'doom-themes)
(require 'evil-lisp-state)

(defun use-prettier-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                      (expand-file-name "node_modules/prettier/bin-prettier.js"
                                        root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-js-command prettier))))
(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;;; ------------------------------------------
;;;;|               C O N F I G               |
;;;;-------------------------------------------
(setq doom-font (font-spec :family "Source Code Pro" :size 11))
(load-theme 'doom-spacegrey t)

(setq parinfer-extensions
      '(defaults
         pretty-parens
         smart-tab
         smart-yank))

;; reset few defaults
(evil-snipe-mode 0)

;; a few custom key bindings
(map! [A-backspace] 'backward-kill-word)
(map! "A-3" (lambda! (insert "£")))
(map! :leader
      :desc "lines"
      :prefix "l"
      :n "s" #'sort-lines)

;; clojure
(after! clojure-mode
  (setq evil-lisp-state-global t)
  (setq evil-lisp-state-major-modes t)
  (setq clojure-align-forms-automatically t))

(evil-lisp-state-leader "SPC k")

;; js and web stuff
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
(add-hook 'prettier-js-mode-hook #'use-prettier-from-node-modules)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;; golang
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . html-mode))
(add-hook 'go-mode-hook #'go-set-project)
(add-hook 'go-mode-hook #'whitespace-turn-off) ; theme specific
(setenv "GOPATH" "/Users/Bravi/Code/go")

;; php mode
(custom-set-variables '(phpcbf-standard "PSR2"))
(add-hook 'php-mode-hook 'phpcbf-enable-on-save)

;; python mode
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;; set up bin folder
(dolist (dir '("~/Code/go/bin" "~/.composer/vendor/bin"))
    (add-to-list 'exec-path dir))

(after! evil
  (setq evil-ex-search-highlight-all nil)
  ;(setq evil-normal-state-cursor '(box "#EEF5DB")) ; theme specific
  (setq evil-insert-state-cursor '(box "#76B8ED")))
