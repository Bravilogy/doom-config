;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
(require 'doom-themes)

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
(load-theme 'doom-nord t)

(map! [A-backspace] 'backward-kill-word)
(map! "A-3" (lambda! (insert "£")))
(map! :leader
      :desc "lines"
      :prefix "l"
      :n "s" #'sort-lines)

(add-to-list 'exec-path "~/Code/go/bin")

(evil-snipe-mode 0)

;; use eslint and prettier from node modules
(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
(add-hook 'prettier-js-mode-hook #'use-prettier-from-node-modules)

;; attach prettier-js to different modes
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;; clojure
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; golang
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . html-mode))
(add-hook 'go-mode-hook #'go-set-project)
(add-hook 'go-mode-hook #'whitespace-turn-off)

(after! evil
  (setq evil-ex-search-highlight-all nil)
  ;(setq evil-normal-state-cursor '(box "#EEF5DB"))
  (setq evil-insert-state-cursor '(box "#76B8ED")))
