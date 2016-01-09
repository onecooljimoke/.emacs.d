;; javascript / html
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


;; coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook
          (defun coffee-mode-newline-and-indent ()
            (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
            (setq coffee-cleanup-whitespace nil)))
(custom-set-variables
 '(coffee-tab-width 2))
