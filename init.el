;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)

;;(add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; probably not needed if using helm, keep it for the time being
    ;; also commented out custimizations/navigation.el below
    ;; which mostly setup ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    ;; smex

    ;; edit html tags like sexps
    tagedit))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
;; (add-to-list 'load-path "~/.emacs.d/vendor")


;; time for some fun with use-package
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; set up hosted packages
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode 1))

(use-package auto-complete
  :ensure t
  :init
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
  (ac-config-default)
  :config
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

(use-package avy
  :ensure t)

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "a l" 'avy-goto-line
   "a w" 'avy-goto-word-1
   "k" 'kill-buffer
   "m" 'magit-status
   "e f" 'find-tag
   "e l" 'list-tags))

(use-package evil
  :ensure t
  :config 
  (require 'evil)
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "q") nil)
  (evil-leader/set-key
    "c j" 'cider-jack-in
    "c k" 'cider-load-buffer 
    "c q" 'cider-quit
    "s" 'evil-write
    "q" 'evil-quit
    "w s h" 'evil-window-split
    "w s v" 'evil-window-vsplit
    "w d w" 'delete-window
    "w d o" 'delete-other-windows
    "w m h" 'evil-window-left
    "w m l" 'evil-window-right
    "w m j" 'evil-window-down
    "w m k" 'evil-window-up
    "u s" 'speedbar-frame-mode)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-insert-state-cursor '("orange" bar)))

(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings ";"))

(use-package evil-matchit
  :ensure t
  :config
  (require 'evil-matchit)
  (global-evil-matchit-mode 1))

(use-package expand-region
  :ensure t
  :config 
  (global-set-key (kbd "C-=") 'er/expand-region)
  (pending-delete-mode t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (evil-leader/set-key
    "i s" 'swiper
    "i r" 'ivy-resume
    "x" 'counsel-M-x
    "f" 'counsel-find-file
    "i c d" 'counsel-describe-function
    "i c v" 'counsel-describe-variable
    "i c l l" 'counsel-load-library
    "i c i" 'counsel-info-lookup-symbol
    "i c u" 'counsel-unicode-char
    "i c g" 'counsel-git
    "i c r" 'counsel-git-grep
    "i c a" 'counsel-ag
    "i c l o" 'counsel-locate
    "i c e" 'counsel-expression-history
    "b" 'ivy-switch-buffer))

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (eval-after-load 'evil-core
   '(evil-set-initial-state 'magit-popup-mode 'emacs)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package markdown-preview-mode
  :ensure t)

(use-package org 
  :ensure t)

(use-package paredit
  :ensure t)

(use-package paredit-everywhere 
  :ensure t)

(use-package php-mode
  :ensure t
  :config (require 'php-mode)
  (add-to-list 'auto-mode-alist '("\\.install\\'" . php-mode)))

(use-package projectile
  :ensure t
  :config 
  ;; projectile everywhere!
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (evil-leader/set-key
    "p i" 'projectile-ibuffer))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :ensure t)

(use-package web-mode
  :ensure t
  :config 
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode)) 
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ; set indenting to 2 spaces
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (evil-leader/set-key
    "u w a i" 'web-mode-attribute-insert
    "u w a k" 'web-mode-attribute-kill
    "u w a n" 'web-mode-attribute-next
    "u w a p" 'web-mode-attribute-previous
    "u w a s" 'web-mode-attribute-select 
    "u w c" 'web-mode-comment-or-uncomment
    "u w d t" 'web-mode-dom-traverse
    "u w e b" 'web-mode-element-beginning
    "u w e c" 'web-mode-element-content-select
    "u w e e" 'web-mode-element-end
    "u w e i" 'web-mode-element-insert
    "u w e r" 'web-mode-element-rename
    "u w e s" 'web-mode-element-select
    "u w e w" 'web-mode-element-wrap
    "u w f" 'web-mode-fold-or-unfold
    "u w i" 'web-mode-buffer-indent
    "u w t m" 'web-mode-tag-match))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (concat user-emacs-directory "customizations"))

(load "speedbar-setup.el")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;; now that we're using helm this is probably not needed anymore
;; but we'll keep it here for the time being
;; (load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
