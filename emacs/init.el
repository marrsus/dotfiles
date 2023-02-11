(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package nlinum-relative)
(nlinum-relative-on)
(global-nlinum-mode 1)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "d"  '(:ignore t :which-key "close stuff")
    "dd" '(delete-window :which-key "close frame")
    "db" '(kill-buffer)
    "n"  '(:ignore t :which-key "make new")
    "nf" '(make-empty-file :which-key "new file")
    "nv" '(split-window-below :which-key "new frame vertically")
    "nh" '(split-window-right :which-key "new frame horizontally")
    ))
(use-package flyspell)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-o") 'find-file)
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-v") 'clipboard-yank)
  (define-key evil-insert-state-map (kbd "C-o") 'find-file)
  (define-key evil-insert-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-v") 'clipboard-yank))
 ; (define-key evil-normal-state-map (kbd "C-c") 'copy)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (global-set-key (kbd "C-f") 'i-search)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)



(use-package projectile)
(use-package page-break-lines)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "the only text editor that is the center of a hole religion")
;; Set the banner
(setq dashboard-startup-banner 1)
(setq dashboard-center-content t)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)))
(setq dashboard-set-heading-icons nil)
(setq dashboard-set-file-icons t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
                eshell-mode-hook
		dashboard-mode-hook))
  (add-hook mode (lambda () (nlinum-mode 0))))


(dolist (list'((comint-mode . normal)
               (help-mode . emacs)
               (grep-mode . emacs)
               (dired-mode . emacs)))
  (evil-set-initial-state (car list) (cdr list)))


  (defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . efs/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
    :config
    (lsp-enable-which-key-integration t))

 
;; optionally
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))
;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


  (use-package company
    :after eglot
    :hook (eglot . company-mode)
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))
  (global-company-mode 1)


(use-package eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure) 

;rust
(use-package rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
;$ pacman -S rust-analyzer


;python
(use-package lsp-jedi
  :ensure t)


(add-to-list 'exec-path "/home/marrinus/.local/bin")

;openscad
(use-package scad-mode)
;cargo install openscad-lsp

;latex
					;cargo install --locked --git https://github.com/latex-lsp/texlab.git
(use-package lsp-latex)
(setq tex-command "platex --synctex=1")
;; "texlab" must be located at a directory contained in `exec-path'.
;; If you want to put "texlab" somewhere else,
;; you can specify the path to "texlab" as follows:
;; (setq lsp-latex-texlab-executable "/path/to/texlab")

(with-eval-after-load "tex-mode"
 (add-hook 'tex-mode-hook 'lsp)
 (add-hook 'latex-mode-hook 'lsp))
(setq lsp-latex-build-executable "xelatex")

;bash
;lsp-install-server <ret> bash-ls

;javascript/typescript
(add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
	  :lint t))

;html og css
;lsp-install-server <ret> css-ls 
;lsp-install-server <ret> html-ls

;LanguageTool
;yay ltex-ls-bin

