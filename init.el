(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq visible-bell t)

(setq electric-pair-preserve-balance nil)
(electric-pair-mode t)

(set-face-attribute 'default nil :font "CMU Typewriter Text" :height 110)
(set-face-attribute 'fixed-pitch nil :font "CMU Typewriter Text" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "CMU Serif" :height 1.2)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-roam evil-collection evil org-journal yasnippet-snippets yasnippet visual-fill-column org-bullets treemacs-projectile projectile lsp-ivy lsp-treemacs company-box company company-lsp company-mode magit lsp-mode which-key counsel ivy-rich ivy rainbow-delimiters doom-themes doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package counsel
  :config
  (ivy-mode 1)
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-s" . swiper)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done))
  :custom
  (ivy-initial-inputs-alist nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

(use-package which-key
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.3))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init
  (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c++-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp-mode
  :init
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :config
  (setq projectile-project-search-path '("C:/dev/"))
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :custom((projectile-completion-system 'ivy)))

(use-package treemacs-projectile)

;; Evil mode
(defun ggiuanni/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; :hook (evil-mode . ggiuanni/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Org mode
(defun ggiuanni/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun ggiuanni/org-font-setup ()
  (dolist (face '((org-document-title . 2.0)
		  (org-level-1 . 1.8)
		  (org-level-2 . 1.7)
		  (org-level-3 . 1.6)
		  (org-level-4 . 1.5)
		  (org-level-5 . 1.4)
		  (org-level-6 . 1.3)
		  (org-level-7 . 1.2)
		  (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "CMU Serif" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . ggiuanni/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	(directory-files-recursively "~/Google Drive/Agenda" "\\.org$"))

  (setq org-refile-targets
	'((org-agenda-files :maxlevel . 1)))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Google Drive/Agenda/Main.org" "Inbox")
       "* TODO %?" :empty-lines 1)
      ("q" "Quick note" entry (file+olp "~/Google Drive/Post-it/QuickNotes.org" "Random")
           "* %?" :empty-lines 1))
    )

  (define-key global-map (kbd "C-c t")
    (lambda () (interactive) (org-capture nil "tt")))

  (define-key global-map (kbd "C-c q")
    (lambda () (interactive) (org-capture nil "q")))


  (which-key-add-key-based-replacements
    "C-c t" "Capture task")

  (which-key-add-key-based-replacements
    "C-c q" "Capture quick-note")

  (ggiuanni/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun ggiuanni/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ggiuanni/org-mode-visual-fill))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode 1))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Google Drive/journal"))

;; Org roam
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Google Drive/Polymath/")
  (org-roam-db-update-method 'immediate)
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n c" . org-roam-capture)
	       ("C-c n g" . org-roam-graph)
	       ("C-c n d" . org-roam-dailies-capture-today))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n")))
  (which-key-add-key-based-replacements
    "C-c n" "Org Roam"))
