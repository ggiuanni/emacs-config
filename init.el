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
;;(electric-pair-mode t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 1.2)

(column-number-mode)
(global-display-line-numbers-mode t)

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "b7e460a67bcb6cac0a6aadfdc99bdf8bbfca1393da535d4e8945df0648fa95fb" default))
 '(package-selected-packages
   '(org-roam nyan-mode flycheck lsp-ui company-box lsp-treemacs lsp-ivy company company-mode lsp-mode treemacs-projectile projectile ivy-rich treemacs org-journal yasnippet-snippets yasnippet visual-fill-column org-bullets magit-gitflow magit rainbow-delimiters doom-themes doom-modeline all-the-icons which-key counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package which-key
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.3))

(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-outrun-electric t))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(use-package nyan-mode
  ;:hook (doom-modeline-mode . nyan-mode)
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t)
  (nyan-bar-length 130)
  (nyan-minimum-window-width 100))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :init
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package treemacs
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package counsel
  :init
  (ivy-mode 1)
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-s" . swiper)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-initial-inputs-alist nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '("~/")))

(use-package treemacs-projectile)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package magit-gitflow
  :after magit
  :hook
  (magit-mode . turn-on-magit-gitflow))

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq header-line-format ""))

(defun org-font-setup ()
  (dolist (face '((org-document-title . 2.0)
		  (org-level-1 . 1.8)
		  (org-level-2 . 1.7)
		  (org-level-3 . 1.6)
		  (org-level-4 . 1.5)
		  (org-level-5 . 1.4)
		  (org-level-6 . 1.3)
		  (org-level-7 . 1.2)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :commands org-mode
  :hook (org-mode . org-mode-setup)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◦")))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package org-journal
  :custom
  (org-journal-dir "~/Polymath/Journal")
  (org-journal-find-file 'find-file))


(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-mode-setup))
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Polymath")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M>\n%?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

(require 'org-roam-node)
(require 'seq)

(defun my/org-roam-get-agenda-notes ()
  (interactive)
  (mapcar
   #'org-roam-node-file
   (seq-filter
    (lambda (node)
      (member "Agenda" (org-roam-node-tags node)))
    (org-roam-node-list))))

(setq org-directory "~/Polymath")
(setq org-agenda-files (my/org-roam-get-project-notes))
