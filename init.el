(setq inhibit-startup-message t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

(set-face-attribute 'default nil :font "Roboto Mono")
(set-face-attribute 'variable-pitch nil :font "Roboto Serif" :height 1.5)

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
(display-time-mode)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(require 'package)

(setq package-archives '(
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  :bind (("C-s" . swiper-isearch)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.05))

(defun org-mode-setup ()
  (org-indent-mode)

  (variable-pitch-mode 1)
  (display-line-numbers-mode 0))

(use-package org
  :ensure t
  :pin elpa
  :hook (org-mode . org-mode-setup)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◦")))

(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-outrun-electric t))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode))

(use-package emacsql-sqlite3
  :ensure t)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-database-connector 'sqlite3)
  (org-roam-directory "~/Polymath")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M>\n\n%?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))
