;;; BASIC
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable menu bar

;; Set up visual bell
(setq visible-bell t)

;; Set font
(set-face-attribute 'default nil :font "Fira Code Retina")

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 1.0)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Sans" :weight 'regular :height 1.2)

;; Make ESC quit propts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Show line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Display time
(display-time-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Function for killing all "other" buffers
(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;; Packages

;; TIPS
;; 1. If say "package XYZ...zip not found"
;; launch command "list-packages". It will update the package list

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
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
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" default))
 '(package-selected-packages
   '(org-roam powershell sound-wav org-pomodoro org-journal yasnippet-snippets yasnippet visual-fill-column visual-fill org-bullets forge evil-magit magit counsel-projectile projectile hydra evil-collection evil general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Logging commands (not really necessary)
(use-package command-log-mode)

;; Use Ivy and Councel for completition
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Doom themes
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;; Colored Parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show possible keys after a base command (Ex. C-x)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay  0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; General.el
(use-package general
  :config
  (general-create-definer ggiuanni/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ggiuanni/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose-theme")))

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

;; Hydra for transient state key-bindings
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ggiuanni/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "C:\dev")
    (setq projectile-project-search-path '("C:\dev")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))
  
;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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
  (set-face-attribute (car face) nil :font "Fira Sans" :weight 'regular :height (cdr face)))

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
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Google Drive/Agenda/Tasks.org" "Inbox")
           "* TODO %?" :empty-lines 1)))

  (define-key global-map (kbd "C-c t")
    (lambda () (interactive) (org-capture nil "tt")))

  (which-key-add-key-based-replacements
    "C-c t" "Capture task")

  (ggiuanni/org-font-setup))

(ggiuanni/leader-keys
   "o"  '(:ignore t :which-key "Org"))

(ggiuanni/leader-keys
  "oc" '(org-capture :which-key "Capture"))

(ggiuanni/leader-keys
  "oa" '(org-agenda :which-key "Agenda"))


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
  ;; (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Google Drive/journal"))

(ggiuanni/leader-keys
 "oj" '(org-journal-new-entry :which-key "New Journal Entry"))


(use-package sound-wav)

(use-package org-pomodoro
  :config
  (setq org-pomodoro-play-sounds t
        org-pomodoro-start-sound-p t
        org-pomodoro-finished-sound-p t
        org-pomodoro-killed-sound-p t
        org-pomodoro-short-break-sound-p t
        org-pomodoro-long-break-sound-p t)
  :custom (org-pomodoro-manual-break t))

(ggiuanni/leader-keys
 "op" '(org-pomodoro :which-key "Pomodoro"))

;; Org-roam
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
	       ("C-c n g" . org-roam-graph)
	       ("C-c n d" . org-roam-dailies-capture-today))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))
