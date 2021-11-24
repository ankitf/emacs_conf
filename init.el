
;; ======= Melpa support =======
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("elpa" . "https://elpa.gnu.org/packages/") t)

;; (add-to-list 'package-archives
;; 	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (setq package-archives
;; 	  '(("gnu" . "http://elpa.gnu.org/packages/")
;; 	    ("marmalade" . "http://marmalade-repo.org/packages")
;; 	    ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; use package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)


;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))


;; ======= Install Packaged Automatically =======
(defvar myPackages
  '(better-defaults    ;; Some better emacs defaults
    elpy               ;; Emacs python environment
    flycheck           ;; On the fly syntax checking
    py-autopep8        ;; Run autopep8 on Save
    blacken            ;; Black formatting on Save
    simpleclip         ;; System clipboard access, copy paste
    ace-window         ;; Window switching
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)


;; ======= Python Setup =======
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; jedi hook
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

  
;; ====== Emacs Customization =======
(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'tango-dark)            ;; tango dark theme
;; (global-linum-mode t)               ;; Enable line numbers globally

;; ;; setting ^h as backspace and ^? as emacs help
;; (global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "M-?") 'mark-paragraph)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)

;; Simpleclip for simplified access to system clipboard
(require 'simpleclip)
(simpleclip-mode 1)

;; Ace window for window switching
(global-set-key (kbd "M-o") 'ace-window)


;; ===== Tramp qblocks Setup =====
(defun connect-qblocks2 ()
  (interactive)
  (dired "/ssh:qblocks2:/home/qblocks/"))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)


;; always open a shell in new window
(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;; always open ansi-term in new window
(defun terminal-window ()
  (interactive)
  (split-window-sensibly)
  (ace-window 1)
  (ansi-term (executable-find "bash")))

(global-set-key (kbd "C-x t") #'terminal-window)


;; start emacs server
;; (server-start)

;; ;; edit chrome with emacs
;; ;; (add-to-list 'load-path "~/.emacs.d")
;; ;; (require 'edit-server)
;; ;; (edit-server-start)

;; ;;; ; setup the mail client - mu4e
;; ;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; ;; (require 'mu4e)
;; ;; (setq mail-user-agent 'mu4e-user-agent)
;; ;; (setq mu4e-maildir "/home/ankit/emacs/Maildir"
;; ;; 	  mu4e-send-folder "/home/ankit/emacs/Maildir/send"
;; ;; 	  my4e-drafts-folder "/home/ankit/emacs/Maildir/drafts"
;; ;; 	  user-mail-address "ankit.f.parmar@gmail.com"
;; ;; 	  smtpmail-default-smtp-server   "smtp.gmail.com"
;; ;; 	  smtpmail-smtp-server           "smtp.gmail.com"
;; ;; 	  smtpmail-smtp-service 587)

;; ;; (defvar my-mu4e-account-alist
;; ;;   '(("Gmail"
;; ;;      (mu4e-sent-folder "/Gmail/sent")
;; ;;      (user-mail-address "ankit.f.parmar@gmail.com")
;; ;;      (smtpmail-smtp-user "ankit.f.parmar")
;; ;;      (smtpmail-local-domain "gmail.com")
;; ;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;; ;;      (smtpmail-smtp-server "smtp.gmail.com")
;; ;;      (smtpmail-smtp-service 587)
;; ;;      )
;; ;;      ;; Include any other accounts here ...
;; ;;     ))

;; ;; (setq
;; ;;  message-send-mail-function     'smtpmail-send-it


;; disable menu-bar too;bar and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; shrink fringes/borders to 1 pixel
(fringe-mode 1)

;; turning on 'display-time-mode'
(setq display-time-default-load-average nil)
(display-time-mode t)

;; enable ido-mode
(ido-mode 1)

;; (keycast-mode 1)
;; (keycast-log-mode 1)

;; ============== Org Mode ============
(org-babel-do-load-languages 'org-babel-load-languages
                             '((python . t)))


                               
(add-hook 'org-mode-hook '(lambda () (visual-line-mode 1)))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; gtd setup
(setq org-agenda-files '("~/disk/org/gtd/inbox.org"
                         "~/disk/org/gtd/gtd.org"
                         "~/disk/org/gtd/tickler.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCELLED")))


(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo [inbox]" entry (file+headline "~/disk/org/gtd/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("T" "Tickler" entry
         (file+headline "~/disk/org/gtd/tickler.org" "Tickler")
         "* %i% \n %U")))

(setq org-refile-targets '(("~/disk/org/gtd/inbox.org" :maxlevel . 3)
                          ("~/disk/org/gtd/gtd.org" :level . 1)
                          ("~/disk/org/gtd/tickler.org" :maxlevel . 2)))



;; Org download
;; (require 'org-download)
;; (add-hook 'dired-mode-hook 'org-download-enable) ;; drag and drop to dired
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y". org-download-yank))))

;; Mathpix.el
;; (use-package mathpix.el
;;   :straight (:host github :repo "jethrokuan/mathpix.el")
;;   :custom ((mathpix-app-id "app-id")
;;            (mathpix-app-key "app-key"))
;;   :bind
;;   ("C-x m" . mathpix-screenshot))

;; org pdf view
(pdf-tools-install)
;; (add-hook 'prog-mode-hook 'linum-on)

;; scrot - Simple screenshot utility for emacs
(add-to-list 'load-path "~/.emacs.d/scrot.el/scrot.el")
;; (load "scrot")
(require 'scrot)

;; ============== Org-Roam ============
(setq org-roam-v2-ack t)

(setq org-roam-directory "/home/ankit/disk/org-roam/")

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; acknowledging v2 upgrade
  :custom
  (org-roam-directory "/home/ankit/disk/org-roam/")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup)
   :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         ("C-c n c" . org-roam-capture)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
   :bind-keymap)



;; (setq org-roam-db-autosync-mode t)
;; (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
;; (global-set-key (kbd "C-c n f") 'org-roam-node-find)
;; (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
;; (setq org-roam-completion-everywhere t)

;; Org Sync to Google Calendar: org-gcal
(setq org-gcal-client-id "444741516692-nd3t72ov6k4qvs4eqnumb1vgmht2kl8q.apps.googleusercontent.com"
      org-gcal-client-secret "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"
      org-gcal-fetch-file-alist '(("ankit.f.parmar@gmail.com" .  "~/disk/org/schedule.org")))



;; setting ^h as backspace and ^? as emacs help
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXWM CONFIG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;(require 'exwm)
;; ;; (require 'exwm-config)
;; ;; (exwm-config-default)

;; ;; load exwm
;; ;; (require 'exwm )
;; ;; (require 'exwm-config)
;; ;; (exwm-config-ido)

;; ;; set the initial number of workspaces
;; ;;(setq exwm-workspace-number 4)


;; User-Defined init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "dark orange" "#8cc4ff" "#eeeeec"])
 '(package-selected-packages
   '(auto-dim-other-buffers scrot org-noter-pdftools org-pdftools org-noter org-download use-package org-roam-bibtex org-gcal keycast org-roam org-bullets simpleclip yasnippet-snippets virtualenvwrapper pyenv-mode py-autopep8 jedi golden-ratio flycheck exwm elpy blacken better-defaults ace-window ace-jump-buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(message "Init Done")


