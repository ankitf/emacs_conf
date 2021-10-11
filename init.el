
;; ======= Melpa support =======
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;; 	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (setq package-archives
;; 	  '(("gnu" . "http://elpa.gnu.org/packages/")
;; 	    ("marmalade" . "http://marmalade-repo.org/packages")
;; 	    ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

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

;; ======= Basic Customisation =======

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
(global-linum-mode t)               ;; Enable line numbers globally

;; setting ^h as backspace and ^? as emacs help
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

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



;; ;;(setq jedi:server-command ("/usr/bin/python" "/home/ankit/.emacs.d/elpa/jedi-core-20191011.1750/jediepcserver.py"))


;; ;; enable virtual env   
;; ;;(require 'pyvenv)
;; ;; (pyvenv-activate "~/.anaconda3/envs/tf_gpu")

;; ;; enable yasnippet, autocomplete and document generation tool.
;; ;; (add-to-list 'load-path
;; ;; 			 "~/.emac.d/elpa/yasnippet")
;; ;; (require 'yasnippet)
;; ;; (yas-global-mode 1)
;; ;; ;;(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
;; ;; (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

;; ;; enable line numbers
;; ;;(setq global-linum-mode t)
;; (global-display-line-numbers-mode 1)


;; ;; setting default tab width
;; (setq-default tab-width 4)



;; ;; window movement using windmove keybindings
;; ;;(windmove-default-keybindings)
;; ;;(global-set-key (kbd "C-M-j") 'windmove-left)
;; ;;(global-set-key (kbd "C-M-;") 'windmove-right)
;; ;;(global-set-key (kbd "C-M-k") 'windmove-up)
;; ;;(global-set-key (kbd "C-M-l") 'windmove-down)


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
 
;; ;; disable menu-bar too;bar and scroll bar
;; ;; (menu-bar-mode -1)
;; ;; (tool-bar-mode -1)
;; ;; (scroll-bar-mode -1)
;; ;; ;; shrink fringes/borders to 1 pixel
;; (fringe-mode 1)

;; turning on 'display-time-mode'
(setq display-time-default-load-average nil)
(display-time-mode t)

;; enable ido-mode
(ido-mode 1)


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
;; (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(flycheck yasnippet-snippets virtualenvwrapper pyenv-mode pkg-info magit jedi golden-ratio exwm elpy better-defaults ace-window ace-jump-buffer)))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; )




;; User-Defined init.el ends here
(message "Done")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(simpleclip yasnippet-snippets virtualenvwrapper pyenv-mode py-autopep8 magit jedi golden-ratio flycheck exwm elpy blacken better-defaults ace-window ace-jump-buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
