;;Disable splash screen and menu bars
(setq inhibit-startup-screen t)
(menu-bar-mode -1) 
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;Enable line numbers
(global-linum-mode 1) 

;;highlight current cursor line
(global-hl-line-mode +1)

;;empty scratch buffer messages
(setq initial-scratch-message "")

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;;start emacs with fullscreen on
(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(fullscreen)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;load gruvbox theme
(load-theme 'gruvbox t)

;;filemanager
(require 'neotree)

;;toggle file manager
(global-set-key (kbd "C-f") 'neotree-toggle)

;;set font
(set-default-font "DejaVu Sans Mono 10")

;;cycle between buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;;cycle between window splits
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)

;;splits window
(global-set-key (kbd "C-/") 'split-window-below)
(global-set-key (kbd "C-\\") 'split-window-right)

(set-cursor-color "#00ff00") 

;;yes = y and no = n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)


;;enable word wrap
(setq-default word-wrap t)
(setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))
(defun my-truncate-lines-disable ()
  "Disable line truncation, even in split windows."
  (let ((inhibit-message t) ; No messages in the echo area - needs emacs 25+
        message-log-max ; No messages in the *Messages* buffer
        truncate-partial-width-windows) ; No truncation in split windows
    (toggle-truncate-lines 0)))
(add-hook 'help-mode-hook #'my-truncate-lines-disable)

;;yas snippet setup
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;cycle through files
(setq completion-cycle-threshold t)

;;display time
(display-time-mode 1)
;;display battery status
(display-battery-mode 1)

;;change color of modeline
(add-hook 'after-change-major-mode-hook 'my-set-mode-line-colors)
(defvar my-mode-line-colors
  '((emacs-lisp-mode :foreground "ivory" :background "DarkOrange2")
    (ruby-mode :foreground "orange" :background "red")))
(defun my-set-mode-line-colors ()
  (face-remap-add-relative
   'mode-line (list (or (cdr (assq major-mode my-mode-line-colors))
                        '(:foreground "black" :background "orange"))
                    'mode-line)))

;;default path
;; (defun my-set-global-default-directory (new-default-directory)
;;   "Set my-global-default-directory to NEW-DEFAULT-DIRECTORY."
;;   (interactive "DNew global default directory: ")
;;   (setq my-global-default-directory new-default-directory))

;; (defun my-find-file ()
;;   (interactive)
;;   (cd "~/")
;;   (call-interactively 'find-file))

;; (global-set-key (kbd "C-x C-f") 'my-find-file)


(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (compile compile-command))
(global-set-key (kbd "C-x C-x") 'save-all-and-compile)

(setq compilation-scroll-output t)

;;close compilation buffer when there are no errors present
(setq compilation-window-height 8)
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
      (message "NO COMPILATION ERRORS! Thank you dear compiler..."))))

;;set indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-z") 'undo)



(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-server-command '("python" "/home/hrishi/.emacs.d/ycmd/"))

;; (defun ycmd-setup-completion-at-point-function ()
;;   "Setup `completion-at-point-functions' for `ycmd-mode'."
;;   (add-hook 'completion-at-point-functions
;;             #'ycmd-complete-at-point nil :local))

;; (add-hook 'ycmd-mode-hook #'ycmd-setup-completion-at-point-function)

;; Autocomplete

(global-company-mode)
(use-package company
  :defer 10
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or 
         (not company-mode/enable-yas) 
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  ;; no delay no autocomplete
  (validate-setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20)

  (validate-setq company-backends 
                 (mapcar #'company-mode/backend-with-yas company-backends)))


;; Code-comprehension server
(use-package ycmd
  :ensure t
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python" "/home/hrishi/.emacs.d/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "/home/hrishi/.emacs.d/ycmd/ycm_conf.py"))

;; Autocomplete
(use-package company
  :defer 10
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or 
         (not company-mode/enable-yas) 
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  ;; no delay no autocomplete
  (validate-setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20)

  (validate-setq company-backends 
                 (mapcar #'company-mode/backend-with-yas company-backends)))


;; Code-comprehension server
(use-package ycmd
  :ensure t
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python" "/home/hrishi/.emacs.d/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "/home/hrishi/.emacs.d/ycmd/ycm_conf.py"))

  (use-package company-ycmd
    :ensure t
    :init (company-ycmd-setup)
    :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))))
