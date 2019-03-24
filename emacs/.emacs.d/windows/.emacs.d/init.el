;;This config is for emacs version 24

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;;________________________________________________________________________________________________________________________
;;Disable splash screen and menu bars
(setq inhibit-startup-screen t)
(menu-bar-mode -1) 
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;;________________________________________________________________________________________________________________________
;;Enable line numbers
(global-linum-mode 1) 
;;________________________________________________________________________________________________________________________
;;highlight current cursor line
(global-hl-line-mode +1)
;;________________________________________________________________________________________________________________________
;;empty scratch buffer messages
(setq initial-scratch-message "")

;;________________________________________________________________________________________________________________________
;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;;________________________________________________________________________________________________________________________
(w32-send-sys-command 61488)	;;windows emacs fullscreen
(load-theme 'gruvbox t)	;;load gruvbox theme
;;________________________________________________________________________________________________________________________
;;emacs filemanager -speedbar
(global-set-key (kbd "C-f") 'speedbar)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'") ;;show hidden files
;;________________________________________________________________________________________________________________________

;; set font
;; (set-default-font "Consolas 10")
;; Use 10-pt Consolas as default font
;;________________________________________________________________________________________________________________________
;; load;;cycle between buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; ;;cycle between window splits
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)

;; ;;splits window
(global-set-key (kbd "C-/") 'split-window-below)
(global-set-key (kbd "C-\\") 'split-window-right)

(set-cursor-color "#00ff00") 

;; ;;yes = y and no = n
(fset 'yes-or-no-p 'y-or-n-p)

;; ;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)


;; ;;enable word wrap
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

;;________________________________________________________________________________________________________________________
;;cycle through files
(setq completion-cycle-threshold t)
;;________________________________________________________________________________________________________________________
;; ;;display time
(display-time-mode 1)
;;display battery status
(display-battery-mode 1)
;;________________________________________________________________________________________________________________________
;; ;;change color of modeline
(add-hook 'after-change-major-mode-hook 'my-set-mode-line-colors)
(defvar my-mode-line-colors
  '((emacs-lisp-mode :foreground "ivory" :background "DarkOrange2")
    (ruby-mode :foreground "orange" :background "red")))
(defun my-set-mode-line-colors ()
  (face-remap-add-relative
   'mode-line (list (or (cdr (assq major-mode my-mode-line-colors))
                        '(:foreground "black" :background "orange"))
                    'mode-line)))
;;________________________________________________________________________________________________________________________
;; ;;default path
;; ;; (defun my-set-global-default-directory (new-default-directory)
;; ;;   "Set my-global-default-directory to NEW-DEFAULT-DIRECTORY."
;; ;;   (interactive "DNew global default directory: ")
;; ;;   (setq my-global-default-directory new-default-directory))

;; ;; (defun my-find-file ()
;; ;;   (interactive)
;; ;;   (cd "~/")
;; ;;   (call-interactively 'find-file))

;; ;; (global-set-key (kbd "C-x C-f") 'my-find-file)

;;________________________________________________________________________________________________________________________
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
;;________________________________________________________________________________________________________________________
