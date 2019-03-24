;;; dired-toggle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dired-toggle dired-toggle-up-directory dired-toggle-find-file
;;;;;;  dired-toggle-quit) "dired-toggle" "dired-toggle.el" (23703
;;;;;;  16920))
;;; Generated autoloads from dired-toggle.el

(autoload 'dired-toggle-quit "dired-toggle" "\
Quit action under `dired-toggle-mode'.

\(fn)" t nil)

(autoload 'dired-toggle-find-file "dired-toggle" "\
Wraper for `dired-find-file', use `find-alternate-file' instead so will not
create new buffer when changing directory, and will keep `dired-toggle-mode' and
`dired-hide-details-mode' states after opening new direcoty.

\(fn)" t nil)

(autoload 'dired-toggle-up-directory "dired-toggle" "\
Wraper for `dired-up-directory', use `find-alternate-file' instead so will
not create new buffer when changing directory, and will keep `dired-toggle-mode'
and `dired-hide-details-mode' states after opening new direcoty.

\(fn)" t nil)

(autoload 'dired-toggle "dired-toggle" "\
Toggle current buffer's directory.

\(fn &optional DIR)" t nil)

;;;***

;;;### (autoloads nil nil ("dired-toggle-pkg.el") (23703 16920 711000))

;;;***

(provide 'dired-toggle-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-toggle-autoloads.el ends here
