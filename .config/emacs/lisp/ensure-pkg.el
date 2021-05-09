;;; ensure-pkg --- Ensure a package is installed.

;; Author: Christopher Bayliss <cjb@cjb.sh>
;; Created: 2021-05-05
;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:

;; Ensure a package is installed.

;;; Code:
(defvar ensure-pkg-lisp-dir (concat user-emacs-directory "lisp")
  "Directory containing Emacs Lisp packages.")

(defvar ensure-pkg--ensured-packages-list '()
  "List of packages managed by ensure-pkg.")

(defun ensure-pkg--git-clone (package url)
  "Clone a PACKAGE from URL using git."
  (message (format "Installing package: %s" package))
  (shell-command
   (format "git clone --depth=1 %s %s"
           url
           (concat ensure-pkg-lisp-dir "/" package))
   nil))

(defun ensure-pkg--git-pull (package)
  "Pull upstream commits for PACKAGE."
  (message (format "Updating package: %s" package))
  (shell-command
   (format "cd %s && git pull -f"
           (concat ensure-pkg-lisp-dir "/" package))))

(defun ensure-pkg--byte-compile-package (package)
  "Byte compile a PACKAGE."
  (byte-recompile-directory (concat ensure-pkg-lisp-dir "/" package) 0 t))

(defun ensure-pkg--add-to-load-path (package &optional subdir)
  "Add PACKAGE to `load-path'.
SUBDIR is the optional subdirectory in a repo in which the
package resides."
  (normal-top-level-add-to-load-path
   (list (concat ensure-pkg-lisp-dir "/" package subdir))))

(defun ensure-pkg-update-packages ()
  "Update all packages in `ensure-pkg--ensured-packages-list'."
  (interactive)
  (mapc (lambda (package)
          (ensure-pkg--git-pull package)
          (ensure-pkg--byte-compile-package package))
        ensure-pkg--ensured-packages-list))

(defun ensure-pkg (package url &optional subdir)
  "Ensure PACKAGE is installed.
PACKAGE will be fetched from URL and install it in
`ensure-pkg-lisp-dir'."
  (when (file-directory-p
         (concat ensure-pkg-lisp-dir "/" (symbol-name package)))
    (ensure-pkg--add-to-load-path (symbol-name package) subdir))
  (unless (locate-library (symbol-name package))
    (ensure-pkg--git-clone (symbol-name package) url)
    (ensure-pkg--add-to-load-path (symbol-name package) subdir)
    (ensure-pkg--byte-compile-package (symbol-name package)))
  (add-to-list 'ensure-pkg--ensured-packages-list (symbol-name package)))

(provide 'ensure-pkg)

;;; ensure-pkg.el ends here
