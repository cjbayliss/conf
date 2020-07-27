;; Copyright (C) 2020 Christopher Bayliss

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

(defvar commit-message-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table) ;; # starts a comment
    (modify-syntax-entry ?\n "> b" table) ;; \n ends a comment
    table))

(defface commit-message-added-face
  '((t :foreground "SpringGreen"))
  "face to highlight added/modified files")

(defface commit-message-removed-face
  '((t :foreground "tomato"))
  "face to highlight removed files")

(defface commit-message-renamed-face
  '((t :foreground "DeepSkyBlue"))
  "face to highlight renamed files")

(defface commit-message-files-face
  '((t :foreground "white"))
  "face to highlight files")

(defvar commit-message-added-face 'commit-message-added-face)
(defvar commit-message-removed-face 'commit-message-removed-face)
(defvar commit-message-renamed-face 'commit-message-renamed-face)
(defvar commit-message-files-face 'commit-message-files-face)

(defconst commit-message-highlights
  `((,(format "^%s\\(.+\\)" (make-string 50 ?.))
     (1 font-lock-warning-face))
    ("\\(\t.*$\\)" 1 commit-message-files-face t) ;; set the files line face first
    ("\\<\\(new file\\|modified\\):" 1 commit-message-added-face t)
    ("\\<\\(deleted\\):" 1 commit-message-removed-face t)
    ("\\<\\(renamed\\):" 1 commit-message-renamed-face t)
    ))

(define-derived-mode git-commit-message-mode fundamental-mode "Git Commit Message"
  "mode for editing git commit messages"
  (setq-local font-lock-defaults '(commit-message-highlights))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (set-syntax-table commit-message-syntax-table))

(provide 'git-commit-message-mode)
