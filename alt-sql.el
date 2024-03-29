;;; alt-sql.el --- Custom login procedure for SQL mode

;; Copyright (C) 2009  Alt

;; Author: Alexander Tihonruk <a.tihonruk@gmail.com>
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom login procedure and other SQL-related stuff. Somewhat Oracle-specific.

;;; Code:

(defvar *sql-profiles* ())
(defvar *sql-path* nil
  "Directory in which SQL shells should be started.")
(defvar *sql-root* nil
  "Reserved, currently unused.")

(defvar *nls-lang* nil)
(defvar *sql-buffer-process-coding* nil)
(defvar *sql-history-prefix* ".sql_history")
(defvar *sql-default-product* 'oracle)

(defun alt-register-sql-profile (name profile)
  (let ((cons (assoc name *sql-profiles*)))
    (if cons
        (setf (cdr cons) profile)
      (push (cons name profile) *sql-profiles*))))


(defun alt-sql-set-coding ()
  (set-buffer (sql-find-sqli-buffer))
  (set-buffer-process-coding-system
   *sql-buffer-process-coding*
   *sql-buffer-process-coding*))


(defun alt-sql-login (name)
  "Custom login procedure. Added features: profiles, environment variables, coding system."
  (interactive 
   (list (completing-read "Choose profile: " *sql-profiles*)))
  (let* ((cons (assoc name *sql-profiles*))
         (product (or (fifth cons) *sql-default-product*))
         (sqlpath (or *sql-path* (getenv "SQL_PATH"))))
    (if (and (equal product 'oracle) *nls-lang*)
	(setenv "NLS_LANG" *nls-lang*))
    (if (file-accessible-directory-p sqlpath)
	(cd sqlpath))
    (setq sql-user (second cons)
          sql-password (third cons) 
          sql-database (fourth cons))
    (setq sql-input-ring-file-name 
	  (concat (file-name-as-directory sqlpath) *sql-history-prefix* "." name))
    (sql-product-interactive product)
    (alt-sql-set-coding)
    (sql-rename-buffer)))


(provide 'alt-sql)
;;; alt-sql.el ends here
