;;; alt-rng-util.el --- Converts Relax NG schema in compact syntax 
;; to lisp-based represenatation used by nxml internally.

;; Copyright (C) 2009  Alt

;; Author: Alt <a.tihonruk@gmail.com>
;; Keywords: tools

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
;;
;; TODO: write non-interactive function for batch processing.
;;
;; rng-cmpct is part of the nxml-mode by James Clark
;; http://www.thaiopensource.com/download/

;;; Code:

(require 'rng-cmpct) 

(defun alt-open-rnl-schema (infile)
  (interactive "fSchema file: ")
  (let ((buf 
         (generate-new-buffer 
          (concat 
           (file-name-nondirectory (file-name-sans-extension infile))
           ".rnl"))))
    (with-current-buffer buf
      (insert 
       (format ";; Generated from %s\n\n" infile)
       (pp-to-string (rng-c-load-schema infile)))
      (lisp-mode))
    (display-buffer buf)))


(defun alt-save-rnl-schema (infile outfile)
  "Converts Relax NG schema in compact syntax to lisp-based represenatation
used by nxml internally."
  (interactive "fSchema file: \nFSave As: ")
  (with-temp-file outfile
    (insert 
     (format ";; Generated from %s\n" infile)
     (rng-c-load-schema infile))))




