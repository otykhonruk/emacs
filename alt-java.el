;;; alt-java.el --- Tools for java programming

;; Copyright (C) 2011  Alexander Tihonruk

;; Author: Alexander Tihonruk <a.tihonruk@gmail.com>
;; Keywords: convenience, languages

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

;; Contains functions for boilerplate java code generation:
;; * Getters and setters
;; * Mandatory constructors for final fields
;; * Main function
;; 

;;; Code:

(require 'semantic)


(defun alt-current-class-vars ()
  "Returns list of tags of variable type of current class"
  (let* ((class (semantic-current-tag-of-class 'type))
	 (members (semantic-tag-get-attribute class :members))
	 (vars (semantic-find-tags-by-class 'variable members)))
    vars))


(defun alt-java-accessors (fname)
  "Writes conventional getter and setter for given class field."
  (interactive
   (list
    (semantic-read-variable "Field name: " nil (alt-current-class-vars))))
  (let* ((tag (semantic-find-first-tag-by-name fname (alt-current-class-vars)))
	 (type (semantic-tag-type tag))
	 (name (semantic-tag-name tag))
	 (cname (capitalize name)))
    (save-excursion
      (let ((start (point)))
	(insert (concat "\n public " type " get" cname "() {\nreturn " name ";\n}\n"))
	(insert (concat "\n public void set" cname "(" type " " name ") {\nthis." name " = " name ";\n}\n"))
	(indent-region start (point))))))
  

(defun alt-java-comment-function ()
  "Inserts javadoc-style comment stub for function at current position."
  (interactive)
  (let ((tag (car (reverse (semantic-find-tag-by-overlay)))))
    (if (semantic-tag-of-class-p tag 'function) ;;  if function
	(let ((start (semantic-tag-start tag))
	      (args (semantic-tag-function-arguments tag))
	      (throws (semantic-tag-function-throws tag))
	      (type (semantic-tag-type tag)))
	  (goto-char start)
	  (insert "/**\n* \n*\n")
	  (dolist (arg args)
	    (insert (concat "* @param " (semantic-tag-name arg)) "\n"))
	  (dolist (ex throws)
	    (insert (concat "* @throws " ex "\n")))
	  (if (not (equal type "void")) (insert "* @return \n"))
	  (insert "*/\n")
	  (indent-region start (+ 1 (point)))
	  (goto-char start)
	  (end-of-line 2))
      (error "Point cursor to a function you want to comment"))))



(defun alt-java-constructor ()
  "Writes constructor for all final fields of current class."
  (interactive)
  (let* ((startpos (point))
	 (vars (alt-current-class-vars))
	 (ff (mapcar 
	      '(lambda (tag)
		 (cons (semantic-tag-name tag) (semantic-tag-type tag)))
	      vars)))
    (insert "public " (semantic-tag-name class) "(")
    (dolist (f fields)
      (insert (concat (cdr ff) " " (car ff) ", ")))
    (delete-char -2)
    (insert ") {\n")
    (dolist (var vars)
      (insert (concat "this." (car ff) " = " (car ff) ";\n")))
    (insert "}\n")
    (indent-region startpos (point))))


(define-skeleton alt-java-main
  "Creates conventional java main function"
  nil
  > "public static void main(String[] args) {" \n
  > _ \n
  "}" > \n)



(provide 'alt-java)
;;; alt-java.el ends here

