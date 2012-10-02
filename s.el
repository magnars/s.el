;;; s.el --- The long lost Emacs string manipulation library.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: strings

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

;; The long lost Emacs string manipulation library.

;;; Code:

(defun s-trim (s)
  "Remove whitespace at beginning and end of S."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun s-chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun s-ends-with-p (suffix s)
  "Does S end in SUFFIX?"
  (let ((pos (- (length suffix))))
    (and (>= (length s) (length suffix))
         (string= suffix (substring s pos)))))

(defun s-starts-with-p (suffix s)
  "Does S start with SUFFIX?"
  (let ((l (length suffix)))
    (and (>= (length s) l)
         (string= suffix (substring s 0 l)))))

(defun s-split-words (s)
  "Split S into list of words"
  (split-string
   (let ((case-fold-search nil))
     (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s))
   "[^A-Za-z0-9]+"))

(defun s--mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun s-lower-camel-case (s)
  "Convert S to lowerCamelCase."
  (mapconcat 'identity (s--mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (s-split-words s)) ""))

(defun s-upper-camel-case (s)
  "Convert S to UpperCamelCase."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (s-split-words s)) ""))

(defun s-snake-case (s)
  "Convert S to snake_case."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (downcase word))
                        (s-split-words s)) "_"))

(defun s-dashed-words (s)
  "Convert S to dashed-words."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (downcase word))
                        (s-split-words s)) "-"))

(defun s-capitalized-words (s)
  "Convert S to Capitalized Words."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (s-split-words s)) " "))

(provide 's)
;;; s.el ends here
