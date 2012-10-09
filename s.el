;;; s.el --- The long lost Emacs string manipulation library.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 1.0.0
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

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(defun s-collapse-whitespace (s)
  "Convert all adjacent whitespace characters to a single space."
  (replace-regexp-in-string "[ \t\n\r]+" " " s))

(defun s-lines (s)
  "Splits S into a list of strings on newline characters."
  (split-string s "\n"))

(defun s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (mapconcat 'identity strings separator))

(defun s-concat (&rest strings)
  "Join all the string arguments into one string."
  (apply 'concat strings))

(defun s-repeat (num s)
  "Make a string of S repeated NUM times."
  (let (ss)
    (while (> num 0)
      (setq ss (cons s ss))
      (setq num (1- num)))
    (apply 'concat ss)))

(defun s-chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun s-chomp (s)
  "Remove trailing newline from S."
  (s-chop-suffix "\n" s))

(defun s-truncate (len s)
  "If S is longer than LEN, cut it down and add ... at the end."
  (if (> (length s) len)
      (format "%s..." (substring s 0 (- len 3)))
    s))

(defun s-word-wrap (len s)
  "If S is longer than LEN, wrap the words with newlines."
  (with-temp-buffer
    (insert s)
    (let ((fill-column len))
      (fill-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun s-left (len s)
  "Returns up to the LEN first chars of S."
  (if (> (length s) len)
      (substring s 0 len)
    s))

(defun s-right (len s)
  "Returns up to the LEN last chars of S."
  (let ((l (length s)))
    (if (> l len)
        (substring s (- l len) l)
      s)))

(defun s-ends-with-p (suffix s &optional ignore-case)
  "Does S end with SUFFIX?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-suffix-p'"
  (let ((start-pos (- (length s) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                s start-pos nil ignore-case)))))

(defun s-starts-with-p (prefix s &optional ignore-case)
  "Does S start with PREFIX?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-prefix-p'. This is a simple wrapper around the built-in
`string-prefix-p'."
  (string-prefix-p prefix s ignore-case))

(defalias 's-suffix-p 's-ends-with-p)
(defalias 's-prefix-p 's-starts-with-p)

(defun s--bool (val)
  (not (null val)))

(defun s-contains-p (needle s &optional ignore-case)
  "Does S contain NEEDLE?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((case-fold-search ignore-case))
    (s--bool (string-match-p (regexp-quote needle) s))))

(defun s-matches-p (regexp s)
  "Does REGEXP match S?

This is a simple wrapper around the built-in `string-match-p'."
  (s--bool (string-match-p regexp s)))

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun s-split-words (s)
  "Split S into list of words."
  (split-string
   (let ((case-fold-search nil))
     (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s))
   "[^A-Za-z0-9]+" t))

(defun s--mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun s-lower-camel-case (s)
  "Convert S to lowerCamelCase."
  (mapconcat 'identity (s--mapcar-head
                        (lambda (word) (downcase word))
                        (lambda (word) (capitalize (downcase word)))
                        (s-split-words s)) ""))

(defun s-upper-camel-case (s)
  "Convert S to UpperCamelCase."
  (mapconcat 'identity (mapcar
                        (lambda (word) (capitalize (downcase word)))
                        (s-split-words s)) ""))

(defun s-snake-case (s)
  "Convert S to snake_case."
  (mapconcat 'identity (mapcar
                        (lambda (word) (downcase word))
                        (s-split-words s)) "_"))

(defun s-dashed-words (s)
  "Convert S to dashed-words."
  (mapconcat 'identity (mapcar
                        (lambda (word) (downcase word))
                        (s-split-words s)) "-"))

(defun s-capitalized-words (s)
  "Convert S to Capitalized Words."
  (mapconcat 'identity (mapcar
                        (lambda (word) (capitalize (downcase word)))
                        (s-split-words s)) " "))

(provide 's)
;;; s.el ends here
