;;; s.el --- The long lost Emacs string manipulation library.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 1.3.0
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
;;
;; See documentation on https://github.com/magnars/s.el#functions

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
  (split-string s "\\(\r\n\\|[\n\r]\\)"))

(defun s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (mapconcat 'identity strings separator))

(defun s-concat (&rest strings)
  "Join all the string arguments into one string."
  (apply 'concat strings))

(defun s-prepend (prefix s)
  "Concatenate PREFIX and S."
  (concat prefix s))

(defun s-append (suffix s)
  "Concatenate S and SUFFIX."
  (concat s suffix))

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

(defun s-chop-suffixes (suffixes s)
  "Remove SUFFIXES one by one in order, if they are at the end of S."
  (while suffixes
    (setq s (s-chop-suffix (car suffixes) s))
    (setq suffixes (cdr suffixes)))
  s)

(defun s-chop-prefix (prefix s)
  "Remove PREFIX if it is at the start of S."
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

(defun s-chop-prefixes (prefixes s)
  "Remove PREFIXES one by one in order, if they are at the start of S."
  (while prefixes
    (setq s (s-chop-prefix (car prefixes) s))
    (setq prefixes (cdr prefixes)))
  s)

(defun s-shared-start (s1 s2)
  "Returns the longest prefix S1 and S2 have in common."
  (let ((search-length (min (length s1) (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i) (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun s-shared-end (s1 s2)
  "Returns the longest suffix S1 and S2 have in common."
  (let* ((l1 (length s1))
         (l2 (length s2))
         (search-length (min l1 l2))
         (i 0))
    (while (and (< i search-length)
                (= (aref s1 (- l1 i 1)) (aref s2 (- l2 i 1))))
      (setq i (1+ i)))
    ;; If I is 0, then it means that there's no common suffix between
    ;; S1 and S2.
    ;;
    ;; However, since (substring s (- 0)) will return the whole
    ;; string, `s-shared-end' should simply return the empty string
    ;; when I is 0.
    (if (zerop i)
        ""
      (substring s1 (- i)))))

(defun s-chomp (s)
  "Remove one trailing `\\n`, `\\r` or `\\r\\n` from S."
  (s-chop-suffixes '("\n" "\r") s))

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

(defun s-center (len s)
  "If S is shorter than LEN, pad it with spaces so it is centered."
  (let ((extra (max 0 (- len (length s)))))
    (concat
     (make-string (ceiling extra 2) ? )
     s
     (make-string (floor extra 2) ? ))))

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

(defun s-ends-with? (suffix s &optional ignore-case)
  "Does S end with SUFFIX?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-suffix?'"
  (let ((start-pos (- (length s) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                s start-pos nil ignore-case)))))

(defalias 's-ends-with-p 's-ends-with?)

(defun s-starts-with? (prefix s &optional ignore-case)
  "Does S start with PREFIX?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-prefix?'. This is a simple wrapper around the built-in
`string-prefix-p'."
  (string-prefix-p prefix s ignore-case))

(defalias 's-starts-with-p 's-starts-with?)

(defalias 's-suffix? 's-ends-with?)
(defalias 's-prefix? 's-starts-with?)
(defalias 's-suffix-p 's-ends-with?)
(defalias 's-prefix-p 's-starts-with?)

(defun s--truthy? (val)
  (not (null val)))

(defun s-contains? (needle s &optional ignore-case)
  "Does S contain NEEDLE?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((case-fold-search ignore-case))
    (s--truthy? (string-match-p (regexp-quote needle) s))))

(defalias 's-contains-p 's-contains?)

(defun s-equals? (s1 s2)
  "Is S1 equal to S2?

This is a simple wrapper around the built-in `string-equal'."
  (string-equal s1 s2))

(defalias 's-equals-p 's-equals?)

(defun s-matches? (regexp s)
  "Does REGEXP match S?

This is a simple wrapper around the built-in `string-match-p'."
  (s--truthy? (string-match-p regexp s)))

(defalias 's-matches-p 's-matches?)

(defun s-blank? (s)
  "Is S nil or the empty string?"
  (or (null s) (string= "" s)))

(defun s-lowercase? (s)
  "Are all the letters in S in lower case?"
  (let ((case-fold-search nil))
    (not (string-match-p "[A-ZÆØÅ]" s))))

(defun s-uppercase? (s)
  "Are all the letters in S in upper case?"
  (let ((case-fold-search nil))
    (not (string-match-p "[a-zæøå]" s))))

(defun s-mixedcase? (s)
  "Are there both lower case and upper case letters in S?"
  (let ((case-fold-search nil))
    (s--truthy?
     (and (string-match-p "[a-zæøå]" s)
          (string-match-p "[A-ZÆØÅ]" s)))))

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun s-downcase (s)
  "Convert S to lower case.

This is a simple wrapper around the built-in `downcase'."
  (downcase s))

(defun s-upcase (s)
  "Convert S to upper case.

This is a simple wrapper around the built-in `upcase'."
  (upcase s))

(defun s-capitalize (s)
  "Convert the first word's first character to upper case and the rest to lower case in S."
  (concat (upcase (substring s 0 1)) (downcase (substring s 1))))

(defun s-titleize (s)
  "Convert each word's first character to upper case and the rest to lower case in S.

This is a simple wrapper around the built-in `capitalize'."
  (capitalize s))

(defmacro s-with (s form &rest more)
  "Threads S through the forms. Inserts S as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
last item in second form, etc."
  (if (null more)
      (if (listp form)
          `(,(car form) ,@(cdr form) ,s)
        (list form s))
    `(s-with (s-with ,s ,form) ,@more)))

(put 's-with 'lisp-indent-function 1)

(defun s-index-of (needle s &optional ignore-case)
  "Returns first index of NEEDLE in S, or nil.

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((case-fold-search ignore-case))
    (string-match-p (regexp-quote needle) s)))

(defun s-reverse (s) ;; from org-babel-reverse-string
  "Return the reverse of S."
  (apply 'string (nreverse (string-to-list s))))

(defun s-match (regexp s)
  "When the given expression matches the string, this function returns a list
of the whole matching string and a string for each matched subexpressions.
If it did not match the returned value is an empty list (nil)."
  (if (string-match regexp s)
      (let ((match-data-list (match-data))
            result)
        (while match-data-list
          (let* ((beg (car match-data-list))
                 (end (cadr match-data-list))
                 (subs (if (and beg end) (substring s beg end) nil)))
            (setq result (cons subs result))
            (setq match-data-list
                  (cddr match-data-list))))
        (nreverse result))))

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
  (s-join "" (s--mapcar-head 'downcase 'capitalize (s-split-words s))))

(defun s-upper-camel-case (s)
  "Convert S to UpperCamelCase."
  (s-join "" (mapcar 'capitalize (s-split-words s))))

(defun s-snake-case (s)
  "Convert S to snake_case."
  (s-join "_" (mapcar 'downcase (s-split-words s))))

(defun s-dashed-words (s)
  "Convert S to dashed-words."
  (s-join "-" (mapcar 'downcase (s-split-words s))))

(defun s-capitalized-words (s)
  "Convert S to Capitalized Words."
  (let ((words (s-split-words s)))
    (s-join " " (cons (capitalize (car words)) (mapcar 'downcase (cdr words))))))

(defun s-titleized-words (s)
  "Convert S to Titleized Words."
  (s-join " " (mapcar 's-titleize (s-split-words s))))


;; Errors for s-format
(progn
  (put 's-format-resolve
       'error-conditions
       '(error s-format s-format-resolve))
  (put 's-format-resolve
       'error-message
       "Cannot resolve a template to values"))

(defun s-format (template replacer &optional extra)
  "Format TEMPLATE with the function REPLACER.

REPLACER takes an argument of the format variable and optionally
an extra argument which is the EXTRA value from the call to
`s-format'.

Several standard `s-format' helper functions are recognized and
adapted for this:

    (s-format \"${name}\" 'gethash hash-table)
    (s-format \"${name}\" 'aget alist)
    (s-format \"$0\" 'elt sequence)

The REPLACER function may be used to do any other kind of
transformation."
  (let ((saved-match-data (match-data)))
    (unwind-protect
         (replace-regexp-in-string
          "\\$\\({\\([^}]+\\)}\\|[0-9]+\\)"
          (lambda (md)
            (let ((var
                   (let ((m (match-string 2 md)))
                     (if m m
                         (string-to-number (match-string 1 md)))))
                  (replacer-match-data (match-data)))
              (unwind-protect
                   (let ((v
                          (cond
                            ((eq replacer 'gethash)
                             (funcall replacer var extra))
                            ((eq replacer 'aget)
                             (funcall replacer extra var))
                            ((eq replacer 'elt)
                             (funcall replacer extra var))
                            (t
                             (set-match-data saved-match-data)
                             (if extra
                                 (funcall replacer var extra)
                                 (funcall replacer var))))))
                     (if v v (signal 's-format-resolve md)))
                (set-match-data replacer-match-data)))) template)
      (set-match-data saved-match-data))))

(provide 's)
;;; s.el ends here
