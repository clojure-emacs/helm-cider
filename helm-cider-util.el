;;; helm-cider.el --- Helm interface to CIDER        -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Tianxiang Xiong

;; Author: Tianxiang Xiong <tianxiang.xiong@gmail.com>

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

;; Common utility functions that don't belong anywhere else.

;;; Code:

(require 'cl-lib)
(require 'subr-x)


;;;; Utilities

(defun helm-cider--regexp-symbol (string)
  "Create a regexp that matches STRING as a symbol.

If STRING ends in a character that `helm-major-mode' does not
consider to be in the word or symbol syntax class, do not include
a symbol-end \(\\_>\); otherwise, the regexp wouldn't match."
  (if (not (string= "" string))
      (let* ((lchar (aref string (1- (length string))))
             (symbol-end (with-syntax-table helm-major-mode-syntax-table
                           (if (or (= ?w (char-syntax lchar))
                                   (= ?_ (char-syntax lchar)))
                               "\\_>"
                             ""))))
        (concat "\\_<" (regexp-quote (or string "")) symbol-end))
    ""))

(defun helm-cider--source-by-name (name &optional sources)
  "Get a Helm source in SOURCES by NAME.

Default value of SOURCES is `helm-sources'."
  (car (cl-member-if (lambda (source)
                       (string= name (assoc-default 'name source)))
                     (or sources helm-sources))))

(defun helm-cider--symbol-name (qualified-name)
  "Get the name portion of the fully qualified symbol name
QUALIFIED-NAME (e.g. \"reduce\" for \"clojure.core/reduce\").

Defaults to QUALIFIED-NAME if name is NOT qualified (as is the
case for special forms)."
  (if (string-match-p "/" qualified-name)
      (cadr (split-string qualified-name "/"))
    qualified-name))

(defun helm-cider--symbol-ns (qualified-name)
  "Get the namespace portion of the fully qualified symbol name
QUALIFIED-NAME (e.g. \"clojure.core\" for
\"clojure.core/reduce\").

Defaults to the `clojure.core' ns if name is NOT qualified (as is
the case for special forms)."
  (if (string-match-p "/" qualified-name)
      (car (split-string qualified-name "/"))
    "clojure.core"))

(defun helm-cider--symbol-face (type)
  "Face for symbol of TYPE.

TYPE values include \"function\", \"macro\", etc."
  (pcase type
    ("function" 'font-lock-function-name-face)
    ("macro" 'font-lock-keyword-face)
    ("special-form" 'font-lock-keyword-face)
    ("variable" 'font-lock-variable-name-face)))

(defun helm-cider--make-sort-sources-fn (&optional descending)
  "Sort Helm sources by name in ascending order.

If DESCENDING is true, sort in descending order."
  (let ((fn (if descending (lambda (a b) (string< b a)) #'string<)))
    (lambda (s1 s2)
      (funcall fn (assoc-default 'name s1) (assoc-default 'name s2)))))


(provide 'helm-cider-util)

;;; cider-util.el ends here
