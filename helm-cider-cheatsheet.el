;;; helm-cider-cheatsheet.el --- Helm interface to CIDER cheatsheet        -*- lexical-binding: t; -*-

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

;; Helm interface to CIDER cheatsheet.

;; Inspired by Kris Jenkins' `clojure-cheatsheet'
;; See: https://github.com/clojure-emacs/clojure-cheatsheet

;;; Code:

(require 'cider)
(require 'cl-lib)
(require 'helm)
(require 'helm-cider-util)



(defvar helm-cider-cheatsheet--stack nil
  "Navigation stack for `helm-cider-cheatsheet'.")



(defun helm-cider-cheatsheet--heading-action (hierarchy)
  "Action for a heading hierarchy.

A heading hierarchy is of the form (<heading> <children>)."
  (helm-exit-and-execute-action
   (cl-destructuring-bind (name &rest children) hierarchy
     (helm :buffer (concat "*Helm CIDER Cheatsheet: " name)
           :sources (if (cider-cheatsheet--has-headings children)
                        (helm-cider-cheatsheet--headings-source hierarchy)
                      (cl-loop
                         for c in children
                         when (symbolp (car c))
                         collect (helm-cider-cheatsheet--leaf-source c)))))))

(defun helm-cider-cheatsheet--headings-source-action-transformer (actions hierarchy)
  "Action transformer for `helm-cider-cheatsheet--headings-source'.

Transform heading HIERARCHY's :url children to actions and append
them to ACTION."
  (cl-loop
     for (desc url) in (cl-loop
                          with children = (cdr hierarchy)
                          for (tag . info) in children
                          when (equal :url tag)
                          collect info)
     collect `(,(concat "Browse " desc) .
                (lambda (_) (helm-browse-url ,url)))
     into browse-actions
     finally return (append actions browse-actions)))

(defun helm-cider-cheatsheet--headings-source (hierarchy)
  "Return a Helm source for a parent heading hierarchy.

A parent heading hierarchy is of the form (<heading> <children>), where
each child is also of the form (<heading> <children>)."
  (let ((name (car hierarchy))
        (heading-children (cl-remove-if-not #'stringp (cdr hierarchy) :key #'car)))
    (helm-build-sync-source name
      :candidates (cl-loop
                     for child in heading-children
                     collect (cons (car child) child) into candidates
                     finally return (cl-sort candidates #'string< :key #'car))
      :action (helm-make-actions "Drill down" #'helm-cider-cheatsheet--heading-action)
      :action-transformer #'helm-cider-cheatsheet--headings-source-action-transformer)))

(defun helm-cider-cheatsheet--leaf-source (leaf)
  (let ((leaf (mapcar #'symbol-name leaf)))
    (cl-destructuring-bind (ns &rest vs) leaf
      (helm-build-sync-source ns
        :candidates (cl-loop
                       for v in (sort vs #'string<)
                       collect (cons (cider-font-lock-as-clojure v) (concat ns "/" v)))
        :persistent-action #'helm-cider-cheatsheet--leaf-persistent-action))))

(defun helm-cider-cheatsheet--leaf-persistent-action (candidate)
  "Persistent action for Helm CIDER cheatsheet leaf."
  (cider-ensure-connected)
  (if (and (helm-attr 'doc-lookup-p)
           (string= candidate (helm-attr 'current-candidate)))
      (progn
        (kill-buffer cider-doc-buffer)
        (helm-attrset 'doc-lookup-p nil))
    (cider-doc-lookup candidate)
    (helm-attrset 'doc-lookup-p t))
  (helm-attrset 'current-candidate candidate))


;;;; API

;;;###autoload
(defun helm-cider-cheatsheet ()
  "Use helm to show a Clojure cheatsheet."
  (interactive)
  (helm :buffer "*Helm CIDER Cheatsheet*"
        :sources (helm-cider-cheatsheet--headings-source (cons "All" cider-cheatsheet-hierarchy))))


(provide 'helm-cider-cheatsheet)
