;;; guix-graph.el --- Making and viewing Guix graphs  -*- lexical-binding: t -*-

;; Copyright © 2017 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides the code to make and view Guix graphs.

;;; Code:

(require 'cl-lib)
(require 'guix-external)
(require 'guix-utils)
(require 'guix-read)
(require 'guix-repl)
(require 'guix-guile)

(defun guix-graph-backend->graph-type (backend)
  "Convert Guix graph BACKEND (string) to a graph type.
The graph type is used internally by Emacs-Guix; it can be one of
the following symbols: `dot', `html'."
  (if (stringp backend)
      (cond ((string= backend "graphviz") 'dot)
            ((string= backend "d3js") 'html)
            (t (error "Unsupported graph backend: %s" backend)))
    (error "Graph backend should be a string: %S" backend)))

(declare-function browse-url-file-url "browse-url" (file))

(defun guix-view-graph (graph-type graph-file)
  "View graph from GRAPH-FILE.
See `guix-graph-backend->graph-type' for the meaning of GRAPH-TYPE."
  (cl-case graph-type
    (dot (guix-find-file graph-file))
    (html
     (require 'browse-url)
     (browse-url (browse-url-file-url graph-file)))))

(defun guix-make-view-graph (backend graph-maker)
  "Make graph using GRAPH-MAKER procedure and view it.
GRAPH-MAKER is called with GRAPH-TYPE and GRAPH-FILE arguments.
It should return non-nil on success.
See `guix-graph-backend->graph-type' for the meaning of GRAPH-TYPE."
  (let* ((graph-type (guix-graph-backend->graph-type backend))
         (graph-file (cl-case graph-type
                       (dot  (guix-dot-file-name))
                       (html (guix-html-file-name)))))
    (if (funcall graph-maker graph-type graph-file)
        (guix-view-graph graph-type graph-file)
      (error "Couldn't create a graph"))))

;;;###autoload
(defun guix-package-graph (package backend node-type)
  "Show BACKEND/NODE-TYPE graph for a PACKAGE.
PACKAGE can be either a package name or a package ID.
Interactively, prompt for arguments."
  (interactive
   (list (guix-read-package-name)
         (guix-read-graph-backend)
         (guix-read-graph-node-type)))
  (guix-make-view-graph
   backend
   (lambda (graph-type graph-file)
     (guix-eval-read
      (guix-make-guile-expression
       'make-package-graph package
       (cl-case graph-type
         (dot  (guix-dot-arguments graph-file))
         (html graph-file))
       :node-type-name node-type
       :backend-name backend)))))

(provide 'guix-graph)

;;; guix-graph.el ends here
