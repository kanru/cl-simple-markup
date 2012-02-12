;;;; parser.lisp --- Parser

;;; Copyright (C) 2012  Kan-Ru Chen

;;; Author(s): Kan-Ru Chen <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; Ref:
;;; http://stackoverflow.com/questions/605434/how-would-you-go-about-parsing-markdown
;;; http://daringfireball.net/projects/markdown/syntax
;;; https://github.com/mfp/ocsiblog/blob/master/simple_markup.ml
;;; https://github.com/mfp/ocsiblog/blob/more-markdown-compat/simple_markup.ml
;;; http://eigenclass.org/R2/writings/markdown-redux-html-generation
;;; http://eigenclass.org/R2/writings/fast-extensible-simplified-markdown-in-ocaml

;;;; Code:

(in-package #:simple-markup)

;;; Enum library

(defclass enum ()
  ((items :initarg :items
          :initform nil          
          :accessor items)))

(defun make-enum (&optional list)
  (make-instance 'enum :items list))

(defun enum-push (enum item)
  (push item (items enum))
  item)

(defun enum-peek (enum)
  (car (items enum)))

(defun enum-junk (enum)
  (pop (items enum)))

(defun enum-map (fun enum)
  (make-enum (mapcar fun (items enum))))

;;; Parser

(defun collect (f x)
  (labels ((%loop (acc)
             (let ((r (funcall f x)))
               (case (car r)
                 (:none (reverse acc))
                 (:some (%loop (cons (cadr r) acc)))))))
    (%loop nil)))

(defun lines (s)
  (with-input-from-string (in s)
    (loop when (read-line in nil)
          collect it)))

(defun parse-text (s)
  (parse-lines (lines s)))

(defun parse-lines (ls)
  (parse-enum (make-enum ls)))

(defun parse-enum (e)
  (collect (lambda (e) (read-paragraph 0 e))
    (enum-map (lambda (l) (let ((l^ (string-strip l)))
                       (list (indentation l) l^ (string= l^ ""))))
              (enum-map #'expand-tabs e))))

(defun string-strip (s)
  (string-trim '(#\Tab #\Space #\Linefeed #\Return) s))

(defun expand-tabs (s)
  (let ((len (length s))
        (n 0))
    (with-output-to-string (out)
      (loop for i from 0 below len
           do (case (char s i)
                (#\Tab (let ((l (- 8 (rem n 8))))
                         (write-string
                          (make-string l :initial-element #\Space) out)
                         (incf n l)))
                (otherwise
                 (incf n)
                 (write-char (char s i) out)))))))

(defun indentation (s)
  (loop for c across s
        while (char= c #\Space)
        count t))

(defun read-paragraph (indent e &optional (prev indent))
  (let ((some (enum-peek e)))
    (case (car some)
      (:none '(:none))
      (:some
       (destructuring-bind (indentation line isblank)
           (cdr some)
         (if isblank
             (progn
               (enum-junk e)
               (read-paragraph indent e prev))
             (if (< indentation indent)
                 '(:none)
                 (progn
                   (enum-junk e)
                   (read-nonempty indentation e line prev)))))))))

(defun read-nonempty (indent e s &optional prev)
  (cond
    ((>= indent (+ prev 4))
     (enum-push e (list indent s nil))
     (read-pre prev e))
    ((char= #\# (char s 0))
     (read-heading s))
    ((ulistp s)
     (push-remainder indent s e)
     (read-ul indent e))
    ((olistp s)
     (push-remainder indent s e)
     (read-ol indent e))
    ((and (char= #\> (char s 0))
          (or (snd-is-space-p s)
              (string= s ">")))
     (enum-push e (list indent s nil))
     (read-quote indent e))
    (t
     (let* ((some (enum-peek e))
            (l (second some)))
       (cond
         ((or (all #\= l)
              (all #\- l))
          (enum-junk e)
          (list :heading (list (hlevel l) (parse-text s))))
         (t
          (enum-push e (list indent s nil))
          (read-normal prev e)))))))

(defun all (c s)
  (every (lambda (char) (char= char c)) s))

(defun hlevel (l)
  (case (char l 0)
    (#\= 1)
    (#\- 2)
    (otherwise
     (error "invalid argument"))))

;; TODO: Remove cl-ppcre dependency
(defparameter *olist-re* "^[*+-]+[ ]+")
(defparameter *ulist-re* "^[0-9]+\\.[ ]+")

(defun matches-re (re s)
  (ppcre:scan re s))

(defun match-end (re s)
  (multiple-value-bind (start end)
      (ppcre:scan re s)
    (declare (ignore start))
    end))

(defun olistp (s)
  (matches-re *olist-re* s))

(defun ulistp (s)
  (matches-re *ulist-re* s))

(defun olist-offset (s)
  (match-end *olist-re* s))

(defun ulist-offset (s)
  (match-end *ulist-re* s))

;;; parser.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
