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

(declaim (optimize debug))

(defun file->string (filename)
  (with-open-file (in filename :external-format :utf-8)
    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)
      buf)))

;;; enum library

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

;;; Macros

(defmacro if-bind ((var expr) form1 &optional form2)
  `(let ((,var ,expr))
     (if ,var
         ,form1
         ,form2)))

;;; Line

(defclass line ()
  ((indent :initarg :indent
           :initform 0
           :accessor indent)
   (line :initarg :line
         :initform ""
         :accessor line)
   (blankp :initarg :blankp
           :initform nil
           :accessor blankp)))

(defun enum-push-line (enum indent line blankp)
  (let ((item (make-instance 'line :indent indent
                                   :line line
                                   :blankp blankp)))
    (enum-push enum item)))

;;; Parser

(defun collect (f x)
  (loop for r = (funcall f x)
        while r
        collect r))

(defun lines (s)
  (with-input-from-string (in s)
    (loop as line = (read-line in nil)
          while line
          collect line)))

(defun parse (s)
  (parse-lines (lines s)))

(defun parse-lines (ls)
  (parse-enum (make-enum ls)))

(defun parse-enum (e)
  (collect (lambda (e) (read-paragraph 0 e))
    (enum-map (lambda (l) (let ((l^ (string-strip l)))
                       (make-instance 'line :indent (indentation l)
                                            :line l^
                                            :blankp (string= l^ ""))))
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
    (typecase some
      (null nil)
      (line
       (if (blankp some)
           (progn
             (enum-junk e)
             (read-paragraph indent e prev))
           (when (>= (indent some) indent)
             (enum-junk e)
             (read-nonempty (indent some) e (line some) prev)))))))

(defun read-nonempty (indent e s &optional prev)
  (cond
    ((>= indent (+ prev 4))
     (enum-push-line e indent s nil)
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
     (enum-push-line e indent s nil)
     (read-quote indent e))
    (t
     (let* ((some (enum-peek e)))
       (cond
         ((and some
               (or (all #\= (line some))
                   (all #\- (line some))))
          (enum-junk e)
          (list :heading (hlevel (line some)) (parse-text s)))
         (t
          (enum-push-line e indent s nil)
          (read-normal prev e)))))))

(defun all (c s)
  (and (not (string= s ""))
       (every (lambda (char) (char= char c)) s)))

(defun hlevel (l)
  (case (char l 0)
    (#\= 1)
    (#\- 2)
    (otherwise
     (error "invalid argument"))))

;; TODO: Remove cl-ppcre dependency
(defparameter *olist-re* "^[0-9]+\\.[ ]+")
(defparameter *ulist-re* "^[*+-]+[ ]+")

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

(defun read-pre (prev e)
  (labels ((%loop (lines e)
             (let ((some (enum-peek e)))
               (if (and some
                        (>= (indent some) (+ prev 4)))
                   (progn
                     (enum-junk e)
                     (%loop (cons (concatenate
                                   'string
                                   (make-string (- (indent some) prev 4)
                                                :initial-element #\Space)
                                   (line some))
                                  lines)
                            e))
                   `(:pre ,(format nil "~{~a~^~%~}"
                                   (reverse (cons "" lines))))))))
    (%loop nil e)))

(defun read-heading (s)
  (let* ((s^ (string-trim '(#\#) s))
         (level (- (length s) (length s^))))
    (list :heading level (parse-text s^))))

(defun push-remainder (indent s e &optional (first 2))
  (let* ((s (subseq s first))
         (s^ (string-strip s)))
    (enum-push-line e (+ indent first (indentation s)) s^ (string= s^ ""))))

(defun read-ul (indent e)
  (read-list (lambda (f o) `(:ulist ,f ,@o))
             #'ulist-offset indent e))

(defun read-ol (indent e)
  (read-list (lambda (f o) `(:olist ,f ,@o))
             #'olist-offset indent e))

(defun read-list (f item-indent indent e)
  (labels ((read-item (indent &optional prev)
             (collect (lambda (x) (read-paragraph indent x prev)) e))
           (read-all (fst others)
             (skip-blank-line e)
             (let ((some (enum-peek e)))
               (if (and some
                        (>= (indent some) indent))
                   (let ((indent^ (indent some))
                         (s (line some)))
                     (if-bind (n (funcall item-indent s))
                              (progn
                                (enum-junk e)
                                (push-remainder indent^ s e n)
                                (read-all fst
                                          (cons (read-item (1+ indent^) (+ indent^ n)) others)))
                              (funcall f fst (reverse others))))
                   (funcall f fst others)))))
    (read-all (read-item (1+ indent) 10000) nil)))

(defun skip-blank-line (e)
  (let ((some (enum-peek e)))
    (when (and some
               (blankp some))
      (enum-junk e)
      (skip-blank-line e))))

(defun snd-is-space-p (s)
  (and (> (length s) 1)
       (char= (char s 1) #\Space)))

(defun read-quote (indent e)
  (labels ((push-and-finish (e elm)
             (enum-push e elm)
             (signal "no-more-elements"))
           (next-without-lt (item)
             (if (blankp item)
                 (push-and-finish e item)
                 (if (or (< (indent item) indent)
                         (not (char= (char (line item) 0) #\>)))
                     (push-and-finish e item)
                     (let* ((s (subseq (line item) 1))
                            (s^ (string-strip s)))
                       (make-instance 'line :indent (- (length s) (length s^))
                                            :line s^
                                            :blankp (string= s^ "")))))))
    (let ((enum (collect (lambda (e) (read-paragraph 0 e))
                  (enum-map #'next-without-lt e))))
      (if enum
          (list :quote (items enum))))))

(defun read-normal (prev e)
  (labels ((gettxt (prev ls)
             (labels ((ret ()
                        (format nil "~{~a~^ ~}" (reverse ls))))
               (let ((some (enum-peek e)))
                 (if (or (not some)
                         (blankp some))
                     (ret)
                     (cond
                       ((>= (indent some) (+ prev 4)) (ret))
                       ((and (or (char= (char (line some) 0) #\#)
                                 (char= (char (line some) 0) #\>))
                             (snd-is-space-p (line some)))
                        (ret))
                       ((or (olistp (line some))
                            (ulistp (line some)))
                        (ret))
                       (t (enum-junk e)
                          (gettxt (indent some) (cons (line some) ls)))))))))
    (parse-text (gettxt prev nil))))

(defclass parse-state ()
  ((max :initarg :max
        :accessor pt-max)
   (current :initarg :current
            :accessor pt-current)
   (fragments :initarg :fragments
              :accessor pt-fragments)))

(defun make-pt (max fragments current)
  (make-instance 'parse-state :max max :current current :fragments fragments))

(defun make-fragment ()
  (make-string-output-stream))

(defun parse-text (s)
  (scan s (make-pt (length s) () (make-fragment)) 0))

(defun scan (s st n)
  (let ((max (pt-max st)))
    (labels ((delim (f d s st n)
               (delimited (lambda (fst lst)
                            (funcall f (unescape-slice s fst lst))) d s st n)))
      (if (>= n max)
          (reverse (push-current st))
          (cond
            ((char= (char s n) #\`)
             (delim (lambda (s) `(:code ,s)) "`" s st n))
            ((and (char= (char s n) #\*)
                  (< (1+ n) max)
                  (char= (char s (1+ n)) #\*))
             (delim (lambda (s) `(:bold ,s)) "**" s st n))
            ((and (char= (char s n) #\_)
                  (< (1+ n) max)
                  (char= (char s (1+ n)) #\_))
             (delim (lambda (s) `(:bold ,s)) "__" s st n))
            ((or (char= (char s n) #\*)
                 (char= (char s n) #\_))
             (delim (lambda (s) `(:emph ,s)) (subseq s n (1+ n)) s st n))
            ((char= (char s n) #\=)
             (delimited (lambda (fst lst)
                          `(:struck ,(scan s (make-pt lst () (make-fragment)) fst)))
                        "==" s st n))
            ((and (char= (char s n) #\!)
                  (matches-at s n "![" :max max))
             (maybe-link "![" (lambda (ref) `(:image ,(src ref) ,(desc ref)))
                         s st (+ n 2)))
            ((char= (char s n) #\[)
             (maybe-link "["
                         (lambda (ref)
                           (cond
                             ((and (string= (src ref) "")
                                   (string= (desc ref) ""))
                              `(:normal ""))
                             ((string= (src ref) "")
                              `(:link ,(desc ref) ,(desc ref)))
                             ((and (string= (desc ref) "")
                                   (char= (char (src ref) 0) #\#))
                              `(:anchor ,(subseq (src ref) 1)))
                             (t
                              `(:link ,(src ref) ,(desc ref)))))
                         s st (1+ n)))
            ((and (char= (char s n) #\\)
                  (< (1+ n) max))
             (write-char (char s (1+ n)) (pt-current st))
             (scan s st (+ n 2)))
            (t
             (write-char (char s n) (pt-current st))
             (scan s st (+ n 1))))))))

(defun delimited (f delim s st first)
  (let ((delim-len (length delim)))
    (labels ((scan-from-next-char ()
               (write-char (char s first) (pt-current st))
               (scan s st (1+ first))))
      (if (not (matches-at s first delim :max (pt-max st)))
          (scan-from-next-char)
          (if-bind (n (scan-past s (+ first delim-len) :delim delim :max (pt-max st)))
                   (let ((chunk (funcall f (+ first delim-len) (- n delim-len))))
                     (scan s (make-pt (pt-max st)
                                      (cons chunk (push-current st))
                                      (make-fragment))
                           n))
                   (scan-from-next-char))))))

(defun maybe-link (delim f s st n)
  (if-bind (link (scan-link s n :max (pt-max st)))
           (destructuring-bind (ref n) link
             (scan s (make-pt (pt-max st)
                              (cons (funcall f ref) (push-current st))
                              (make-fragment))
                   n))
           (progn
             (write-string delim (pt-current st))
             (scan s st n))))

(defun scan-past (s n &key delim max)
  (loop for m = n then pos
        for pos = (search delim s :start2 m)
        while (and pos (< pos max))
        when (not (char= (char s pos) #\\))
          return (+ pos (length delim))))

(defun scan-link (s n &key max)
  (if-bind (end-of-desc (scan-past s n :max max :delim "]"))
           (when (and (< end-of-desc max)
                      (char= (char s end-of-desc) #\())
             (if-bind (end-of-uri (scan-past s (1+ end-of-desc) :delim ")" :max max))
                      (list (make-ref (unescape-slice s n (1- end-of-desc))
                                      (unescape-slice s
                                                      (1+ end-of-desc)
                                                      (1- end-of-uri)))
                            end-of-uri)))))

(defun matches-at (s n delim &key (max (length s)))
  (let ((len (length delim)))
    (and (>= max (+ n len))
         (string= s delim :start1 n :end1 (+ n len)))))

(defun unescape-slice (s first last)
  (unescape (string-strip (subseq s first last))))

(defun unescape (s)
  (with-output-to-string (out)
    (let ((len (length s)))
      (loop for i from 0 below len
            do (cond
                 ((and (char= (char s i) #\\)
                       (< i (1- len)))
                  (write-char (char s (1+ i)) out))
                 (t
                  (write-char (char s i) out)))))))

(defclass ref ()
  ((src :initarg :src
        :accessor src)
   (desc :initarg :desc
         :accessor desc)))

(defun make-ref (src desc)
  (make-instance 'ref :src src :desc desc))

(defun push-current (st)
  (let ((content (get-output-stream-string (pt-current st))))
    (if (plusp (length content))
        (cons `(:normal ,content) (pt-fragments st))
        (pt-fragments st))))

;;; parser.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
