;;
;; Freo reorders Fortran indicies between column major and row major.
;;
;; Copyright (c) 2011 Charles O'Neill (ocharle@gmail.com)
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

(require-extension srfi-13 files extras)

(define (first-paren str start)
  (define (bal-paren-iter str index depth)
    (cond ((= index (string-length str))  #f) ;; Stop if end of line
    ((char=? (string-ref str index) #\() ;; Open Paren
           index)
    (else (bal-paren-iter str (+ index 1) depth))))
  (bal-paren-iter str start 0))

(define (balanced-paren str start)
  (define (bal-paren-iter str index depth)
    (cond ((= index (string-length str))  #f) ;; Stop if end of line
    ((char=? (string-ref str index) #\() ;; Open Paren
     (bal-paren-iter str (+ index 1) (+ depth 1)))
    ((char=? (string-ref str index) #\)) ;; Closed Paren
     (if (= depth 1)
         index
         (bal-paren-iter str (+ index 1) (- depth 1))))
    (else (bal-paren-iter str (+ index 1) depth))))
  (bal-paren-iter str start 0))

(define (comma-break str start)
  (define (bal-paren-iter str index depth)
    (cond ((= index (string-length str))  #f) ;; Stop if end of line
    ((char=? (string-ref str index) #\( ) ;; Open Paren
     (bal-paren-iter str (+ index 1) (+ depth 1)))
    ((char=? (string-ref str index) #\) ) ;; Closed Paren
     (bal-paren-iter str (+ index 1) (- depth 1)))
    ((char=? (string-ref str index) #\, ) ;; Comma
     (if (= depth 1)
         index
         (bal-paren-iter str (+ index 1) depth )))
    (else (bal-paren-iter str (+ index 1) depth))))
  (bal-paren-iter str start 0))

(define (triple-positions str start)
  (list (first-paren str start) (comma-break str start) (balanced-paren str start)))


(define (swap-str str loc)
  (define a (substring str (+ (first loc) 1) (second loc) ))
  (define b (substring str (+ (second loc) 1) (third loc) ))
  (define c ",")
  (display ".")
  (string-replace str (string-concatenate (list b c a))
      (+ (first loc) 1)
      (third loc) ))

(define (worth-looking? str array-name start)
  (string-contains str array-name start))

(define (find-all-name str name)
  (define (find-iter str name start)
    (let ((current (worth-looking? str name start)))
      (if current
    (cons current (find-iter str name (+ current 1)))
    '())))
  (find-iter str name 0))

 (define (swap-line str name )
   (define name-paren (string-concatenate (list name "(")))
   (define locations (find-all-name str name-paren))
   (define (swap-line-iter str name-paren loc)
     (if (null? loc)
   str
   (swap-line-iter (swap-str str (triple-positions str (car loc))) name-paren (cdr loc))))
   (swap-line-iter str name-paren locations))

(define (main)
  (if (< (length (argv)) 3)
      (inform-user)
      (begin
  (define (op-file file)
    (begin
      (newline)
      (display file)
      (operate (second (argv)) file)))
  (define file-list
    (cdr (cdr (argv))))
  (for-each op-file file-list)))
  (newline))


(define (operate function file)
  (begin
    ;;(file-copy file (string-concatenate (list file "OLD")))
    (define input (read-lines file))
    (define output (open-output-file file))
    (define (operate-name lis)
      (format output "~a\n" (swap-line lis function)))
    (for-each operate-name input)
    (close-output-port output)))

(define (inform-user)
  (format #t " \n")
  (format #t " Freo reorders Fortran indicies between column major and row major.\n No longer is that cursed C programmer ruining the efficiency \n of your beautiful Fortran number cruncher. \n")
  (format #t "     Useage: freo <function> <files> \n")
  (format #t "     Effect: function(i,j) -> function(j,i) \n\n"))

(main)
