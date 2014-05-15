#|
Copyright (c) 2014 Richard van Roy (pluizer _at_ freeshell _dot_ de)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#


(define-syntax vect-list->f64vector
  (syntax-rules ()
    ((_  vects)
     (list->f64vector (append-map f64vector->list vects)))))

; Chipmunk often passes or returns structures of type cpVect and cpBB by
; value. This functions works just like foreign-lambda but adds the types
; 'vect' and 'bb' for passing or returning those structures.
(define-syntax %cm-lambda
  (er-macro-transformer
   (lambda (e r c?)
     (apply (lambda (_ ret-type func-name #!rest arg-types)

              (let ((ret (gensym))
                    (arg-pairs (map (lambda (a) (list a (gensym))) arg-types)))
                
                (define (convert-arg-pair arg-pair)
                  `(,(case (car arg-pair)
                       ((vect bb) 'f64vector)
                       (else (car arg-pair)))
                    ,(cadr arg-pair)))
                
                (define (convert-arg-pairs)
                  (append (map convert-arg-pair arg-pairs)
                          (case ret-type
                               ((vect bb) `((f64vector ,ret)))
                               (else (list)))))

                (define (comma-seperate strings)
                  (if (null? strings) ""
                   (apply string-append
                          (cons (car strings)
                                (map (lambda (s)
                                       (string-append ", " s))
                                     (cdr strings))))))

                (define (create-c-call)
                  (sprintf "~a(~a)"
                           func-name
                           (comma-seperate
                            (map
                             (lambda (arg-pair)
                               (string-append
                                (case (car arg-pair)
                                  ((vect) "*(cpVect*)")
                                  ((bb) "*(cpBB*)")
                                  (else ""))
                                (symbol->string (cadr arg-pair))))
                             arg-pairs))))

                (define (create-c-body)
                  (case ret-type
                    ((vect)
                     (sprintf
                      "cpVect r = ~a;~%
			memcpy(~a, &r, sizeof(cpVect));~%"
                      (create-c-call) ret))
                    ((bb)
                     (sprintf
                      "cpBB r = ~a;~%
			memcpy(~a, &r, sizeof(cpBB));~%"
                      (create-c-call) ret))
                    ((void)
                     (sprintf "~a;" (create-c-call)))
                    (else
                     (sprintf "C_return(~a);" (create-c-call)))))

                (case ret-type
                  ((vect bb)
                   `(,(r 'lambda) (,@(map cadr arg-pairs))
                     (,(r 'let) ((,ret (make-f64vector ,(case ret-type ((vect) 2) ((bb) 4)))))
                      ((,(r 'foreign-lambda*) ,'void ,(convert-arg-pairs) ,(create-c-body))
                       ,@(append (map cadr arg-pairs) (list ret)))
                      ,ret)))
                  (else
                   `(,(r 'foreign-lambda*) ,ret-type ,(convert-arg-pairs) ,(create-c-body))))))
            e))))

(define-syntax %define-chipmunk-foreign-methods
  (er-macro-transformer
   (lambda (e r c?)
     (apply (lambda (_ class-type-pair #!rest forms)
              (let ((class (car class-type-pair)))
                `(begin
                   ,@(map (lambda (form)
                            (let ((name (car form))
                                  (c-name (car (cadr form)))
                                  (ret-type (cadr (cadr form)))
                                  (arg-types (cons (cadr class-type-pair)
                                                   (cddr (cadr form)))))
                              `(,(r 'define) ,(symbol-append class '- name)
                                (,(r '%cm-lambda) ,ret-type ,c-name ,@arg-types))))
                          forms))))

            e))))

(define-syntax %define-chipmunk-foreign-properties
  (er-macro-transformer
   (lambda (e r c?)
     (apply (lambda (_ class-type-pair #!rest forms)
              (let ((class (car class-type-pair))
                    (type (cadr class-type-pair)))

                `(begin
                   ,@(map (lambda (form)
                            (let ((name (car form))
                                  (value-type (car (cadr form)))
                                  (c-getter (cadr (cadr form)))
                                  (c-setter (if (null? (cddr (cadr form))) #f
                                                (caddr (cadr form)))))
                              `(define ,(symbol-append class '- name)
                                 (getter-with-setter
                                  (%cm-lambda ,value-type ,c-getter ,type)
                                  ,(if c-setter
                                       `(%cm-lambda void ,c-setter ,type ,value-type)
                                       `(lambda _ (error ,(sprintf "~a-~a is read only." class name)))
                                       )))))
                          forms))))
            e))))
