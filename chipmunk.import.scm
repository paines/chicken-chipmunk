;;;; chipmunk.import.scm - GENERATED BY CHICKEN 4.8.0.6 -*- Scheme -*-

(eval '(import chicken scheme foreign lolevel srfi-1 srfi-4))
(##sys#register-compiled-module
  'chipmunk
  (list)
  '((infinity . chipmunk#infinity)
    (space-shape-query . chipmunk#space-shape-query)
    (space-bb-query . chipmunk#space-bb-query)
    (segment-query-hit-point . chipmunk#segment-query-hit-point)
    (space-segment-query . chipmunk#space-segment-query)
    (shape-segment-query . chipmunk#shape-segment-query)
    (seqment-query-info-normal . chipmunk#seqment-query-info-normal)
    (segment-query-info-normalized-distance
      .
      chipmunk#segment-query-info-normalized-distance)
    (segment-query-info-shape . chipmunk#segment-query-info-shape)
    (space-nearest-point-query-nearest
      .
      chipmunk#space-nearest-point-query-nearest)
    (space-nearest-point-query . chipmunk#space-nearest-point-query)
    (shape-nearest-point-query . chipmunk#shape-nearest-point-query)
    (nearest-point-query-info-closest-gradient-distance
      .
      chipmunk#nearest-point-query-info-closest-gradient-distance)
    (nearest-point-query-info-distance
      .
      chipmunk#nearest-point-query-info-distance)
    (nearest-point-query-info-closest-point
      .
      chipmunk#nearest-point-query-info-closest-point)
    (nearest-point-query-info-shape . chipmunk#nearest-point-query-info-shape)
    (arbiter-total-kinetic-energy . chipmunk#arbiter-total-kinetic-energy)
    (arbiter-total-impuse . chipmunk#arbiter-total-impuse)
    (arbiter-total-impuse-with-friction
      .
      chipmunk#arbiter-total-impuse-with-friction)
    (contact-point-set-distance-ref . chipmunk#contact-point-set-distance-ref)
    (contact-point-set-normal-ref . chipmunk#contact-point-set-normal-ref)
    (contact-point-set-point-ref . chipmunk#contact-point-set-point-ref)
    (contact-point-set-length . chipmunk#contact-point-set-length)
    (arbiter-get-bodies . chipmunk#arbiter-get-bodies)
    (arbiter-get-shapes . chipmunk#arbiter-get-shapes)
    (arbiter-first-contact? . chipmunk#arbiter-first-contact?)
    (arbiter-depth . chipmunk#arbiter-depth)
    (arbiter-point . chipmunk#arbiter-point)
    (arbiter-normal . chipmunk#arbiter-normal)
    (arbiter-count . chipmunk#arbiter-count)
    (arbiter-surface-velocity . chipmunk#arbiter-surface-velocity)
    (arbiter-friction . chipmunk#arbiter-friction)
    (arbiter-elasticity . chipmunk#arbiter-elasticity)
    (simple-motor-rate . chipmunk#simple-motor-rate)
    (create-simple-motor . chipmunk#create-simple-motor)
    (gear-joint-ratio . chipmunk#gear-joint-ratio)
    (gear-joint-phase . chipmunk#gear-joint-phase)
    (create-gear-joint . chipmunk#create-gear-joint)
    (ratchet-joint-ratchet . chipmunk#ratchet-joint-ratchet)
    (ratchet-joint-phase . chipmunk#ratchet-joint-phase)
    (ratchet-joint-angle . chipmunk#ratchet-joint-angle)
    (create-ratchet-joint . chipmunk#create-ratchet-joint)
    (rotary-limit-joint-max . chipmunk#rotary-limit-joint-max)
    (rotary-limit-joint-min . chipmunk#rotary-limit-joint-min)
    (create-rotary-limit-joint . chipmunk#create-rotary-limit-joint)
    (damped-sping-joint-damping . chipmunk#damped-sping-joint-damping)
    (damped-sping-joint-stiffness . chipmunk#damped-sping-joint-stiffness)
    (damped-sping-joint-rest-length . chipmunk#damped-sping-joint-rest-length)
    (create-damped-rotary-spring . chipmunk#create-damped-rotary-spring)
    (damped-sping-joint-damping . chipmunk#damped-sping-joint-damping)
    (damped-sping-joint-stiffness . chipmunk#damped-sping-joint-stiffness)
    (damped-sping-joint-rest-length . chipmunk#damped-sping-joint-rest-length)
    (damped-sping-joint-anchor-b . chipmunk#damped-sping-joint-anchor-b)
    (damped-sping-joint-anchor-a . chipmunk#damped-sping-joint-anchor-a)
    (create-damped-spring . chipmunk#create-damped-spring)
    (groove-joint-groove-b . chipmunk#groove-joint-groove-b)
    (groove-joint-groove-a . chipmunk#groove-joint-groove-a)
    (groove-joint-anchor . chipmunk#groove-joint-anchor)
    (create-groove-joint . chipmunk#create-groove-joint)
    (pivot-joint-anchor-b . chipmunk#pivot-joint-anchor-b)
    (pivot-joint-anchor-a . chipmunk#pivot-joint-anchor-a)
    (create-pivot-joint-with-anchors
      .
      chipmunk#create-pivot-joint-with-anchors)
    (create-pivot-joint-with-pivot . chipmunk#create-pivot-joint-with-pivot)
    (slide-joint-max . chipmunk#slide-joint-max)
    (slide-joint-min . chipmunk#slide-joint-min)
    (slide-joint-anchor-b . chipmunk#slide-joint-anchor-b)
    (slide-joint-anchor-a . chipmunk#slide-joint-anchor-a)
    (create-slide-joint . chipmunk#create-slide-joint)
    (pin-joint-distance . chipmunk#pin-joint-distance)
    (pin-joint-anchor-b . chipmunk#pin-joint-anchor-b)
    (pin-joint-anchor-a . chipmunk#pin-joint-anchor-a)
    (create-pin-joint . chipmunk#create-pin-joint)
    (constraint-impulse . chipmunk#constraint-impulse)
    (constraint-space . chipmunk#constraint-space)
    (constraint-max-bias . chipmunk#constraint-max-bias)
    (constraint-error-bias . chipmunk#constraint-error-bias)
    (constraint-max-force . chipmunk#constraint-max-force)
    (constraint-b . chipmunk#constraint-b)
    (constraint-a . chipmunk#constraint-a)
    (constraint-free . chipmunk#constraint-free)
    (space-add-poststep-callback . chipmunk#space-add-poststep-callback)
    (%space-default-collision-handler-set!
      .
      chipmunk#%space-default-collision-handler-set!)
    (%space-remove-collision-handler
      .
      chipmunk#%space-remove-collision-handler)
    (%space-add-collision-handler . chipmunk#%space-add-collision-handler)
    (space-use-spatital-hash . chipmunk#space-use-spatital-hash)
    (space-step . chipmunk#space-step)
    (space-each-constraint . chipmunk#space-each-constraint)
    (space-each-shape . chipmunk#space-each-shape)
    (space-each-body . chipmunk#space-each-body)
    (space-reindex-static . chipmunk#space-reindex-static)
    (space-reindex-shapes-for-body . chipmunk#space-reindex-shapes-for-body)
    (space-reindex-shape . chipmunk#space-reindex-shape)
    (space-body->dynamic . chipmunk#space-body->dynamic)
    (space-body->static . chipmunk#space-body->static)
    (space-has-constraint? . chipmunk#space-has-constraint?)
    (space-has-body? . chipmunk#space-has-body?)
    (space-has-shape? . chipmunk#space-has-shape?)
    (space-remove-constraint . chipmunk#space-remove-constraint)
    (space-remove-body . chipmunk#space-remove-body)
    (space-remove-shape . chipmunk#space-remove-shape)
    (space-add-constraint . chipmunk#space-add-constraint)
    (space-add-static-shape . chipmunk#space-add-static-shape)
    (space-add-body . chipmunk#space-add-body)
    (space-add-shape . chipmunk#space-add-shape)
    (space-static-body . chipmunk#space-static-body)
    (space-locked? . chipmunk#space-locked?)
    (space-current-time-step . chipmunk#space-current-time-step)
    (space-collision-persistence . chipmunk#space-collision-persistence)
    (space-collision-bias . chipmunk#space-collision-bias)
    (space-collision-slop . chipmunk#space-collision-slop)
    (space-sleep-time-treshold . chipmunk#space-sleep-time-treshold)
    (space-idle-speed-treshold . chipmunk#space-idle-speed-treshold)
    (space-damping . chipmunk#space-damping)
    (space-gravity . chipmunk#space-gravity)
    (space-iterations . chipmunk#space-iterations)
    (space-free . chipmunk#space-free)
    (create-space . chipmunk#create-space)
    (centroid-for-polygon . chipmunk#centroid-for-polygon)
    (centroid-for-polygon . chipmunk#centroid-for-polygon)
    (valid-polygon? . chipmunk#valid-polygon?)
    (create-box-shape . chipmunk#create-box-shape)
    (%create-box-shape . chipmunk#%create-box-shape)
    (%create-box-shape-with-radius . chipmunk#%create-box-shape-with-radius)
    (polygon-shape-radius . chipmunk#polygon-shape-radius)
    (polygon-shape-vertex-ref . chipmunk#polygon-shape-vertex-ref)
    (polygon-shape-vertex-count . chipmunk#polygon-shape-vertex-count)
    (create-polygon-shape . chipmunk#create-polygon-shape)
    (segment-shape-set-neighbors! . chipmunk#segment-shape-set-neighbors!)
    (segment-shape-radius . chipmunk#segment-shape-radius)
    (segment-shape-normal . chipmunk#segment-shape-normal)
    (segment-shape-b . chipmunk#segment-shape-b)
    (segment-shape-a . chipmunk#segment-shape-a)
    (create-segment-shape . chipmunk#create-segment-shape)
    (cicle-shape-radius . chipmunk#cicle-shape-radius)
    (cicle-shape-offset . chipmunk#cicle-shape-offset)
    (create-circle-shape . chipmunk#create-circle-shape)
    (reset-id-counter . chipmunk#reset-id-counter)
    (shape-update . chipmunk#shape-update)
    (shape-cache-bb . chipmunk#shape-cache-bb)
    (shape-free . chipmunk#shape-free)
    (shape-space . chipmunk#shape-space)
    (shape-layers . chipmunk#shape-layers)
    (shape-group . chipmunk#shape-group)
    (shape-collision-type . chipmunk#shape-collision-type)
    (shape-surface-velocity . chipmunk#shape-surface-velocity)
    (shape-friction . chipmunk#shape-friction)
    (shape-elasticity . chipmunk#shape-elasticity)
    (shape-sensor . chipmunk#shape-sensor)
    (shape-bb . chipmunk#shape-bb)
    (shape-body . chipmunk#shape-body)
    (all-layers . chipmunk#all-layers)
    (no-group . chipmunk#no-group)
    (body-world->local . chipmunk#body-world->local)
    (body-local->world . chipmunk#body-local->world)
    (area-for-polygon . chipmunk#area-for-polygon)
    (area-for-seqment . chipmunk#area-for-seqment)
    (area-for-circle . chipmunk#area-for-circle)
    (moment-for-polygon . chipmunk#moment-for-polygon)
    (moment-for-box . chipmunk#moment-for-box)
    (moment-for-seqment . chipmunk#moment-for-seqment)
    (moment-for-circle . chipmunk#moment-for-circle)
    (body-each-arbiter . chipmunk#body-each-arbiter)
    (body-each-constraint . chipmunk#body-each-constraint)
    (body-each-shape . chipmunk#body-each-shape)
    (body-sleep-with-group . chipmunk#body-sleep-with-group)
    (body-activate-static . chipmunk#body-activate-static)
    (body-roque? . chipmunk#body-roque?)
    (body-static? . chipmunk#body-static?)
    (body-sleep . chipmunk#body-sleep)
    (body-activate . chipmunk#body-activate)
    (body-sleeping? . chipmunk#body-sleeping?)
    (body-apply-impulse . chipmunk#body-apply-impulse)
    (body-apply-force . chipmunk#body-apply-force)
    (body-reset-forces . chipmunk#body-reset-forces)
    (body-space . chipmunk#body-space)
    (body-angular-velocity-limit . chipmunk#body-angular-velocity-limit)
    (body-velocity-limit . chipmunk#body-velocity-limit)
    (body-rotation . chipmunk#body-rotation)
    (body-torque . chipmunk#body-torque)
    (body-angle-velocity . chipmunk#body-angle-velocity)
    (body-angle . chipmunk#body-angle)
    (body-force . chipmunk#body-force)
    (body-velocity . chipmunk#body-velocity)
    (body-position . chipmunk#body-position)
    (body-moment . chipmunk#body-moment)
    (body-mass . chipmunk#body-mass)
    (create-static-body . chipmunk#create-static-body)
    (body-free . chipmunk#body-free)
    (create-body . chipmunk#create-body)
    (bb-wrap-vect . chipmunk#bb-wrap-vect)
    (bb-clamp-vect . chipmunk#bb-clamp-vect)
    (bb-intersects-seqment . chipmunk#bb-intersects-seqment)
    (bb-segment-query . chipmunk#bb-segment-query)
    (bb-merged-area . chipmunk#bb-merged-area)
    (bb-area . chipmunk#bb-area)
    (bb-center . chipmunk#bb-center)
    (bb-expand . chipmunk#bb-expand)
    (bb-merge . chipmunk#bb-merge)
    (bb-contains-vect? . chipmunk#bb-contains-vect?)
    (bb-contains-bb? . chipmunk#bb-contains-bb?)
    (bb-intersects? . chipmunk#bb-intersects?)
    (bb-right-top . chipmunk#bb-right-top)
    (bb-left-bottom . chipmunk#bb-left-bottom)
    (create-bb-for-circle . chipmunk#create-bb-for-circle)
    (create-bb . chipmunk#create-bb)
    (vect->angle . chipmunk#vect->angle)
    (angle->vect . chipmunk#angle->vect)
    (vect-near? . chipmunk#vect-near?)
    (vect-dist-squared . chipmunk#vect-dist-squared)
    (vect-dist . chipmunk#vect-dist)
    (vect-clamp . chipmunk#vect-clamp)
    (vect-normalize . chipmunk#vect-normalize)
    (vect-slerpconst . chipmunk#vect-slerpconst)
    (vect-slerp . chipmunk#vect-slerp)
    (vect-lerpconst . chipmunk#vect-lerpconst)
    (vect-lerp . chipmunk#vect-lerp)
    (vect-length-squared . chipmunk#vect-length-squared)
    (vect-length . chipmunk#vect-length)
    (vect-unrotate . chipmunk#vect-unrotate)
    (vect-rotate . chipmunk#vect-rotate)
    (vect-project . chipmunk#vect-project)
    (vect-rperp . chipmunk#vect-rperp)
    (vect-perp . chipmunk#vect-perp)
    (vect-cross . chipmunk#vect-cross)
    (vect-dot . chipmunk#vect-dot)
    (vect-mult . chipmunk#vect-mult)
    (vect-neg . chipmunk#vect-neg)
    (vect-sub . chipmunk#vect-sub)
    (vect-add . chipmunk#vect-add)
    (vect-=? . chipmunk#vect-=?)
    (vect-y . chipmunk#vect-y)
    (vect-x . chipmunk#vect-x)
    (vect-zero . chipmunk#vect-zero)
    (create-vect . chipmunk#create-vect)
    (cp-scale . chipmunk#cp-scale))
  (list (cons '%define-chipmunk-foreign-properties
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
                                               (c-setter
                                                 (if (null? (cddr (cadr form)))
                                                   #f
                                                   (caddr (cadr form)))))
                                           `(define ,(symbol-append class '- name)
                                              (getter-with-setter
                                                (%cm-lambda ,value-type ,c-getter ,type)
                                                ,(if c-setter
                                                   `(%cm-lambda void ,c-setter ,type ,value-type)
                                                   `(lambda _
                                                      (error ,(sprintf "~a-~a is read only." class name))))))))
                                       forms))))
                         e))))
        (cons '%define-chipmunk-foreign-methods
              (er-macro-transformer
                (lambda (e r c?)
                  (apply (lambda (_ class-type-pair #!rest forms)
                           (let ((class (car class-type-pair)))
                             `(begin
                                ,@(map (lambda (form)
                                         (let ((name (car form))
                                               (c-name (car (cadr form)))
                                               (ret-type (cadr (cadr form)))
                                               (arg-types
                                                 (cons (cadr class-type-pair) (cddr (cadr form)))))
                                           `(,(r 'define)
                                             ,(symbol-append class '- name)
                                             (,(r '%cm-lambda) ,ret-type ,c-name ,@arg-types))))
                                       forms))))
                         e))))
        (cons '%cm-lambda
              (er-macro-transformer
                (lambda (e r c?)
                  (apply (lambda (_ ret-type func-name #!rest arg-types)
                           (let ((ret (gensym))
                                 (arg-pairs
                                   (map (lambda (a) (list a (gensym))) arg-types)))
                             (define (convert-arg-pair arg-pair)
                               `(,(case (car arg-pair)
                                    ((vect bb) 'f64vector)
                                    (else (car arg-pair)))
                                 ,(cadr arg-pair)))
                             (define (convert-arg-pairs)
                               (append
                                 (map convert-arg-pair arg-pairs)
                                 (case ret-type
                                   ((vect bb) `((f64vector ,ret)))
                                   (else (list)))))
                             (define (comma-seperate strings)
                               (if (null? strings)
                                 ""
                                 (apply string-append
                                        (cons (car strings)
                                              (map (lambda (s) (string-append ", " s))
                                                   (cdr strings))))))
                             (define (create-c-call)
                               (sprintf
                                 "~a(~a)"
                                 func-name
                                 (comma-seperate
                                   (map (lambda (arg-pair)
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
                                    "cpVect r = ~a;~%\n\t\t\tmemcpy(~a, &r, sizeof(cpVect));~%"
                                    (create-c-call)
                                    ret))
                                 ((bb)
                                  (sprintf
                                    "cpBB r = ~a;~%\n\t\t\tmemcpy(~a, &r, sizeof(cpBB));~%"
                                    (create-c-call)
                                    ret))
                                 ((void) (sprintf "~a;" (create-c-call)))
                                 (else (sprintf "C_return(~a);" (create-c-call)))))
                             (case ret-type
                               ((vect bb)
                                `(,(r 'lambda)
                                  (,@(map cadr arg-pairs))
                                  (,(r 'let)
                                   ((,ret
                                     (make-f64vector
                                       ,(case ret-type ((vect) 2) ((bb) 4)))))
                                   ((,(r 'foreign-lambda*)
                                     ,'void
                                     ,(convert-arg-pairs)
                                     ,(create-c-body))
                                    ,@(append (map cadr arg-pairs) (list ret)))
                                   ,ret)))
                               (else
                                `(,(r 'foreign-lambda*)
                                  ,ret-type
                                  ,(convert-arg-pairs)
                                  ,(create-c-body))))))
                         e))))
        (cons 'vect-list->f64vector
              (syntax-rules
                ()
                ((_ vects)
                 (list->f64vector (append-map f64vector->list vects))))))
  (list))

;; END OF FILE
