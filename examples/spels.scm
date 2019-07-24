(define (first l) (car l))
(define (second l) (car (cdr l)))
(define (third l) (car (cdr (cdr l))))

(define remove-if-not filter)

(define (push element the-list)
  (set-cdr! the-list (cons (car the-list) (cdr the-list)))
  (set-car! the-list element))

(define objects '(whiskey-bottle bucket frog chain))

(define the-map '((living-room (you are in the living room
				    of a wizards house - there is a wizard
				    snoring loudly on the couch -)
			       (west door garden)
			       (upstairs stairway attic))
		  (garden (you are in a beautiful garden -
			       there is a well in front of you -)
			  (east door living-room))
		  (attic (you are in the attic of the
			      wizards house - there is a giant
			      welding torch in the corner -)
			 (downstairs stairway living-room))))

(define object-locations '((whiskey-bottle living-room)
                           (bucket living-room)
                           (chain garden)
                           (frog garden)))

(define location 'living-room)

(define (describe-location location map)
  (car (cdr (assoc location map))))

(define (describe-path path)
    (list 'there 'is 'a (second path) 'going (first path) 'from 'here '-))

(define (describe-paths location the-map)
    (apply append (map describe-path (cddr (assoc location the-map)))))

(define (is-at obj loc obj-loc)
    (equal? (second (assoc obj obj-loc)) loc))


(define (describe-floor loc objs obj-loc)
    (apply append (map (lambda (x)
                         (list 'you 'see 'a x 'on 'the 'floor '-))
                       (remove-if-not (lambda (x)
                                        (is-at x loc obj-loc))
                                      objs))))

(define (look)
    (append (describe-location location the-map)
            (describe-paths location the-map)
            (describe-floor location objects object-locations)))

(define (walk-direction direction)
    (let ((next (assoc direction (cddr (assoc location the-map)))))
      (cond (next (set! location (third next)) (look))
	    (#t '(you cannot go that way -)))))

(define (pickup-object object)
  (cond ((is-at object location object-locations)
	 (begin
           (push (list object 'body) object-locations)
           `(you are now carrying the ,object)))
         (#t '(you cannot get that.))))

(define (inventory)
  (remove-if-not (lambda (x)
                   (is-at x 'body object-locations))
		 objects))

(define (have object)
  (member object (inventory)))

(define chain-welded #f)

(define (weld subject object)
  (cond ((and (eq? location 'attic)
              (and (eq? subject 'chain)
		   (and (eq? object 'bucket)
			(and (have 'chain)
			     (and (have 'bucket)
				  (not chain-welded))))))
	 (begin (set! chain-welded #t)
		'(the chain is now securely welded to the bucket -)))
        (#t '(you cannot weld like that -))))

(define bucket-filled #f)

(define (dunk subject object)
   (cond ((and (eq? location 'garden)
               (and (eq? subject 'bucket)
		    (and (eq? object 'well)
			 (and (have 'bucket)
			      chain-welded))))
          (begin (set! bucket-filled #t)
		 '(the bucket is now full of water)))
         (#t '(you cannot dunk like that -))))

(define (splash subject object)
  (cond ((and (eq? location 'living-room)
              (and (eq? subject 'wizard)
                   (and (eq? object 'bucket)
			(have 'bucket))))
	 (cond ((not bucket-filled) '(the bucket has nothing in it -))
	       ((have 'frog) '(the wizard awakens and sees that you stole
				   his frog - 
				   he is so upset he banishes you to the 
				   netherworlds - you lose! the end -))
	       (#t '(the wizard awakens from his slumber and greets you
			 warmly - 
			 he hands you the magic low-carb donut - you win!
			 the end -))))
	 (#t '(you cannot dunk like that -))))
