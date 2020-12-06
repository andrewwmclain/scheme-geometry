(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (make-point x-cor y-cor)
        (cons x-cor y-cor)
)

(define (get-x point)
        (car point)
)

(define (get-y point)
         (cdr point)
)

(define (is-line point1 point2 point3)
        (cond
            (           
            (   =
                (*(- (get-y point3) (get-y point2))
                (- (get-x point2) (get-x point1)))

                (*(- (get-y point2) (get-y point1))
                (- (get-x point3) (get-x point2)))
            )            
            #t            
            )
            (else #f)
        )
)

(define (distance point1 point2)
        (sqrt   
            (+
                (expt (- (get-x point2) (get-x point1)) 2)
                (expt (- (get-y point2) (get-y point1)) 2)
            )
        )
)


(define (perimeter point1 point2 point3)
        (+
        (sqrt   
            (+
                (expt (- (get-x point2) (get-x point1)) 2)
                (expt (- (get-y point2) (get-y point1)) 2)
            )
        )
        (sqrt   
            (+
                (expt (- (get-x point3) (get-x point1)) 2)
                (expt (- (get-y point3) (get-y point1)) 2)
            )
        )
        (sqrt   
            (+
                (expt (- (get-x point3) (get-x point2)) 2)
                (expt (- (get-y point3) (get-y point2)) 2)
            )
        )   
        )   
)

(define (area point1 point2 point3)
    (let(
            (s (/ (perimeter point1 point2 point3) 2))
        ) 
        (sqrt(
            *
            s
            (- s (distance point1 point2))
            (- s (distance point1 point3))
            (- s (distance point2 point3))
            )
        )
    )
)


(define (calculate-triangle point1 point2 point3)
        (let(
            (a (distance point1 point2))
            (b (distance point1 point3))
            (c (distance point2 point3))
        )
          (display "Side 1 = ")
          (display (round-off a 3))
          (newline)
          (display "Side 2 = ")
          (display (round-off b 3))
          (newline)
          (display "Side 3 = ")
          (display (round-off c 3)) 
          (newline)
          (display "Perimeter = ")
          (display (round-off 
                    (perimeter point1 point2 point3)
                    3
                   )
          )
          (newline)
          (display "Area = ")
          (display 
                    (round-off (
                        area point1 point2 point3                    
                        )
                        1
                    )
          )
          (newline)
          (display "Angle 1 = ")
          (display 
             (   round-off 
                 (acos ( / ( - (+ (expt b 2) (expt c 2)) (expt a 2)) (* 2 b c) ))
                 5
             )
          )
          (display "   ")
          (display 
             (round-off
              ( *
                 (/ 180 3.141592653)
                 (acos ( / ( - (+ (expt b 2) (expt c 2)) (expt a 2)) (* 2 b c)) ) 
              )            
              5            
             )
          )
          (newline)
          (display "Angle 2 = ")
          (display 
            (round-off
             ( acos ( / ( - (+ (expt c 2) (expt a 2)) (expt b 2)) (* 2 c a) ) )            
             5
            )
          )
          (display "   ")
          (display 
             (round-off
              ( *
                 (/ 180 3.141592653)
                 (acos ( / ( - (+ (expt c 2) (expt a 2)) (expt b 2)) (* 2 c a) )) 
              )            
              5
             )
          )
          (newline)
          (display "Angle 3 = ")
          (display 
            (round-off
                ( -
                    3.141592653
                    (acos ( / ( - (+ (expt b 2) (expt c 2)) (expt a 2)) (* 2 b c)))     
                    (acos ( / ( - (+ (expt c 2) (expt a 2)) (expt b 2)) (* 2 c a)))                  
                )            
             5
            )
          )
          (display "   ")
          (display
             (round-off
              ( *
                 (/ 180 3.141592653)
                 (- 3.141592653
                    (acos ( / ( - (+ (expt b 2) (expt c 2)) (expt a 2)) (* 2 b c)))     
                    (acos ( / ( - (+ (expt c 2) (expt a 2)) (expt b 2)) (* 2 c a)))
                 ) 
              )            
              5
             ) 
          )
          (newline)
          (newline)
        )
)
