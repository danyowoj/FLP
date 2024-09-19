(defun set-intersection (set1 set2)
  (cond
  ; Если хотя бы одно из множеств пусто, пересечение также пусто, поэтому функция возвращает nil.
    ((or (null set1) (null set2)) nil)
  
  ;Если первый элемент set1 присутствует в set2, мы добавляем его в результирующее множество 
    ((member (car set1) set2)
  ;  и рекурсивно вызываем функцию set-intersection с оставшейся частью set1 и set2 без этого элемента.
     (cons (car set1) (set-intersection (cdr set1) (remove (car set1) set2))))
     
  ; Если первый элемент set1 не присутствует в set2, мы рекурсивно вызываем функцию set-intersection 
  ; с оставшейся частью set1 и неизменным set2.
    (t (set-intersection (cdr set1) set2))))


(print (set-intersection '() '()))
; => NIL

(print (set-intersection '(2 3 4) '(1 2 3)))
; => (2 3)

(print (set-intersection '(1 5 6) '(3 4 5)))
; => (5)