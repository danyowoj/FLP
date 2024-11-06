; Определите предикат, проверяющий равенство двух множеств.

(defun equal-sets (set1 set2)
  (cond
  ; Базовый случай: если оба множества пусты, то они равны, поэтому возвращаем t.
    ((and (null set1) (null set2)) t)
  
  ; Если одно из множеств пусто, а другое - нет, то они не равны, поэтому возвращаем nil.
    ((or (null set1) (null set2)) nil) 
  
  ; Если первый элемент первого множества присутствует во втором множестве, 
    ((member (car set1) set2) 
  ; то мы рекурсивно вызываем функцию, передавая хвост первого множества и второе множество, из которого мы удалили этот элемент.
     (equal-sets (cdr set1) (remove (car set1) set2))) 
  
  ; Если первый элемент первого множества не присутствует во втором множестве, то множества не равны, поэтому возвращаем nil.
    (t nil))) 

(print (equal-sets '(1 2 3) '(3 2 1)))
; => t

(print (equal-sets '(1 2 3) '(1 2 4)))
; => nil

; --------------------------------------------------------------------------------------------------------------

; Определите функцию, возвращающую пересечение двух множеств.

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

; --------------------------------------------------------------------------------------------------------------

; Определите функционал, вставляющий перед каждым элементом списка, обладающим определенным свойством, символ *. 
; Проверьте работу функционала для предикатов:
;     - неотрицательное число (функциональный аргумент – лямбда выражение);
;     - четное число (функциональный аргумент – имя встроенного предиката evenp).

; Функция insert-star-if принимает два аргумента: 
;   list - исходный список
;   predicate - предикат, который определяет, перед каким элементом списка необходимо вставить символ *
(defun insert-star-if (list predicate)
  (cond
  ; Функция использует рекурсивный подход: если список пуст, возвращается пустой список;
    ((null list) nil)
    
  ; если предикат возвращает true для текущего элемента,
    ((funcall predicate (car list))
  ; то перед ним вставляется символ * и рекурсивно вызывается функция для оставшейся части списка;
     (cons '* (cons (car list) (insert-star-if (cdr list) predicate))))
  ; если предикат возвращает false, то текущий элемент просто добавляется в результирующий список.
    (t (cons (car list) (insert-star-if (cdr list) predicate)))))

; Функции insert-star-if-non-negative и insert-star-if-even являются вспомогательными 
; и используют insert-star-if с соответствующими предикатами.
(defun insert-star-if-non-negative (list)
  (insert-star-if list #'(lambda (x) (>= x 0))))

(defun insert-star-if-even (list)
  (insert-star-if list #'evenp))


(print (insert-star-if-non-negative '(-1 0 1 2 -3 4)))
; Результат: (-1 * 0 * 1 * 2 -3 * 4)

(insert-star-if '(-1 0 1 2 -3 4) #'(lambda (x) (>= x 0)))

(print (insert-star-if-even '(1 2 3 4 5 6)))
; Результат: (1 * 2 3 * 4 5 * 6) 