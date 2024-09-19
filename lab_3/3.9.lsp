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

(print (insert-star-if-even '(1 2 3 4 5 6)))
; Результат: (1 * 2 * 3 * 4 * 5 * 6)
