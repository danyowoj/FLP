; 1. Добавляющую заданный параметром символ после каждого неотрицательного 
; элемента списка. 
; Например, x=*, L=(-1 d 6 -3 a 0) –> (-1 d (6 *) -3 a (0 *))

(defun add-symbol-after-nonneg (lst symbol)
  (cond
    ;; Если список пустой, вернуть пустой список
    ((null lst) nil)
    
    ;; Если первый элемент списка неотрицательный (>= 0), добавить символ после него
    ((and (numberp (car lst)) (>= (car lst) 0))
     (cons (list (car lst) symbol) (add-symbol-after-nonneg (cdr lst) symbol)))
    
    ;; В остальных случаях просто добавляем элемент в результат и продолжаем рекурсию
    (t (cons (car lst) (add-symbol-after-nonneg (cdr lst) symbol)))))

(print(add-symbol-after-nonneg '(-1 d 6 -3 a 0) '*)) ; out = (-1 d (6 *) -3 a (0 *))
; --------------------------------------------------------------------------------------------------
; 2. Объединяющую 2 списка в один, чередуя элементы списков.
; Например, L1=(1 2 3 4 5 6 7 8), L2=(a s d f) –> (1 a 2 s 3 d 4 f 5 6 7 8). 

(defun alternate-lists (list1 list2)
  (cond
    ;; Если оба списка пустые, вернуть пустой список
    ((and (null list1) (null list2)) nil)

    ;; Если первый список пуст, возвращаем второй
    ((null list1) list2)

    ;; Если второй список пуст, возвращаем первый
    ((null list2) list1)

    ;; В остальных случаях объединяем первый элемент из обоих списков и продолжаем чередование
    (t (cons (car list1) 
             (cons (car list2) 
                   (alternate-lists (cdr list1) (cdr list2)))))))

(print (alternate-lists '(1 2 3 4 5 6 7 8) '(a s d f))) ; out = (1 a 2 s 3 d 4 f 5 6 7 8)
; --------------------------------------------------------------------------------------------------
; 3. Формирующую список, состоящий из сумм первого и последнего, второго и 
; предпоследнего элементов числового списка и т.д. Каждый элемент должен 
; участвовать в сложении не более одного раза.
; Например, (1 -2 -3 4 5 6 -7 8 9) –> (10 6 -10 10 5).

(defun sum-pairs (lst)
  (cond
    ;; Если список пустой, возвращаем пустой список
    ((null lst) nil)

    ;; Если в списке остался один элемент, возвращаем этот элемент (середина списка)
    ((null (cdr lst)) (list (car lst)))

    ;; В остальных случаях складываем первый и последний элемент
    ;; и вызываем функцию рекурсивно для подсписка без первого и последнего элементов
    (t (cons (+ (car lst) (car (last lst))) 
             (sum-pairs (butlast (cdr lst)))))))
(print (sum-pairs '(1 -2 -3 4 5 6 -7 8 9))) ; out = (10 6 -10 10 5)



