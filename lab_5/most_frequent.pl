most_frequent(List, MostFrequent) :-
  count_occurrences(List, Counts),
  find_max_count(Counts, MaxCount),
  get_most_frequent(Counts, MaxCount, MostFrequent).


% Базовый случай: если список пуст, то список подсчетов тоже пуст.
count_occurrences([], []).
% Рекурсивное определение подсчета вхождений.
% Разбирает список по голове (H) и хвосту (T).
count_occurrences([H|T], Counts) :-
  % Рекурсивно обрабатывает хвост списка.
  count_occurrences(T, Counts0),
  % Проверяет, существует ли уже пара (H, C) в Counts0. 
  % Если да (member), оператор ! (cut) предотвращает дальнейшее backtracking, повышая эффективность.
  member((H, C), Counts0), !,
  % Значение C инкрементируется (NewC is C + 1)
  NewC is C + 1,
  % пара заменяется на (H, NewC) с помощью replace
  replace((H, C), (H, NewC), Counts0, Counts).

% Если пара (H, C) не найдена, добавляет новую пару (H, 1) в начало списка Counts
count_occurrences([H|T], [(H, 1)|Counts]) :-
  count_occurrences(T, Counts).


% Заменяет Pair на NewPair в списке
% Pair находится в начале списка
replace(Pair, NewPair, [Pair|Rest], [NewPair|Rest]).

% Рекурсивная замена Pair на NewPair  в остальной части списка
replace(Pair, NewPair, [H|T], [H|NewT]) :-
  replace(Pair, NewPair, T, NewT).


% Рекурсивно находит максимальное значение количества вхождений
% Сравнивает текущее количество C с максимальным найденным ранее MaxCount0
find_max_count([( _, C)|Rest], MaxCount):-
    find_max_count(Rest, MaxCount0),
    (C > MaxCount0 -> MaxCount = C ; MaxCount = MaxCount0).

% Базовый случай:  если список пуст, максимальное количество равно 0
find_max_count([], 0).


% Базовый случай: если список подсчетов пуст, 
% список наиболее часто встречающихся элементов пуст
get_most_frequent([], _, []).

% Если количество вхождений Count равно максимальному MaxCount, 
% элемент Element добавляется в список MostFrequent.
get_most_frequent([(Element, Count)|Rest], MaxCount, [Element|MostFrequent]) :-
  Count = MaxCount,
  get_most_frequent(Rest, MaxCount, MostFrequent).

% Если количество вхождений не равно максимальному, элемент пропускается
get_most_frequent([( _, _)|Rest], MaxCount, MostFrequent) :-
  get_most_frequent(Rest, MaxCount, MostFrequent).



% Предикат для взаимодействия с пользователем
% Запрашивает ввод списка, вызывает most_frequent и выводит результат
start :-
write('Enter a numeric list (e.g. [0,3,5,7,1,5,3,0,3,3,5,7,0,5,0]): '),
  read(List),  % пример ввода: [0,3,5,7,1,5,3,0,3,3,5,7,0,5,0]
  most_frequent(List, MostFrequent),
  write('Most frequent elements: '), writeln(MostFrequent).
