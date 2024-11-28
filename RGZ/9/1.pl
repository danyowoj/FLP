% Основной предикат программы
main() :-
    write('Введите количество человек N: '), % Запрашиваем количество человек N
    read(N),                                 % Читаем введенное значение N
    write('Введите шаг M: '),               % Запрашиваем шаг M
    read(M),                                 % Читаем введенное значение M
    findall(X, between(1, N, X), People),   % Формируем список людей от 1 до N
    elimination_order(People, M, Order),    % Вычисляем порядок выбывания
    write('Порядок выбывания: '), writeln(Order). % Выводим результат

% Предикат вычисления порядка выбывания
% Начальная версия, вызывает рекурсивный вариант с дополнительными параметрами
elimination_order(People, M, Order) :- 
    elimination_order(People, M, 1, [], Order).

% Базовый случай: если список людей пустой, возвращаем накопленный порядок
elimination_order([], _, _, Order, Order).

% Рекурсивный случай
elimination_order(People, M, StartIndex, Acc, Order) :-
    length(People, N),                               % Определяем количество оставшихся людей
    NextIndex is (StartIndex + M - 1) mod N,        % Вычисляем индекс следующего выбывшего (по модулю количества людей)
    NextIndex1 is (NextIndex + 1) mod N,            % Сдвигаем индекс, чтобы соответствовать 1-индексации
    (NextIndex =:= 0 -> Selected is N; Selected is NextIndex), % Корректируем индекс для случая NextIndex = 0
    nth1(Selected, People, Person),                 % Находим человека с этим индексом
    select(Person, People, Remaining),              % Удаляем этого человека из списка
    append(Acc, [Person], NewAcc),                  % Добавляем его к порядку выбывания
    elimination_order(Remaining, M, Selected, NewAcc, Order). % Рекурсивно вызываем для оставшихся людей

% Инициализация программы при запуске
:- initialization(main()).
