main() :-
    write('Введите количество человек N: '),
    read(N),
    write('Введите шаг M: '),
    read(M),
    findall(X, between(1, N, X), People),
    elimination_order(People, M, Order),
    write('Порядок выбывания: '), writeln(Order).

elimination_order(People, M, Order) :- 
    elimination_order(People, M, 1, [], Order).

elimination_order([], _, _, Order, Order).
elimination_order(People, M, StartIndex, Acc, Order) :-
    length(People, N),
    NextIndex is (StartIndex + M - 1) mod N,
    NextIndex1 is (NextIndex + 1) mod N,
    (NextIndex =:= 0 -> Selected is N; Selected is NextIndex),
    nth1(Selected, People, Person),
    select(Person, People, Remaining),
    append(Acc, [Person], NewAcc),
    elimination_order(Remaining, M, Selected, NewAcc, Order).

:- initialization(main()).