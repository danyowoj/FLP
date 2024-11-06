fib(0, 1).  % the basic case: F(0) = 1
fib(1, 1).  % the basic case: F(1) = 1
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.  % recursive definition: F(N) = F(N-1) + F(N-2)


start :-
    repeat,
        write('Enter the number of the Fibonacci number (a negative number to exit): '),
        read(N),
        (   N < 0 -> !, fail  % Условие выхода из цикла: отрицательное число
        ;   fib(N, F),
            writef('Число Фибоначчи F(%t) = %t\n', [N, F]),
            fail % Имитация цикла с помощью fail и repeat
        ).
