% basic cases F(0) = F(1) = 1
fib(0, 1).
fib(1, 1).

% a recursive function that calculates the Nth Fibonacci number.
% It checks whether N is greater than 1. If so, recursively calls itself for N-1 and N-2 
% and then summarizes the results.
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.  % recursive definition: F(N) = F(N-1) + F(N-2)


start :-
    % this is the main loop that uses repeat to repeat the query multiple times
    repeat,
        write('Enter the number of the Fibonacci number (a negative number to exit): '),
        read(N),
        % repeat by itself does not control the exit of the loop
        % this is done using the loop exit condition
        % if the entered number N is less than 0, then ! (cut) cuts off the search 
        % for alternative solutions, and fail causes repeat to end
        (   N < 0 -> !
        % if N >= 0 than the following part is executed
        ;   fib(N, F), % call function fib to calculate the Fibonacci number
            writef('Число Фибоначчи F(%t) = %t\n', [N, F]), % displays the result
            fail % loop simulation using fail and repeat
        ),!, write('Программа завершена.'), nl, true.

 