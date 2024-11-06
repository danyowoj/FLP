% define a predicate for printing odd numbers in descending order
print_odd_numbers(Lower, Upper) :-
    Upper >= Lower, % check that the upper bound >= the lower one
    (   Upper mod 2 =:= 0 -> Upper1 is Upper - 1 ; Upper1 is Upper % if Upper is even, decrease by 1
    ),
    print_odd(Upper1, Lower). % calling the auxiliary predicate for printing

% auxiliary predicate for printing odd numbers
print_odd(Current, Lower) :-
    Current >= Lower,       % check that the current number >= the lower bound
    writeln(Current),       % printing the current number
    Next is Current - 2,    %  moving on to the next odd number
    print_odd(Next, Lower). % recursively calling the predicate

% the initial predicate for running the program
start :-
    write('Enter the lower bound: '), flush_output(current_output), % ask the user to enter the lower limit
    read(Lower), % reading the lower bound
    write('Enter the upper bound: '), flush_output(current_output), % ask the user to enter the upper limit
    read(Upper), % reading the upper bound
    print_odd_numbers(Lower, Upper). % output of odd numbers
