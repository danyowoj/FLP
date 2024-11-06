% the main predicate that regulates the operation of the other predicates
most_frequent(List, MostFrequent) :-
  count_occurrences(List, Counts),
  find_max_count(Counts, MaxCount),
  get_most_frequent(Counts, MaxCount, MostFrequent).


% count_occurrences(+List, -Counts)
% counts the number of occurrences of each item in the list
% result: list of pairs (Element, Counts)
count_occurrences([], []).
% this predicate recursively processes the input list
count_occurrences([H|T], Counts) :-
  count_occurrences(T, Counts0),
  % for each element, it checks if a pair (Element, Counts) 
  % already exists in the Counts list
  member((H, C), Counts0), !,
  % if so, the Counts is increased by 1
  NewC is C + 1,
  replace((H, C), (H, NewC), Counts0, Counts).
count_occurrences([H|T], [(H, 1)|Counts]) :-
  count_occurrences(T, Counts).


% replace(+OldPair, +NewPair, +List, -NewList)
% replace OldPair to NewPair int the List.
replace(Pair, NewPair, [Pair|Rest], [NewPair|Rest]).
% an auxiliary predicate for replacing an item in the list
% it is necessary to effectively update the number of occurrences
replace(Pair, NewPair, [H|T], [H|NewT]) :-
  replace(Pair, NewPair, T, NewT).


% find_max_count(+Counts, -MaxCount)
% find the maximum number of occurrences
% this predicate recursively finds the maximum number of occurrences among all pairs (Element, Number)
find_max_count([( _, C)|Rest], MaxCount):-
    find_max_count(Rest, MaxCount0),
    (C > MaxCount0 -> MaxCount = C ; MaxCount = MaxCount0).
find_max_count([], 0).


% get_most_frequent(+Counts, +MaxCount, -MostFrequent)
% retrieves the elements with the maximum number of occurrences
get_most_frequent([], _, []).
% this predicate filters the list of pairs (Element, Quantity), 
% selecting only the elements with the maximum number of occurrences (equal to maxCount)
get_most_frequent([(Element, Count)|Rest], MaxCount, [Element|MostFrequent]) :-
  Count = MaxCount,
  get_most_frequent(Rest, MaxCount, MostFrequent).
get_most_frequent([( _, _)|Rest], MaxCount, MostFrequent) :-
  get_most_frequent(Rest, MaxCount, MostFrequent).



% the main predicate for keyboard input and output of the result
start :-
write('Enter a numeric list (e.g. [0,3,5,7,1,5,3,0,3,3,5,7,0,5,0]): '),
  read(List),  % reads the list from the keyboard (e.g. [0,3,5,7,1,5,3,0,3,3,5,7,0,5,0])
  most_frequent(List, MostFrequent),
  write('Most frequent elements: '), writeln(MostFrequent).
