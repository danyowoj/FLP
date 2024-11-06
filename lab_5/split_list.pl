split_list(List, Min, Max, LessThanMin, BetweenMinMax, GreaterThanMax) :-
  % basic case: empty list
  List = [],
  LessThanMin = [],
  BetweenMinMax = [],
  GreaterThanMax = [].

split_list(List, Min, Max, LessThanMin, BetweenMinMax, GreaterThanMax) :-
  % recursive case: processing the first list element
  List = [Head | Tail],
  % splitting the list based on the values of Head, Min and Max
  
  % The element is added to the list LessThanMin
  % and the recursion continues with the rest of the list
  (   Head < Min
      -> LessThanMin = [Head | LessThanMinRest],
         split_list(Tail, Min, Max, LessThanMinRest, BetweenMinMax, GreaterThanMax)
  % the element is added to the list BetweenMinMax 
  % and the recursion continues with the rest of the list
  ;   Head >= Min, Head =< Max
      -> BetweenMinMax = [Head | BetweenMinMaxRest],
         split_list(Tail, Min, Max, LessThanMin, BetweenMinMaxRest, GreaterThanMax)
  % the element is added to the list GreaterThanMax
  % and the recursion continues with the rest of the list
  ;   Head > Max
      -> GreaterThanMax = [Head | GreaterThanMaxRest],
         split_list(Tail, Min, Max, LessThanMin, BetweenMinMax, GreaterThanMaxRest)
  ).


% the predicate provides user interaction by getting a list and two numbers from the keyboard
% then calls split_list to process the data and output the results
start :-
  write('Enter a numeric list (e.g. [3,7,1,-3,5,8,0,9,2]): '),
  read(List),
  write('Enter the minimum number: '),
  read(Min),
  write('Enter the maximum number: '),
  read(Max),
  split_list(List, Min, Max, LessThanMin, BetweenMinMax, GreaterThanMax),
  write('Less than min numbers: '), writeln(LessThanMin),
  write('Between min and max numbers: '), writeln(BetweenMinMax),
  write('Greater than max numbers: '), writeln(GreaterThanMax).
