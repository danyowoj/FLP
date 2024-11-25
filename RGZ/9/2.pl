vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).

count_vowels([], 0).
count_vowels([H|T], Count) :- 
    (vowel(H) -> count_vowels(T, Count1), Count is Count1 + 1 ; count_vowels(T, Count)).

process_line(Line, WordsWithCounts) :-
    split_string(Line, " ", "", Words),
    findall(Word-Count, (member(Word, Words), string_chars(Word, Chars), count_vowels(Chars, Count)), WordsWithCounts).

max_vowel_count([], 0).
max_vowel_count([_-Count|Tail], MaxCount) :- 
    max_vowel_count(Tail, TempMax), MaxCount is max(Count, TempMax).

extract_max_vowel_words(WordsWithCounts, MaxCount, MaxWords) :-
    findall(Word, (member(Word-Count, WordsWithCounts), Count =:= MaxCount), MaxWords).

process_file(InputFile, OutputFile) :-
    open(InputFile, read, Stream),
    read_lines(Stream, WordsWithCounts),
    close(Stream),
    max_vowel_count(WordsWithCounts, MaxCount),
    extract_max_vowel_words(WordsWithCounts, MaxCount, MaxWords),
    open(OutputFile, write, OutStream),
    write_words(OutStream, MaxWords),
    close(OutStream).

read_lines(Stream, WordsWithCounts) :- 
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        process_line(Line, WordsCounts),
        read_lines(Stream, Rest),
        append(WordsCounts, Rest, WordsWithCounts)
    ; 
        WordsWithCounts = []
    ).

write_words(_, []).
write_words(Stream, [Word|T]) :- 
    format(Stream, '~w~n', [Word]),
    write_words(Stream, T).

:- initialization(main).

main :-
    process_file('input.txt', 'output.txt'),
    halt.
