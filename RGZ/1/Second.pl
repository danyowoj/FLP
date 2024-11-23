:- use_module(library(readutil)).

start :-
    ВходнойФайл = 'input.txt',
    ВыходнойФайл = 'output.txt',
    read_file(ВходнойФайл, Слова),
    find_max_length(Слова, МаксДлина),
    find_words_by_length(Слова, МаксДлина, СловаМаксДлины),
    write_to_file(ВыходнойФайл, СловаМаксДлины).

read_file(File, Words) :-
    read_file_to_string(File, Content, []),
    split_string(Content, " \n\t", "", Words).

find_max_length(Words, MaxLength) :-
    maplist(string_length, Words, Lengths),
    max_list(Lengths, MaxLength).

find_words_by_length(Words, Length, FilteredWords) :-
    include(check_length(Length), Words, FilteredWords).

check_length(Length, Word) :-
    string_length(Word, Length).

write_to_file(File, Words) :-
    open(File, write, Stream),
    forall(member(Word, Words), (write(Stream, Word), write(Stream, " "))),
    close(Stream).
