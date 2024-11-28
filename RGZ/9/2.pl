:- use_module(library(readutil)).

% Основной предикат
process_file(InputFile, OutputFile) :-
    read_file_to_string(InputFile, Content, []),         % Читаем содержимое файла
    split_string(Content, " \n\t", " \n\t", Words),      % Разбиваем текст на слова
    findall(Count-Word, (member(Word, Words), count_vowels(Word, Count)), Counts), % Считаем гласные в каждом слове
    max_vowel_words(Counts, MaxWords),                  % Находим слова с максимальным количеством гласных
    write_words_to_file(MaxWords, OutputFile).          % Записываем их в новый файл

% Предикат подсчёта количества гласных в слове
count_vowels(Word, Count) :-
    string_chars(Word, Chars),
    include(is_vowel, Chars, Vowels),
    length(Vowels, Count).

% Предикат проверки, является ли символ гласным
is_vowel(Char) :-
    member(Char, ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']).

% Находим слова с максимальным количеством гласных
max_vowel_words(Counts, MaxWords) :-
    max_member(MaxCount-_, Counts),                     % Определяем максимальное количество гласных
    findall(Word, member(MaxCount-Word, Counts), MaxWords).

% Записываем список слов в файл
write_words_to_file(Words, OutputFile) :-
    open(OutputFile, write, Stream),
    forall(member(Word, Words), writeln(Stream, Word)),
    close(Stream).
