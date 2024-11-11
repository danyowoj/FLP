% Установка кодировки UTF-8
:- set_prolog_flag(encoding, utf8).

% Определяем отношения "родитель"
родитель(джон, боб).
родитель(мери, боб).
родитель(боб, лиз).
родитель(боб, паул).
родитель(боб, сэм).
родитель(паул, пат).
родитель(мери, энн).

% Определяем пол
мужчина(джон).
мужчина(боб).
мужчина(паул).
мужчина(сэм).
мужчина(пат).

женщина(мери).
женщина(энн).
женщина(лиз).

% Определяем отношения "отец" и "мать"
отец(X, Y) :- родитель(X, Y), мужчина(X).
мать(X, Y) :- родитель(X, Y), женщина(X).

% Определяем отношения "брат" и "сестра"
брат(X, Y) :- родитель(Z, X), родитель(Z, Y), мужчина(X), X \= Y.
сестра(X, Y) :- родитель(Z, X), родитель(Z, Y), женщина(X), X \= Y.

% Определяем отношения "внук" и "внучка"
внук(X, Y) :- родитель(Z, X), родитель(Y, Z), мужчина(X).
внучка(X, Y) :- родитель(Z, X), родитель(Y, Z), женщина(X).

% Определяем отношение "тётя"
тётя(X, Y) :- сестра(X, Z), родитель(Z, Y).

% Определяем "иметь двух детей"
имеет_двух_детей(X) :- родитель(X, Y1), родитель(X, Y2), Y1 \= Y2,
    \+ (родитель(X, Y3), Y3 \= Y1, Y3 \= Y2).

% Определяем "продолжатель рода"
продолжатель_рода(X) :- мужчина(X), родитель(X, Y), мужчина(Y).

% Определяем "имеет мать"
имеет_мать(X) :- мать(_, X).

% Функция для вывода ответов на вопросы
start :-
    % а) Кто отец Сэма?
    (отец(КтоОтецСэма, сэм) -> write('а) Кто отец Сэма? '), write(КтоОтецСэма), nl; write('а) Кто отец Сэма? Нет данных'), nl),

    % б) Есть ли мать у Боба?
    (имеет_мать(боб) -> write('б) Есть ли мать у Боба? true'), nl; write('б) Есть ли мать у Боба? false'), nl),

    % в) Кто сестра Сэма?
    (сестра(КтоСестраСэма, сэм) -> write('в) Кто сестра Сэма? '), write(КтоСестраСэма), nl; write('в) Кто сестра Сэма? Нет данных'), nl),

    % г) Есть ли сестра у Лиз?
    (сестра(_, лиз) -> write('г) Есть ли сестра у Лиз? true'), nl; write('г) Есть ли сестра у Лиз? false'), nl),

    % д) Кто брат Боба?
    (брат(КтоБратБоба, боб) -> write('д) Кто брат Боба? '), write(КтоБратБоба), nl; write('д) Кто брат Боба? Нет данных'), nl),

    % е) Кто внуки Мэри?
    findall(ВнукМэри, (внук(ВнукМэри, мери); внучка(ВнукМэри, мери)), ВнукиМэри),
    write('е) Кто внуки Мэри? '), write(ВнукиМэри), nl,

    % ж) Чей внук Паул?
    findall(ДедушкаБабушкаПаула, (внук(паул, ДедушкаБабушкаПаула); внучка(паул, ДедушкаБабушкаПаула)), ДедушкиБабушкиПаула), write('ж) Чей внук Паул? '), write(ДедушкиБабушкиПаула), nl,

    % з) Кто тетя Сэма?
    (тётя(КтоТётяСэма, сэм) -> write('з) Кто тетя Сэма? '), write(КтоТётяСэма), nl; write('з) Кто тетя Сэма? Нет данных'), nl),

    % и) Есть ли племянники у Энн?
    (брат(КтоБратЭнн, энн), родитель(КтоБратЭнн, ПлемянникЭнн) -> write('и) Есть ли племянники у Энн? true'), nl; write('и) Есть ли племянники у Энн? false'), nl),

    % к) У кого ровно двое детей? (должна найти только Мэри один раз)
    (имеет_двух_детей(УКогоДвоеДетей) -> write('к) У кого ровно двое детей? '), write(УКогоДвоеДетей), nl; write('к) У кого ровно двое детей? Нет данных'), nl),

    % л) Боб - продолжатель рода?
    (продолжатель_рода(боб) -> write('л) Боб - продолжатель рода? true'), nl; write('л) Боб - продолжатель рода? false'), nl).
