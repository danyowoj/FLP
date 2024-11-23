:- dynamic игрушка/3. % Факты в базе данных: игрушка(Название, Стоимость, Возраст).

start :-
    load_database('toys.db'),
    main_menu.

load_database(File) :-
    exists_file(File) -> consult(File) ; true.

save_database(File) :-
    tell(File),
    listing(игрушка/3), 
    told.

main_menu :-
    writeln('\nМеню:'),
    writeln('1. Просмотр содержимого базы данных'),
    writeln('2. Добавить записи'),
    writeln('3. Удалить записи'),
    writeln('4. Запрос: найти дешевые игрушки для ребенка 3 лет'),
    writeln('5. Выход с сохранением'),
    write('Выберите пункт: '),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :- view_database, main_menu.
handle_choice(2) :- add_toys, main_menu.
handle_choice(3) :- delete_toys, main_menu.
handle_choice(4) :- query_cheapest_for_age(3), main_menu.
handle_choice(5) :- save_database('toys.db'), writeln('База данных сохранена. Выход.'), !.
handle_choice(_) :- writeln('Неверный выбор. Попробуйте снова.'), main_menu.

view_database :-
    writeln('\nСодержимое базы данных:'),
    (   игрушка(_, _, _) ->
        forall(игрушка(Название, Стоимость, Возраст),
               format('Название: ~w, Стоимость: ~w, Возрастные границы: ~w~n', [Название, Стоимость, Возраст]))
    ;   writeln('База данных пуста.')
    ).


add_toys :- 
    writeln('\nДобавление записей. Введите "stop." чтобы закончить.'),
    add_toys_loop.

add_toys_loop :-
    write('Название игрушки: '), read(Название),
    (   Название == stop
    ->  true
    ;   write('Стоимость: '), read(Стоимость),
        write('Возрастные границы (например, 1-5): '), read(Возраст),
        assertz(игрушка(Название, Стоимость, Возраст)),
        writeln('Запись добавлена.'),
        add_toys_loop
    ).

delete_toys :- 
    writeln('\nУдаление записей. Введите "stop." чтобы закончить.'),
    delete_toys_loop.

delete_toys_loop :-
    write('Название игрушки для удаления: '), read(Название),
    (   Название == stop
    ->  true
    ;   retractall(игрушка(Название, _, _)),
        writeln('Запись удалена.'),
        delete_toys_loop
    ).

query_cheapest_for_age(Age) :-
    writeln('\nДешевые игрушки, подходящие для возраста 3 лет:'),
    findall(Стоимость, (игрушка(_, Стоимость, Мин-Макс), Мин =< Age, Макс >= Age), Стоимости),
    (   Стоимости == [] ->
        writeln('Нет подходящих игрушек.')
    ;   min_list(Стоимости, МинСтоимость),
        findall(Название, (игрушка(Название, МинСтоимость, Мин-Макс), Мин =< Age, Макс >= Age), Названия),
        format('Самые дешевые игрушки: ~w~n', [Названия])
    ).

