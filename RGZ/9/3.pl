:- dynamic employee/3. 
:- initialization(main).

load_database :-
    retractall(employee(_, _, _)),
    (   exists_file('employees.db')
    ->  open('employees.db', read, Stream),
        read_data(Stream),
        close(Stream)
    ;   writeln('База данных не найдена. Будет создана новая.')
    ).

read_data(Stream) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  true
    ;   assertz(Term),
        read_data(Stream)
    ).

save_database :-
    open('employees.db', write, Stream),
    forall(
        employee(Name, Position, Salary),
        format(Stream, 'employee(\'~w\', \'~w\', ~w).~n', [Name, Position, Salary])
    ),
    close(Stream).

read_string_input(String) :-
    read_line_to_string(user_input, String).

add_employee :-
    writeln('Введите данные сотрудников. Для завершения введите "end".'),
    repeat,
    writeln('Ф.И.О. сотрудника: '),
    read_string_input(Name),
    (   Name == "end"
    ->  !
    ;   writeln('Должность: '),
        read_string_input(Position),
        writeln('Оклад (введите число): '),
        read_string_input(SalaryStr),
        catch(number_string(Salary, SalaryStr), _, fail),
        (   number(Salary)
        ->  assertz(employee(Name, Position, Salary)),
            writeln('Сотрудник добавлен.')
        ;   writeln('Ошибка: Оклад должен быть числом.')
        ),
        fail
    ).

delete_employee :-
    writeln('Введите Ф.И.О. сотрудников для удаления. Для завершения введите "end".'),
    repeat,
    writeln('Ф.И.О.: '),
    read_string_input(Name),
    (   Name == "end"
    ->  !
    ;   atom_string(NameAtom, Name),
        (   retract(employee(NameAtom, _, _))
        ->  writeln('Сотрудник удален.')
        ;   writeln('Сотрудник не найден.')
        ),
        fail
    ).

view_database :-
    writeln('Содержимое базы данных:'),
    (   forall(employee(Name, Position, Salary),
               format('Ф.И.О.: ~w, Должность: ~w, Оклад: ~w~n', [Name, Position, Salary]))
    ;   writeln('База данных пуста.')
    ).

query_above_average :-
    findall(Salary, employee(_, _, Salary), Salaries),
    length(Salaries, Count),
    (   Count > 0
    ->  sum_list(Salaries, Total),
        Average is Total / Count,
        format('Средний оклад: ~2f~n', [Average]),
        writeln('Сотрудники с окладом выше среднего:'),
        forall(
            (employee(Name, Position, Salary), Salary > Average),
            format('Ф.И.О.: ~w, Должность: ~w, Оклад: ~w~n', [Name, Position, Salary])
        )
    ;   writeln('База данных пуста. Средний оклад невозможно вычислить.')
    ).

main_menu :-
    writeln('\nМеню:'),
    writeln('1. Просмотр базы данных'),
    writeln('2. Добавить запись'),
    writeln('3. Удалить запись'),
    writeln('4. Выполнить запрос'),
    writeln('5. Выход'),
    writeln('Выберите пункт (введите число):'),
    read_string_input(ChoiceStr),
    catch(number_string(Choice, ChoiceStr), _, fail),
    handle_choice(Choice).

handle_choice(1) :-
    view_database,
    main_menu.
handle_choice(2) :-
    add_employee,
    main_menu.
handle_choice(3) :-
    delete_employee,
    main_menu.
handle_choice(4) :-
    query_above_average,
    main_menu.
handle_choice(5) :-
    save_database,
    writeln('Данные сохранены. Выход из программы.').
handle_choice(_) :-
    writeln('Некорректный выбор, попробуйте снова.'),
    main_menu.

main :-
    load_database,
    main_menu.
