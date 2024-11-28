% Определение динамического предиката employee/3
% Хранит информацию о сотрудниках в формате: employee(Имя, Должность, Оклад)
:- dynamic employee/3.

% Инициализация программы
:- initialization(main).

% Загрузка базы данных из файла
load_database :-
    retractall(employee(_, _, _)), % Удаляем все предыдущие записи из базы данных
    (   exists_file('employees.db') % Проверяем, существует ли файл базы данных
    ->  open('employees.db', read, Stream), % Если файл существует, открываем его для чтения
        read_data(Stream), % Считываем данные из файла
        close(Stream) % Закрываем поток
    ;   writeln('База данных не найдена. Будет создана новая.') % Если файла нет, выводим сообщение
    ).

% Рекурсивное чтение данных из файла
read_data(Stream) :-
    read(Stream, Term), % Считываем следующий термин из файла
    (   Term == end_of_file % Если достигнут конец файла, завершаем
    ->  true
    ;   assertz(Term), % Добавляем считанную запись в базу данных
        read_data(Stream) % Рекурсивно продолжаем чтение
    ).

% Сохранение базы данных в файл
save_database :-
    open('employees.db', write, Stream), % Открываем файл для записи
    forall(
        employee(Name, Position, Salary), % Для каждой записи в базе данных
        format(Stream, 'employee(\'~w\', \'~w\', ~w).~n', [Name, Position, Salary]) % Форматируем и записываем в файл
    ),
    close(Stream). % Закрываем поток

% Чтение строки ввода от пользователя
read_string_input(String) :-
    read_line_to_string(user_input, String).

% Добавление сотрудника в базу данных
add_employee :-
    writeln('Введите данные сотрудников. Для завершения введите "end".'),
    repeat, % Повторяем цикл до выполнения условия завершения
    writeln('Ф.И.О. сотрудника: '),
    read_string_input(Name),
    (   Name == "end" % Если введено "end", выходим из цикла
    ->  !
    ;   writeln('Должность: '),
        read_string_input(Position),
        writeln('Оклад (введите число): '),
        read_string_input(SalaryStr),
        catch(number_string(Salary, SalaryStr), _, fail), % Преобразуем строку в число
        (   number(Salary)
        ->  assertz(employee(Name, Position, Salary)), % Добавляем запись в базу данных
            writeln('Сотрудник добавлен.')
        ;   writeln('Ошибка: Оклад должен быть числом.') % Выводим ошибку, если оклад некорректен
        ),
        fail % Возвращаемся в начало цикла
    ).

% Удаление сотрудника из базы данных
delete_employee :-
    writeln('Введите Ф.И.О. сотрудников для удаления. Для завершения введите "end".'),
    repeat,
    writeln('Ф.И.О.: '),
    read_string_input(Name),
    (   Name == "end" % Если введено "end", выходим из цикла
    ->  !
    ;   atom_string(NameAtom, Name), % Преобразуем строку в атом
        (   retract(employee(NameAtom, _, _)) % Удаляем запись из базы данных
        ->  writeln('Сотрудник удален.')
        ;   writeln('Сотрудник не найден.') % Если запись не найдена, выводим сообщение
        ),
        fail
    ).

% Просмотр содержимого базы данных
view_database :-
    writeln('Содержимое базы данных:'),
    (   forall(employee(Name, Position, Salary), % Для каждой записи в базе
               format('Ф.И.О.: ~w, Должность: ~w, Оклад: ~w~n', [Name, Position, Salary]))
    ;   writeln('База данных пуста.') % Если записей нет, выводим сообщение
    ).

% Запрос сотрудников с окладом выше среднего
query_above_average :-
    findall(Salary, employee(_, _, Salary), Salaries), % Получаем список всех окладов
    length(Salaries, Count),
    (   Count > 0 % Если есть хотя бы одна запись
    ->  sum_list(Salaries, Total), % Суммируем оклады
        Average is Total / Count, % Вычисляем средний оклад
        format('Средний оклад: ~2f~n', [Average]),
        writeln('Сотрудники с окладом выше среднего:'),
        forall(
            (employee(Name, Position, Salary), Salary > Average), % Находим сотрудников с окладом выше среднего
            format('Ф.И.О.: ~w, Должность: ~w, Оклад: ~w~n', [Name, Position, Salary])
        )
    ;   writeln('База данных пуста. Средний оклад невозможно вычислить.') % Если записей нет, выводим сообщение
    ).

% Главное меню программы
main_menu :-
    writeln('\nМеню:'),
    writeln('1. Просмотр базы данных'),
    writeln('2. Добавить запись'),
    writeln('3. Удалить запись'),
    writeln('4. Выполнить запрос'),
    writeln('5. Выход'),
    writeln('Выберите пункт (введите число):'),
    read_string_input(ChoiceStr),
    catch(number_string(Choice, ChoiceStr), _, fail), % Преобразуем строку в число
    handle_choice(Choice).

% Обработка выбора пользователя в меню
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
    save_database, % Сохраняем базу данных перед выходом
    writeln('Данные сохранены. Выход из программы.').
handle_choice(_) :-
    writeln('Некорректный выбор, попробуйте снова.'),
    main_menu.

% Главная точка входа в программу
main :-
    load_database, % Загружаем базу данных при старте
    main_menu. % Переходим в главное меню
