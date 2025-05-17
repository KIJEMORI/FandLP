% Создание линии с перебором значений 1(Есть связь с вершиной n) и 0(нет связи) 
%get_random_line(+N, +A, +Acc, -Ab)
get_random_line(A, A, Acc, Acc):-!.
% Добавление 0 и установка метки
get_random_line(N, A, Acc, Ab):- 
	N1 is N + 1,
	append(Acc, [0], Acc1),
	get_random_line(N1, A, Acc1, Ab)
.
% Использование метки или же добавление 1
get_random_line(N, A, Acc, Ab):- 
	N1 is N + 1,
	append(Acc, [1], Acc1),
	get_random_line(N1, A, Acc1, Ab)
.
% Получение всех переборов линий
get_random_line(A, Line):-get_random_line(0,A,[], Line).

%Добавление в конец массива элемент(в том числе и другой массив) -- Для матриц

addElement(X, [], [X]):-!. 
addElement(X, [Y], [Y,X]) :- !.
addElement(X, [Y | Rest], [Y | Rest2]):- addElement(X, Rest, Rest2).

addFirstElement(X, [], [X]):-!. 
addFirstElement(X, [Y], [X,Y]) :- !.
addFirstElement(X, [Y | Rest], [X,Y | Rest]):- !.

addNulls(X, A, A, X):-!.
addNulls(X, N, A, Acc):-
	addFirstElement(0,X,X1),
	N1 is N + 1,
	addNulls(X1, N1, A, Acc)
.

% То же что и с линиями но теперь матрица и перебираются различные линии
%get_random_matrix(+N, +A, +Acc, -Ab)
get_random_matrix(A, A, A1, Acc, Acc):-!.
get_random_matrix(N, A, A1, Acc, Ab):-
	N1 is N + 1,
	A2 is A1 - 1,
	get_random_line(A2,Line),
	A3 is A - A2,
	addNulls(Line, 0, A3, Line1),
	addElement(Line1, Acc, Mat),
	get_random_matrix(N1, A, A2, Mat, Ab)
.
% Получение всех матриц связей(Верхнетреугольных)
get_random_matrix(A, Matrix):-get_random_matrix(0,A,A,[],Matrix).

create_unit_array(A,A,Acc,Acc):-!.
create_unit_array(N,A,Acc,Array):-
	N1 is N + 1,
	addElement(1, Acc, Acc1),
	create_unit_array(N1,A,Acc1,Array)
.
create_unit_array(A, Array):-create_unit_array(0,A,[],Array).

check_raskras(A, Ncolor):-
	get_random_matrix(A, Matrix),
	create_unit_array(A, Array),
	raskras(Matrix, Array, 0, 1, N),
	N is Ncolor,
	write(Matrix),nl,
	fail
.

max([], Max, Max):-!.
max([H|T], Max, Acc):-
	(Max >= H, max(T, Max, Acc) ; Max < H, max(T, H, Acc)),!
.


colors([],[], _, _, Color, AccColors,AccColors):-!.

colors([MatrixH|MatrixT], [ColorsH|ColorsT], A, A, Color, AccColors, Acc):-
	((Color = ColorsH,
	N1 is ColorsH + MatrixH);
	(Color \= ColorsH,
	N1 is ColorsH)),!,
	addElement(N1, AccColors, AccColors1),
	colors(MatrixT, ColorsT, A, A, Color, AccColors1, Acc)
.
colors([MatrixH|MatrixT], [ColorsH|ColorsT], N, A, Color, AccColors, Acc):-
	addElement(ColorsH, AccColors, AccColors1),
	N1 is N + 1,
	colors(MatrixT, ColorsT, N1, A, ColorsH, AccColors1, Acc)
.
colors([], [ColorsH|ColorsT], N, A, Color, AccColors, Acc):-
	addElement(ColorsH, AccColors, AccColors1),
	N1 is N + 1,
	colors([], ColorsT, N1, A, ColorsH, AccColors1, Acc)
.

raskras([], Colors, A, N, N):-!.
raskras([H|T], Colors, A, N, Acc):-
	
	A1 is A+1,
	colors(H,Colors,0,A1,0,[],NewColors),
	max(NewColors, N, Max),
	raskras(T,NewColors,A1,Max,Acc)
.
