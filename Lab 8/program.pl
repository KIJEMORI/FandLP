man :- man(X), print(X),nl,fail.
woman :- woman(X), print(X),nl,fail.
children(X) :- parent(X,Y), print(Y),nl,fail.
mother :- parent(X,Y),woman(X),print(X),nl,fail.
mother(X,Y) :- parent(X,Y),woman(X).
mother(X) :- parent(Y,X),woman(Y),print(Y).
brothers :- parent(P,X),man(X),parent(P,Y),man(Y),X\=Y,print(X),print(Y),nl,fail.
brother(X,Y) :- parent(W,X),man(X),parent(W,Y),man(Y),X\=Y.
brothers(X) :-parent(P,X),man(X),parent(P,Y),man(Y),Y\=X,print(Y),nl,fail.
b_s(X,Y) :- (parent(P,X),parent(P,Y),X\=Y),(man(X);woman(Y),woman(X);man(Y)),print(X),print(Y),nl,fail.
b_s(X):- parent(P,X),parent(P,Y),X\=Y,print(Y),nl,fail.

% Задание 2
% Вариант №1

father(X, Y) :- parent(X,Y),man(X).
father(X) :- parent(Y,X),man(Y),print(Y),nl,fail.

sister(X,Y) :- parent(P,X),woman(X),parent(P,Y),woman(Y),X\=Y.
sister(X) :- sister(X,Y),print(X),write(' '),print(Y),nl,fail.

% Задание 3
% Вариант №1

grand_pa(X,Y) :- parent(X,P),parent(P,Y),man(X).
grand_pas(X) :- grand_pa(Y,X),print(Y),nl,fail.

grand_pa_and_son(X,Y) :- grand_pa(X,Y);grand_pa(Y,X).

uncle(X,Y) :- parent(P,Y),parent(W,P),parent(W,X),man(X),X\=P.
uncles(X) :- uncle(Y,X),print(Y),nl,fail.

% Задание 4

