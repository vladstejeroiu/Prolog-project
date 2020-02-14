%Q1
parent(queenmother,elisabeth). 
parent(elisabeth,charles).
parent(elisabeth,andrew). 
parent(elisabeth,anne).
parent(elisabeth,edward). 
parent(diana,william).
parent(diana,harry). 
parent(sarah,beatrice).
parent(anne,peter). 
parent(anne,zara).
parent(george,elisabeth). 
parent(philip,charles).
parent(philip,andrew). 
parent(philip,edward).
parent(charles,william). 
parent(charles,harry).
parent(andrew,beatrice). 
parent(andrew,eugenie).
parent(mark,peter). 
parent(mark,zara).
parent(william,georgejun). 
parent(kate,georgejun).
parent(william,charlotte). 
parent(kate,charlotte).
parent(philip,anne). 
parent(william,louis).
parent(kate,louis). 
parent(harry,archie).
parent(meghan,archie).

the_royal_females([queenmother,elisabeth,anne,diana,sarah,beatrice,
zara,eugenie,charlotte,kate,meghan]).
the_royal_males([charles,andrew,edward,william,harry,peter,george,
philip,mark,georgejun,louis,archie]).

%(1)
the_royal_family([H|T]) :-the_royal_females(H),the_royal_males(T).

%(2)
father(X,Y) :- parent(X,Y),
	           the_royal_males(L),
	           member(X,L).

%(3)
granddad(X,Z) :-
	           father(X,Y),
	           parent(Y,Z).

%(4)
has_child(X) :-	parent(X, _),
				the_royal_family(Z),
				X \=Z.

%(5)				
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,A), ancestor(A,Y).


%(6)
sibling(X,Y) :-
	          parent(Z,X),
	          parent(Z,Y),
	          X \= Y.
%(7)
brother(X,Y) :-
	          sibling(X,Y),
	          the_royal_males(L),
	          member(X,L).

%(8)
%granddad(george, X).
/**
charles 
andrew
anne
edward 
*/

%(9)
% has_child(X).
/**
X = queenmother
X = elisabeth
X = elisabeth
X = elisabeth
X = elisabeth
X = diana
X = diana
X = sarah
X = anne
X = anne
X = george
X = philip
X = philip
X = philip
X = charles
X = charles
X = andrew
X = andrew
X = mark
X = mark
X = william
X = kate
X = william
X = kate
X = philip
X = william
X = kate
X = harry
X = meghan
*/
%(10)
descendant(X, Y) :- ancestor(Y, X).
%descendant(X, diana).
/**
X = william
X = harry
X = georgejun
X = charlotte
X = louis
X = archie
*/
%(11)
%ancestor(X, archie).
/**
X = harry ;
X = meghan ;
X = queenmother ;
X = elisabeth ;
X = diana ;
X = george ;
X = philip ;
X = charles ;
*/
%(12)		  
has_brother_who_is_granddad(X) :-
	          brother(Y,X),
	          granddad(Y,_).

%has_brother_who_is_granddad(X).
/**
X = andrew ;
X = andrew ;
X = andrew ;
X = andrew ;
X = anne ;
X = anne ;
X = anne ;
X = anne ;
X = edward ;
X = edward ;
X = edward ;
X = edward ;
X = andrew ;
X = andrew ;
X = andrew ;
X = andrew ;
X = edward ;
X = edward ;
X = edward ;
X = edward ;
X = anne ;
X = anne ;
X = anne ;
X = anne ;
*/

%Q2

%a)
toEven([], []).
toEven([H|T], [H1|L]):- 1 is mod(H, 2), !,  H1 is H*2, toEven(T, L).
toEven([H|T], [H|L]) :- toEven(T, L).
/**
toEven([1,5,10], L).
L = [2, 10, 10].

toEven([2,4,10], L).
L = [2, 4, 10].
*/


%b)
star(0).
star(N):-
    N > 0,
    foreach(between(1,N,_),write('*')),nl,
    N1 is N-1,
    star(N1).

star1(1, 0).
star1(N, N2):-
    N2 =< N,
    foreach(between(1,N2,_),write('*')),nl,
    N3 is N2 + 1,
    star1(N, N3).

star2(0).
star2(N) :-
    N > 0,
	star(N),
	N2 is N,
	star1(N2, 2),
	N2 is N - 1,
	star(N2).

/**
star(3).
***
**
*

star2(3).
***
**
*
**
***
*/

%Q3

%a)
euclidsqr1(X,Y,ED) :-
	euclidsqr_acc(X,Y,0, ED1),
	ED is ED1.

euclidsqr_acc([],[],ED,ED).
euclidsqr_acc([X1|X], [Y1|Y], A, ED) :-
	A1 is A + (X1-Y1)*(X1-Y1),
	euclidsqr_acc(X, Y, A1, ED).

%b)
euclidsqr2([], [], 0).
euclidsqr2([X|Xs], [Y|Ys], ED):-
    euclidsqr2(Xs, Ys, ED1),
    ED is ED1 + (X - Y)*(X - Y).
		

%Q4

%1.
member_rem(H, [H|L], L).
member_rem(E, [H|L], [H|R]) :- member_rem(E, L, R).

%2.
gen_n(0,_,[]).
gen_n(N,D,[X|Xs]) :-
            N>0,
			N1 is N-1,
            member_rem(X, D, D1),
            gen_n(N1, D1, Xs).

gen4(L) :-  gen_n(4,[1,2,3,4], L).

%3.
distinct_in_entries(X, Y) :-
            maplist((\=), X, Y).

%4.
gen_poss_soln([R1, R2, R3, R4]) :-
            gen4(R1),
            gen4(R2),
            distinct_in_entries(R1, R2),
            gen4(R3),
            distinct_in_entries(R3, R1),
            distinct_in_entries(R3, R2),
			gen4(R4),
			distinct_in_entries(R4, R1),
			distinct_in_entries(R4, R2),
			distinct_in_entries(R4, R3).

%5.
solve([R1, R2, R3, R4]):-
            gen_poss_soln([R1, R2, R3, R4]),
			R1 = [R11, R12, _, _],
			R2 = [R21, R22, _, R24],
			R3 = [R31, R32, R33, R34],
		    R4 = [_, _, R43, R44],
			R11 > R12,
			R21 < R22,
			R31 > R32,
			R24 > R34,
			R34 > R33,
			R44 > R43.

%6.
solve_in_steps([R1, R2, R3, R4]) :-
            gen4(R1),
			R1 = [R11, R12, _, _],
			R11 > R12,
			gen4(R2),
			distinct_in_entries(R1, R2),
			R2 = [R21, R22, _, R24],
			R21 < R22,
			gen4(R3),
			distinct_in_entries(R3, R1),
            distinct_in_entries(R3, R2),
			R3 = [R31, R32, R33, R34],
			R24 > R34,
			R31 > R32,
			R34 > R33,
			gen4(R4),
			distinct_in_entries(R4, R1),
			distinct_in_entries(R4, R2),
			distinct_in_entries(R4, R3),
			R4 = [_, _, R43, R44],
			R44 > R43.
			
			
			
			

            			
			
		
         
        
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
		