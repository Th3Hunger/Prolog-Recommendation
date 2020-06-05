% Author:
% Date: 3/15/2019
%   Updated 3/16/2019
% All done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 1ST PREDICATE */

grid_build(N,M):-
    grid_build_helper(N,N,M).

grid_build_helper(N, M, []):-
        M = 0, !.
grid_build_helper(N, M, [H|T]) :-
   length(Z,N),
        H = Z,
    M2 is M - 1,
    grid_build_helper(N, M2, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 2ND PREDICATE */



grid_gen(N, M) :-
    grid_gen_helper(N, N, M).

grid_gen_helper(N, M, []) :-
    M =< 0, !.
grid_gen_helper(N, M, [H|T]) :-
    random_generator(N,H),
    M2 is M - 1,
    grid_gen_helper(N, M2, T).

random_generator(N,L):- length(L,N),maplist(between(1,N),L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 3rd PREDICATE */


num_gen(X,Y,Z):-
        num_gen_helper(X,Y,[],Z).

num_gen_helper(X,Y,Acc,Acc):-
        Y<X.
num_gen_helper(X,Y,Acc,Z):-
        Y>=X,
        Y2 is Y - 1,
        num_gen_helper(X,Y2,[Y|Acc],Z).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 4TH PREDICATE */


check_num_grid(G):-
        flatten(G,Result),
        max_list(Result,X),length(G,R),
        X =<R,
        check_list(Result).


check_list(Grid):-
        max_list(Grid,Z),
        check_list_helper(Z,Grid).

check_list_helper(Z,Grid):-
        Z=<0.
check_list_helper(Z,Grid):-
        member(Z,Grid),
        Z2 is Z - 1,
        check_list_helper(Z2,Grid).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
/* 5th PREDICATE */



acceptable_permutation(L,R):-
        length(L,N),
        permutation(L,R),
        changed(R,L,N).

changed(_,_,N):-
        N = 0, !.
        
changed(R,L,N):-
        N > 0,
        nth1(N,R,Z),
        nth1(N,L,S),
        S \= Z,
        N2 is N - 1,
        changed(R,L,N2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
/* 6th PREDICATE */


dist([],[]):-
fail,!.
dist([HX|TX],[HY|TY]):-
                       HX=\=HY,!.
dist([HX|TX],[HY|TY]):-
                       HX=:=HY,
dist(TX,TY).





acceptable_distribution(GridRow):-
                   size(GridRow,Row,Col),transpose(GridRow,GridCol),
                   acchelper(Row,Col,GridRow,GridCol).

acchelper(Row,Col,GridRow,GridCol):-
 Row =< 0, !.

acchelper(Row,Col,GridRow,GridCol):-
                     nth1(Row,GridRow,ResultRow),nth1(Col,GridCol,ResultCol),
                     dist(ResultRow,ResultCol),
N1 is Row-1,
N2 is Col-1,

acchelper(N1,N2,GridRow,GridCol).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 7th PREDICATE */


row_col_match(GridRow):-
                    size(GridRow,Row,Col),transpose(GridRow,GridCol),
                   row_col_match_helper(Row,Col,GridRow,GridCol).





 row_col_match_helper(Row,Col,GridRow,GridCol):- %base statement will out from loop.
                                       Row =< 0, !.


row_col_match_helper(Row,Col,GridRow,GridCol):- % CHECKER FOR EACH ROW THAT IS Helsinki_list
                      nth1(Row,GridRow,ResultRow),nth1(Col,GridCol,ResultCol),delete(GridCol,ResultCol,Final),
                     member(ResultRow,Final),
  NewRow is Row-1,
  NewCol is Col-1,

  row_col_match_helper(NewRow,NewCol,GridRow,GridCol).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 8th PREDICATE */



trans(G,R):-
            transpose(G,R).

 transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix),
                                 transpose(RestMatrix, Rows).
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 9th PREDICATE*/




distinct_row([_]).
distinct_row([H|T]):-
        distinct_row_helper(H,T),
        distinct_row(T).

distinct_row_helper(_,[]).
distinct_row_helper(X,[H|T]):- 
        X\==H, 
        distinct_row_helper(X,T).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 10th PREDICATE */



distinct_column(M):-
                  transpose(M,R),distinct_row(R).


  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 11th PREDICATE */

allprop(G):-
           distinct_row(G),distinct_column(G),acceptable_distribution(G),check_num_grid(G),row_col_match(G).
helsinki(N,G):-
          grid_gen(N,R),allprop(R),acceptable_permutation(R,G).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* USED PREDICATEs */

length_of(N, Ls) :-
   length(Ls, N).

size(Mss, R, C) :-
   length(Mss, R),
   maplist(length_of(C), Mss).




size_of_matrix([],0).
        
size_of_matrix([H|T],N) :-
        size_of_matrix(T,N2),
        N is N2 + 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%