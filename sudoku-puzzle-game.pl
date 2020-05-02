
% HELSINKI

			
grid_build(N,M):-
			length(M,N),
			grid_build2(N,M).
grid_build2(_,[]).
grid_build2(N,[H|T]):-
			length(H,N),
			grid_build2(N,T).
			
			
grid_gen(N,M):-
			grid_build(N,M),
			trans(M,MT),
			acceptable_permutation(M,MT),
			grid_gen2(N,M).
grid_gen2(_,[]).
grid_gen2(N,[H|T]):-
			grid_gen3(N,H),
			grid_gen2(N,T).
grid_gen3(N,H):-
			num_gen(1,N,L),
			perm(L,H).
			
perm2([],_).
perm2([H|T],Source):-
			member(H,Source),
			perm2(T,Source).

perm(Source, L):-
			length(Source,N),
			length(L,N), 
			perm2(L,Source).

num_gen(F,L,[]):-
			F>L.
num_gen(F,L,[F|T]):-
			F=<L,
			F1 is F+1,
			num_gen(F1,L,T).
			
check_num_grid(G):-
            length(G,N),
			
			merge_sublists(G,L),
			check_num_grid2(L,L,N).
check_num_grid2([],_,_).
check_num_grid2([H|T],L,N):-
			H=<N,
			num_gen(1,H,X),
			subset(X,L),
			check_num_grid2(T,L,N).

merge_sublists(G,L):-
			merge_sublists2(G,[],L).
merge_sublists2([],R,R).			
merge_sublists2([H|T],L,R):-
			append(L,H,L1),
			merge_sublists2(T,L1,R).

acceptable_permutation(L,R):-
			permutation(L,R),
			accp(L,R).


accp([],[]).
accp([H1|T1],[H2|T2]):-
			H1\==H2,
			accp(T1,T2).

acceptable_distribution(R):-
			trans(R,C),
			compare_sublists(R,C).
compare_sublists([],[]).			
compare_sublists([H|T],[A|B]):-
			H\=A,
			compare_sublists(T,B).
			

trans([[]|_], []).
trans(M, [H|T]) :- 
		   trans1(M, H, RestMatrix),
		   trans(RestMatrix, T).
		   trans1([], [], []).
trans1([[H|T]|Rows], [H|H1], [T|T1]) :- 
           trans1(Rows, H1, T1).
		   


distinct_rows([]).
distinct_rows([H|T]):-
		   distinctR(H,T),
		   distinct_rows(T).

distinctR(_,[]).
distinctR(H,[F|T]):-
			H\=F,
			distinctR(H,T).

distinct_columns(M):-
		trans(M,L),
		distinct_rows(L).	
		
row_col_match(R):-
		trans(R,C),
		rows_columns2(R,C).
rows_columns2([],_).		
rows_columns2([H|T],C):-
			member(H,C),
			rows_columns2(T,C).

					
					
helsinki(N,G):-
			grid_gen(N,G),
			check_num_grid(G),
			row_col_match(G),
			acceptable_distribution(G),
			distinct_rows(G),
			distinct_columns(G).
			