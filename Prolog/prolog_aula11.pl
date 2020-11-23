/*Tamanho de uma lista*/
tamV1(L,N) :- L=[],N=0.
tamV1(L,N) :- L=[_|R], tamV1(R,NN), N is NN+1.

tamV2([],0).
tamV2([_|R],N) :- tamV2(R,NN), N is NN+1.

tamV3(L,N) :- tamx(L,N,0).
tamx([],N,Acc) :- N=Acc.
tamx([_|R],N,Acc) :- AAcc is Acc+1,tamx(R,N,AAcc).

/*Soma dos elementos de uma lista*/
somaV1([],0).
somaV1([X|R],N) :- somaV1(R,NN), N is NN+X.

somaV2(L,N) :- somax(L,N,0).
somax([],N,Acc) :- N=Acc.
somax([X|R],N,Acc) :- NAcc is Acc+X, somax(R,N,NAcc).

/*Soma dos elementos pares de uma lista*/
somapV1([],0).
somapV1([X|R],N) :- X mod 2 =:= 0, somapV1(R,NN), N is NN+X.
somapV1([_|R],N) :- somapV1(R,N).

somapV2(L,N) :- somapax(L,N,0).
somapax([],N,Acc) :- N=Acc.
somapax([X|R],N,Acc) :- X mod 2 =:= 0, AAcc is Acc+X, somapax(R,N,AAcc); somapax(R,N,Acc).

/*Soma dos elementos nas posicoes pares*/
somapospar(L,Sum) :- somaposparax(L,1,0,Sum).
somaposparax([],_,Acc,Sum) :- Sum=Acc.
somaposparax([X|R],Pos,Acc,Sum) :- Pos mod 2 =:= 0, AAcc is Acc+X, PPos is Pos+1, somaposparax(R,PPos,AAcc,Sum);PPos is Pos+1, somaposparax(R,PPos,Acc,Sum).

/*Existe item na lista*/
existx([],_) :- false.
existx([X|R],IT) :- X =:= IT, true;existx(R,IT).

existxV2(L,IT) :- existxax(L,IT,false).
existxax([],_,Acc) :- Acc.
existxax([X|R],IT,Acc) :- X =:= IT, existxax(R,IT,true);existxax(R,IT,Acc).

/*Posicao de um item na lista*/
pos(IT,L,P) :- pos(IT,L,P,1).
pos(IT,[X|_],P,Acc) :- X=IT,P=Acc.
pos(IT, [_|R],P,Acc) :- AAcc is Acc+1, pos(IT,R,P,AAcc).

/*conta quantas vezes o item aparece na lista*/
conta(_,[],0). 
conta(IT,[X|R],N) :- X==IT, conta(IT,R,NN), N is NN+1.
conta(IT,[_|R],N) :- conta(IT,R,N).

contaV1(IT,L,N) :- contax(IT,L,N,0).
contax(_,[],N,Acc) :- N=Acc.
contax(IT,[X|R],N,Acc) :- X==IT, AAcc is Acc+1, contax(IT,R,N,AAcc).
contax(IT,[_|R],N,Acc) :- contax(IT,R,N,Acc).

/*maior elemento de uma lista*/
maior([X],X).
maior([X|R],M) :- maior(R,MM), X>MM, M=X.
maior([_|R],M) :- maior(R,M).

maiorV2([X|R],M) :- maiorax(R,M,X).
maiorax([],M,Acc) :- M=Acc.
maiorax([X|R],M,Acc) :- X > Acc, maiorax(R,M,X).
maiorax([_|R],M,Acc) :- maiorax(R,M,Acc).

/*reverte uma lista*/
rev([X],[X]).
rev([X|R],B) :- rev(R,BB),append(BB,[X],B).

revV2([X|R],B) :- revax(R,B,[X]).
revax([],B,Acc) :- B=Acc.
revax([X|R],B,A) :- revax(R,B,[X|A]).

/*intercala 2 listas*/
intercala([],B,B).
intercala(A,[],A).
intercala([A|RA],[B|RB],[A,B|RR]) :- intercala(RA,RB,RR).

intercalaV2(A,B,C) :- intercalax(A,B,C,[]).
intercalax([],B,C,Acc) :- C=[Acc|B].
intercalax(A,[],C,Acc) :- C=[Acc|A].
intercalax([],[],C,Acc) :- C=Acc.
intercalax([A|RA],[B|RB],C,Acc) :- intercalax(RA,RB,C,[A,B|Acc]).

/*a lista ja esta ordenada?*/
ordenada([]).
ordenada([_]).
ordenada([A,B|R]):- A =< B, ordenada([B|R]).

/*dado n gera a lista de 1 a n*/
gera(N,L) :- gera(N,L,1).
gera(N,[N],N).
gera(N,L,X) :- XX is X+1, gera(N,LL,XX), L = [X|LL].

geraV2(N,L) :- gerax(N,L,[1],1).
gerax(N,L,LAcc,_) :- N=:=1, L=LAcc. 
gerax(N,L,LAcc,Acc) :- AAcc is Acc+1, append(LAcc,[AAcc],LLAcc), NN is N-1, gerax(NN,L,LLAcc,AAcc).

/*retorna o ultimo elemento da lista*/
ultimo([X],X).
ultimo([_|R],X) :- ultimo(R,X).

/*retorna a lista sem o ultimo elemento*/
semult(L,LS) :- rev(L,[_|LL]),rev(LL,LS).

/*shift right*/
shiftr(L,LS) :- shiftrax(L,LS,[]).
shiftrax([X],LS,Acc) :- append([X],Acc,LS).
shiftrax([X|R],LS,Acc) :- append(Acc,[X],AAcc), shiftrax(R,LS,AAcc).

/*shiftr n vezes*/
shiftrn(L,N,LS) :- N=:=0, LS=L.
shiftrn(L,N,LS) :- shiftr(L,LL), NN is N-1, shiftrn(LL,NN,LS).

/*shift left*/
shiftl([],[]).
shiftl([X|R],LS) :- append(R,[X],LS).

/*shift left n vezes*/
shiftln(L,N,LS) :- N=:=0, LS=L.
shiftln(L,N,LS) :- shiftl(L,LL), NN is N-1, shiftln(LL,NN,LS).

/*remove item da lista 1 vez*/
remitem(_,[],[]).
remitem(IT,[X|R],LS) :- IT=:=X, LS=R.
remitem(IT,[X|R],LS) :- remitem(IT,R,LL), append([X],LL,LS).

/*remove item da lista todas as vezes*/
remall(_,[],[]).
remall(IT,[X|R],LS) :- IT=:=X, remall(IT,R,LS).
remall(IT,[X|R],LS) :- remall(IT,R,LL), append([X],LL,LS).

/*remove item da lista as primeiras n vezes*/
remn(_,[],_,[]).
remn(_,L,0,L).
remn(IT,L,N,LS) :- remitem(IT,L,LL), NN is N-1, remn(IT,LL,NN,LS).

/*remove item da lista a ultima vez que aparece*/
remlast(IT,L,LS) :- conta(IT,L,N), remlastax(IT,L,LS,N).
remlastax(_,[],[],_).
remlastax(IT,[X|R],LS,N) :- X=:=IT, NN is N-1, (NN=:=0 -> remlastax(IT,R,LS,NN);remlastax(IT,R,LL,NN), append([X],LL,LS)).
remlastax(IT,[X|R],LS,N) :- remlastax(IT,R,LL,N), append([X],LL,LS).

/*troca velho por novo na lista (1 so vez)*/
troca1([],_,[]).
troca1([X|R],It,LN) :- X=It, LN=R.
troca1([X|R],It,LN) :- troca1(R,It,LR),LN = [X|LR].

/*troca velho por novo na lista (todas vezes)*/
trocaall([],_,[]).
trocaall([X|R],N,V,LN) :- X=V, trocaall(R,N,V,RR), LN=[N|RR].
trocaall([X|R],N,V,LN) :- trocaall(R,N,V,RR),LN = [X|RR].

/*troca velho por novo na lista n (as primeiras n vezes)*/
trocan(_,[],_,[]).
trocan(_,L,0,L).
trocan(IT,L,N,LS) :- troca1(L,IT,LL), NN is N-1, trocan(IT,LL,NN,LS).