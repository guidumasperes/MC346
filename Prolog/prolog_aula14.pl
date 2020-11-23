/*EXERCICIOS DE ARVORES*/

/*Queries: finditem(3,arv(2,arv(1,nil,nil),arv(3,nil,nil)))*/
/*finditem(3,arv(2,arv(1,arv(0,nil,nil),arv(4,nil,nil)),arv(3,nil,arv(5,nil,nil))))*/

/*Acha um item numa abb*/
finditem(IT,arv(X,_,_)) :- X =:= IT.
finditem(IT,arv(X,_,AD)) :- X < IT, finditem(IT,AD).
finditem(IT,arv(X,AE,_)) :- X > IT, finditem(IT,AE).

/*Verifica se uma arvore e abb*/
verabb(arv(_,nil,nil)).
verabb(arv(X,nil,AD)) :- verabb(AD), menor(M,AD), X < M.
verabb(arv(X,AE,nil)) :- verabb(AE), maior(M,AE), X > M.
verabb(arv(X,AE,AD)) :- verabb(AE), verabb(AD), menor(MAD,AD), X < MAD, maior(MAE,AE), X > MAE.

menor(X,arv(X,nil,_)).
menor(IT,arv(_,AE,_)) :- menor(IT,AE).

maior(X,arv(X,_,nil)).
maior(IT,arv(_,_,AD)) :- maior(IT,AD).

verabbV2(IT,arv(IT,_,_)).
verabbV2(IT,arv(X,AE,AD)) :- IT < X -> verabbV2(IT,AE); verabbV2(IT,AD).

/*Insere um item numa abb*/
insertitem(nil,IT,arv(IT,nil,nil)).
insertitem(arv(X,AE,AD),IT,A) :- IT =:= X,A = arv(X,AE,AD).
insertitem(arv(X,AE,AD),IT,A) :- IT > X, insertitem(AD,IT,AA), A=arv(X,AE,AA).
insertitem(arv(X,AE,AD),IT,A) :- IT < X, insertitem(AE,IT,AA), A=arv(X,AA,AD).

/*Remove um item numa abb*/
remove(vazio,N,vazio)
remove(arv(N,AE,vazio), N, AE).
remove(arv(N,vazio,AD), N, AD).
remove(arv(N,AE,AD),N,A) :- menor(AD,Men,NAD), A = arv(Men,AE,NAD).
remove(arv(N,AE,AD), X, A) :- (X < N -> remove(AE,X,NAE), A=arv(N,NAE,AD); remove(AD,X,NAD), A=arv(N,AE,NAD)).          

menoraux(arv(N,vazio,AD), N, AD).
menoraux(arv(N,AE,AD), Men, A) :- menoraux(AE,Men, NAE),A=arv(N, NAE, AD).

/*Calcula profundidade maxima de uma abb*/
maxprof(nil,0).
maxprof(arv(_,AE,AD),N) :- maxl(AE,NE),maxr(AD,ND),(NE > ND -> N is NE+1; N is ND+1).

maxl(arv(_,nil,_),1).
maxl(arv(_,AE,_),N) :- maxl(AE,NN), N is NN+1.

maxr(arv(_,_,nil),1).
maxr(arv(_,_,AD),N) :- maxr(AD,NN), N is NN+1.

/*Coverte uma abb numa lista em ordem infixa (arvore-esquerda, no, arvore-direita)*/
abbinorder(nil,[]).
abbinorder(arv(X,AE,AD),L) :- abbinorder(AE,LE), append(LE,[X],LAUX), abbinorder(AD,LD), append(LAUX,LD,L).

/*Converte uma abb numa lista em ordem prefixa (no, ae, ad)*/
abbpreorder(nil,[]).
abbpreorder(arv(X,AE,AD),L) :- abbpreorder(AE,LE), append([X],LE,LAUX), abbpreorder(AD,LD), append(LAUX,LD,L).

 /*Converte uma lista em uma abb*/
buildabb(L,A) :- buildabbax(L,A,nil).
buildabbax([],A,Aux) :- A=Aux.
buildabbax([X|R],A,Aux) :- insertitem(Aux,X,AA), buildabbax(R,A,AA).

/*EXERCICIOS DE DICIONARIOS*/

/*Queries: [dict(1,a),dict(2,b),dict(3,c)]*/

/*Dado um dicionário acesse o valor associado a uma chave, falha se a chave não esta no dicionário*/
acesse(CH,[dic(CH,V)|_],V).
acesse(CH,[_|DIC],V) :- acesse(CH,DIC,V).

/*Insere um par chave valor no dicionário (ou troca o valor associado a chave se ela ja esta no dicionário)*/
insertdict(dict(CH,V),[],[dict(CH,V)]).
insertdict(dict(CHA,VA),[dict(CH,V)|R],Res) :- CH =:= CHA, append([dict(CHA,VA)],R,Res).
insertdict(dict(CHA,VA),[dict(CH,V)|R],Res) :- insertdict(dict(CHA,VA),R,RRes), append([dict(CH,V)],RRes,Res).

/*Remove uma chave (e seu valor) do dicionário*/
remdict(_,[],[]).
remdict(CHA,[dict(CH,V)|R],Res) :- CHA =:= CH, Res=R.
remdict(CHA,[dict(CH,V)|R],Res) :- remdict(CHA,R,RRes), append([dict(CH,V)],RRes,Res).

/*Implemente o soma1 que dado um contador soma um ao valor associado a uma chave, ou se ela nao estiver no dicionario, acrescenta a chave com valor 1*/
soma1(CH,[],[dict(CH,1)]).
soma1(CHA,[dict(CH,V)|R],Res) :- CHA =:= CH, VV is V+1, append([dict(CH,VV)],R,Res).
soma1(CHA,[dict(CH,V)|R],Res) :- soma1(CHA,R,RRes), append([dict(CH,V)],RRes,Res).

/*Teste: Um dicionário é implementado como uma arvore de busca binaria na Chave, Dado um dicionário nesta forma, implemente o predicado soma1(Dic, Chave, NovoDic)  modo(+ + -), que soma 1 no valor associado a chave Chave no dicionário Dic, se a chave ja esta no dicionário, ou inclui a chave com o valor associado 1, se ela nao existe em Dic.*/
soma1AD(CH,nil,arv(IT,1,nil,nil)).
soma1AD(CH,arv(X,V,AE,AD),A) :- X =:= CH, VV is V+1, A = arv(X,VV,AE,AD).
soma1AD(CH,arv(X,V,AE,AD),A) :- X < CH, soma1AD(CH,AD,AA),A = arv(CH,V,AE,AA).
soma1AD(CH,arv(X,V,AE,AD),A) :- X > CH, soma1AD(CH,AE,AA),A = arv(CH,V,AA,AD).
