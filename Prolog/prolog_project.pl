/*Esse é o predicado deve estar no programa!!! ele será executado assim: ele lê 'Input', processa, e escreve o resultado 'Output'*/
topo :- read(Input), process(Input,Output,T), write(Output),!.
	
/*Função que processa o request*/
process(Input,Output,T) :- find_intersecs(Input,[],Output), tam(Output,T).

/*Funcao para calcular o tamanho da lista de tuplas*/
tam([],0).
tam([_|R],T) :- tam(R,TT), T is TT+1.

/*Funcao para achar intersecções e colocar no 'Acc'*/
find_intersecs([],Acc,Acc).
find_intersecs([circ(NC,XC,YC,RC)|R],Acc,Output) :- intersec_circ(circ(NC,XC,YC,RC),R,[],Res), append(Res,Acc,AAAcc), find_intersecs(R,AAAcc,Output).
find_intersecs([quad(NQ,XQ,YQ,L)|R],Acc,Output) :- intersec_quad(quad(NQ,XQ,YQ,L),R,[],Res), append(Res,Acc,AAAcc), find_intersecs(R,AAAcc,Output).

/*Essa funcao faz a interseccao entre quad e circ, quad e quad e coloca os pares que se intersectam numa lista*/
intersec_quad(_,[],Acc,Acc).
intersec_quad(quad(NQ1,XQ1,YQ1,L1),[quad(NQ2,XQ2,YQ2,L2)|R],Acc,Res) :- (intersec_quad_quad(quad(NQ1,XQ1,YQ1,L1),quad(NQ2,XQ2,YQ2,L2)) -> append([(NQ1,NQ2)],Acc,AAcc), intersec_quad(quad(NQ1,XQ1,YQ1,L1),R,AAcc,Res); intersec_quad(quad(NQ1,XQ1,YQ1,L1),R,Acc,Res)).
intersec_quad(quad(NQ,XQ,YQ,L),[circ(NC,XC,YC,RC)|R],Acc,Res) :- (intersec_circ_quad(circ(NC,XC,YC,RC),quad(NQ,XQ,YQ,L)) -> append([(NC,NQ)],Acc,AAcc), intersec_quad(quad(NQ,XQ,YQ,L),R,AAcc,Res); intersec_quad(quad(NQ,XQ,YQ,L),R,Acc,Res)).

/*Essa funcao faz a interseccao entre circ e circ, circ e quad e coloca os pares que se intersectam numa lista*/
intersec_circ(_,[],Acc,Acc).
intersec_circ(circ(NC1,XC1,YC1,RC1),[circ(NC2,XC2,YC2,RC2)|R],Acc,Res) :- (intersec_circ_circ(circ(NC1,XC1,YC1,RC1),circ(NC2,XC2,YC2,RC2)) -> append([(NC1,NC2)],Acc,AAcc), intersec_circ(circ(NC1,XC1,YC1,RC1),R,AAcc,Res); intersec_circ(circ(NC1,XC1,YC1,RC1),R,Acc,Res)).
intersec_circ(circ(NC,XC,YC,RC),[quad(NQ,XQ,YQ,L)|R],Acc,Res) :- (intersec_circ_quad(circ(NC,XC,YC,RC),quad(NQ,XQ,YQ,L)) -> append([(NC,NQ)],Acc,AAcc), intersec_circ(circ(NC,XC,YC,RC),R,AAcc,Res); intersec_circ(circ(NC,XC,YC,RC),R,Acc,Res)).

/*Checa intersecção com quadrado e círculo*/
intersec_circ_quad(circ(NC,XC,YC,RC),quad(NQ,XQ,YQ,L)) :- LL is L/2, XXQ is XQ-LL, XC < XXQ, TX is XQ, findY(circ(NC,XC,YC,RC),quad(NQ,XQ,YQ,L),TY),!,check_intersec(TX,TY,circ(_,XC,YC,RC)).
intersec_circ_quad(circ(NC,XC,YC,RC),quad(NQ,XQ,YQ,L)) :- LL is L/2, D is XQ+LL, XC > D, TX is D, findY(circ(NC,XC,YC,RC),quad(NQ,XQ,YQ,L),TY),!,check_intersec(TX,TY,circ(_,XC,YC,RC)).

/*Acha onde está o círculo em relação ao quadrado, com relação ao eixo y*/
findY(circ(_,_,YC,_),quad(_,_,YQ,L),TY) :- LL is L/2, YYQ is YQ+LL, YC < YYQ, TY is YQ.
findY(circ(_,_,YC,_),quad(_,_,YQ,L),TY) :- LL is L/2, D is YQ-LL, YC > D, TY is D.

/*Checa se tem intersecção entre círculo e quadrado*/
check_intersec(TX,TY,circ(_,XC,YC,RC)) :- DX is XC-TX, DY is YC-TY, DXS is DX*DX, DYS is DY*DY,S is DXS+DYS,DT is sqrt(S), DT =< RC.

/*Checa intersecção entre círculo e círculo*/
intersec_circ_circ(circ(_,XC1,YC1,RC1),circ(_,XC2,YC2,RC2)) :- XR is XC1-XC2, YR is YC1-YC2, XRS is XR*XR, YRS is YR*YR, R is XRS+YRS, RR = sqrt(R), RCR is RC1+RC2, RR =< RCR.

/*Checa intersecação entre quadrado e qudrado*/
intersec_quad_quad(quad(_,X1,Y1,L1),quad(_,X2,Y2,L2)) :- H1 is L1/2, XX1 is X1-H1, YY1 is Y1+H1, H2 is L2/2, XX2 is X2-H2, YY2 is Y2+H2, XXX1 is XX1+L1, XXX1 >= XX2, XXX2 is XX2+L2, XX1 =< XXX2, YYY2 is YY2-L2, YY1 >= YYY2, YYY1 is YY1-L1, YYY1 =< YY2.

/*Referências:
 * http://jeffreythompson.org/collision-detection/table_of_contents.php
 * http://jeffreythompson.org/collision-detection/circle-rect.php
 * http://jeffreythompson.org/collision-detection/rect-rect.php
 * http://jeffreythompson.org/collision-detection/circle-circle.php 
 * https://www.geeksforgeeks.org/check-two-given-circles-touch-intersect/
 * https://www.geeksforgeeks.org/find-two-rectangles-overlap/
 * */
 
 

/* teste1 = [circ('a',5,5,3),quad('b',10,10,2),circ('c',4,4,3)]
teste 2 = [quad(a,3,12,2),quad(b,3,3,2),quad(c,5.5,5.5,5),circ(d,9,9,2),circ(e,13,1,1),quad(f,17.5,5.5,3),quad(g,19,7,4),circ(h,17,9,3),circ[i,11,2,2]]
 * */
 
 /*domains
    l = integer*
    
predicates
    printlist(l)
    
clauses
    printlist([]).
    
    printlist([X|List]) :-
        write(X),nl,
        printlist(List).*/
 
