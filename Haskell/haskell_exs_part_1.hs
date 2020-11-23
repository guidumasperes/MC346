-- 1 - tamanho de uma lista

tam [] = 0
tam (a:as) = 1 + tam as

tam l = tam' l 0
    where tam' [] acc = acc
          tam' (x:xs) acc = tam' xs (acc+1)

-- 2 - soma dos elementos de uma lista

soma [] = 0
soma (a:as) = a + soma as

soma l = soma' l 0
    where soma' [] acc = acc
          soma' (x:xs) acc = soma' xs (acc+x)

-- 3 - soma dos números pares de uma lista ( modulo = %)

somapar [] = 0
somapar (a:as) = if a mod 2 == 0
                 then a + somapar as
                 else somapar as

somapar l = somapar' l 0
      where somapar' [] acc = acc
            somapar' (x:xs) acc = if mod x 2 == 0
                                  then somapar' xs (x+acc)
                                  else somapar' xs acc

-- 4 - retorna o ultimo elemento de uma lista

lastelem [] = []
lastelem (a:as) = if as == []
                  then [a] ++ lastelem as
                  else lastelem as

lastelem [x] = x
lastelem (x:xs) = lastelem xs

-- 5 - existe item x na lista (True ou False)

exist [] _ = False
exist (a:as) x = if a == x
                 then True || exist as x
                 else False || exist as x

existx [] _ = False
existx (x:xs) e
  | x == e = True
  | otherwise = existx xs e

-- 6 - dado n gera a lista de n a 1

genlist n = if n==1
            then [n]
            else [n] ++ genlist (n-1)

gerlist n = gerlist' n []
        where gerlist' 0 acc = acc
              gerlist' n acc = gerlist' (n-1) (acc++[n])

-- 7 - posição do item na lista (0 se nao esta la, 1 se é o primeiro)

elempos [] _ = 0
elempos (a:as) x = if x == a
                   then 1
                   else if elempos as x == 0
                        then 0
                        else 1 + elempos as x

elempos l e = elempos' l e 0
        where elempos' [] _ acc = 0
              elempos' (x:xs) e acc = if x == e
                                      then acc+1
                                      else elempos' xs e (acc+1)

-- 8 - conta quantas vezes o item aparece na lista (0 se nenhuma)

conta [] _ = 0
conta (a:as) x = if a == x
                 then 1 + conta as x
                 else conta as x

-- 9 - maior elemento de uma lista (refazer com acumulador)

maior [] = 0
maior (a:as) = if a > maior as
               then a
               else maior as

-- 10 - reverte uma lista

revert [] = []
revert (a:as) = revert as ++ [a]

-- 11 - dado n gera a lista de 1 a n

genlistord n = if n==1
               then [n]
               else genlistord (n-1) ++ [n]

-- 12 - retorna a lista sem o ultimo elemento

eraselast [] = []
eraselast (a:as) = if as == []
                   then []
                   else [a] ++ eraselast as

-- 13 - soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1)

sumpospar l = fst (foldl (\(s,pos) x -> if (mod (snd (s,pos)) 2) == 0 then (s+x,pos+1) else (s,pos+1)) (0,0) l)

-- 14 - intercala 2 listas (intercala1 e intercala2)
intercala1 [1,2,3] [4,5,6,7,8]
 ==> [1,4,2,5,3,6]

intercala [] [] = []
intercala [] (b:bs) = [b] ++ intercala [] bs 
intercala (a:as) [] = [a] ++ intercala as [] 
intercala (a:as) (b:bs) = [a] ++ [b] ++ intercala as bs

-- 15 - a lista ja esta ordenada?

alord l = foldl (\(res,laux) x -> if tail(laux) == [] then (res,laux) else if (x < head(tail(laux)) && fst (res,laux) == True) then (True,tail(laux)) else (False,tail(l))) (True,l) l

-- 16 - shift right
-- shiftr [1,2,3,4]
-- ==> [4,1,2,3]

shiftr l = shiftr' l []
    where shiftr' [x] acc = [x] ++ acc
          shiftr' (x:xs) acc = shiftr' xs (acc++[x])

-- 17 - shiftr n lista (shift right n vezes)
-- 18 - shift left

shiftl [] = []
shiftl (a:as) = as ++ [a]

-- 19 - shift left n vezes
-- 20 - remove item da lista (1 vez so)

removefirst [] _ = []
removefirst (a:as) x = if a == x then 
		         as
                       else [a] ++ removefirst as x

-- 21 - remove item da lista (todas as vezes)

removeall [] _ = []
removeall (a:as) x = if a == x
                       then removeall as x
                       else [a] ++ removeall as x

rem' it l = foldr' (\x res -> if it==x then res else x:res) [] l

-- 22 - remove item da lista n (as primeiras n vezes)
-- 23 - remove item da lista (a ultima vez que ele aparece) **

removelast [] _ = []
removelast (a:as) x = if x /= a 
                      then [a] ++ removelast as x
                      else if elem x as
                           then [a] ++ removelast as x
                           else removelast as x

-- 24 - troca velho por novo na lista (1 so vez)

trocaR velho novo l = foldr' (\x res -> if x==velho then novo:res else x:res) [] l
trocaL velho novo l = foldl' (\ac x -> if x==velho then ac++[novo] else ac++[x]) [] l

-- 25 - troca velho por novo na lista (todas vezes)
-- 26 - troca velho por novo na lista n (as primeiras n vezes)
