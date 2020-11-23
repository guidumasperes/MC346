--> 1 - posicoes - dado um item e uma lista, retorna uma lista com todas as posicoes (primeiro elemento esta na posicao 1) do item na lista

positem l e = positem' l e 1
              where positem' [] _ acc = []
                    positem' (x:xs) e acc = if e == x
                                            then [acc] ++ positem' xs e (acc+1) 
                                            else positem' xs e (acc+1)

--> 2 - split - dado um item e uma lista retorna uma lista de listas, todos os elementos da lista antes do item (a primeira vez que ele aparece) e todos depois
--> split "qwertyuiopoiuyt" 't' ==> ["qwer", "yuiopoiuyt"]

split l e = split' l e []
            where split' [] e acc = [acc]
                  split' (x:xs) e acc = if x == e
                                        then [acc] ++ [xs]
                                        else split' xs e (acc++[x])

--> 3 - splitall - mesma coisa que o split mas retorna todas as sublistas
--> splitall "qwertyuiopoiuytxxt" 't' ==> ["qwer", "yuiopoiuy", "xx", ""]  ou  ["qwer", "yuiopoiuy", "xx"]

splitall l e = splitall' l e []
            where splitall' [] e acc = [acc]
                  splitall' (x:xs) e acc = if x == e
                                        then [acc] ++ splitall' xs e []
                                        else splitall' xs e (acc++[x])

--> 4 - drop n lista - a lista sem os n primeiros elementos

dropelem _ [] = []
dropelem n (x:xs) = if n == 0
                    then [x] ++ xs
                    else dropelem (n-1) xs

dropelem n l = dropelem' n l []
               where dropelem' _ [] acc = acc
                     dropelem' n (x:xs) acc = if n == 0
                                              then dropelem' n xs (acc++[x])
                                              else dropelem' (n-1) xs acc

--> 5 - take n lista - os primeiros n elementos da lista

takelem _ [] = []
takelem n (x:xs) = if n == 0
                then []
                else [x] ++ takelem (n-1) xs

takelem n l = takelem' n l []
            where takelem' _ [] acc = acc
                  takelem' n (x:xs) acc = if n == 0
                                          then acc
                                          else takelem' (n-1) xs (acc++[x])
