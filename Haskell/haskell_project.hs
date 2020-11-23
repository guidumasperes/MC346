--RA: 173711.
--NOME: Guilherme Dumas Peres.

--Vamos seguir o algoritmo proposto:
--1)separar os pontos com e sem label.
--2)calcular a distancia entre os pontos dos 2 grupos.
--3)selecionar o ponto do grupo sem classe mais proximo de algum do grupo com classe.
--4)atribuir a classe a este ponto e atualizar os 2 grupos.
--5)repetir.

 --O código foi desenvolvido e testado no site https://www.tutorialspoint.com/compile_haskell_online.php
 
module Main where
import Debug.Trace
import Data.List
import Text.Read

main = do
      contents <- getContents --Cada linha e um ponto exceto as que vem depois da linha em branco que são labels
      let all_lines = lines contents --Agora all_lines contem uma lista ["aa x1 x2","bb y1 y2", "cc y3 y4", ..., " ", "aa 1", ...] com os pontos e labels, precisamos separá-los!
          points_and_labels = split_points_and_labels all_lines
          points = fst points_and_labels --Coloca pontos de um lado
          labels = snd points_and_labels --Labels do outro
          label_or_not = organize_points points labels --Vamos separar quem tem label ou nao
          labeled = fst label_or_not --Pontos com label
          not_labeled = snd label_or_not --Pontos sem label
          clusters = clustering labeled not_labeled labels --Fazer a clusterizacao
          correct_format = organize clusters --Colocar no formato desejado
          new_correct_format = Data.List.sort correct_format
      mapM_ (putStrLn . show_tup) new_correct_format

show_tup :: (String,[String]) -> String
show_tup (a,b) = "(" ++ (show (read a :: Integer)) ++ ", " ++ (show b) ++ ")"

split_points_and_labels :: [String] -> ([[String]],[[String]])
split_points_and_labels l = split_points_and_labels' l []
                            where split_points_and_labels' (x:xs) acc = if x == "\r" then
                                                                      (acc,labels_list xs)
                                                                    else
                                                                      split_points_and_labels' xs (acc ++ [words x])

labels_list :: [String] -> [[String]]
labels_list l = labels_list' l []
                where labels_list' [] acc = acc
                      labels_list' (x:xs) acc = labels_list' xs (acc ++ [words x])

organize_points :: [[String]] -> [[String]] -> ([[String]],[[String]])
organize_points p l = organize_points' p l [] []
                      where organize_points' [] _ accl accnl = (accl,accnl)
                            organize_points' (x:xs) l accl accnl = if check (head x) l then
                                                                    organize_points' xs l ([x] ++ accl) accnl
                                                                   else
                                                                    organize_points' xs l accl ([x] ++ accnl)

check :: String -> [[String]] -> Bool
check _ [] = False
check p (x:xs) = if check_aux p x then
                  True
                 else
                  check p xs

check_aux :: String -> [String] -> Bool
check_aux e [] = False
check_aux e (x:xs) = if e == x then
                      True
                     else 
                      check_aux e xs

clustering :: [[String]] -> [[String]] -> [[String]] -> [[String]]
clustering _ [] lbs = lbs
clustering lb (x:xs) lbs = let better_choice = find_dist x lb
                           in clustering (lb ++ [x]) xs (find_label lbs (head x) (fst better_choice))

find_label :: [[String]] -> String -> String ->[[String]]
find_label l lp lr = find_label' l lp lr []
                     where  find_label' [] _ _ acc = acc
                            find_label' (x:xs) lp lr acc = if check_aux lr x then
                                                                find_label' xs lp lr [[lp]++x]++acc
                                                            else
                                                                find_label' xs lp lr [x]++acc

organize :: [[String]] -> [(String,[String])]
organize l = organize' l []
             where organize' [] acc = acc
                   organize' (x:xs) acc = let e = organizeaux x
                                              nl = removefirst x e
                                              sl = Data.List.sort nl
                                              in organize' xs (acc++[(e,sl)])

organizeaux :: [String] -> String
organizeaux [] = "something_wrong"
organizeaux (x:xs) = if (readMaybe x :: Maybe Int) == Nothing then
                        organizeaux xs
                     else
                        x

removefirst :: [String] -> String -> [String]
removefirst [] _ = []
removefirst (a:as) x = if a == x then 
                        as
                      else 
                        [a] ++ removefirst as x

find_dist :: [String] -> [[String]] -> (String,Float)
find_dist p lb = find_dist' p lb (" ", maxValue)
                 where find_dist' _ [] acc = acc
                       find_dist' p (x:xs) acc = let pac = tail p
                                                     plb = tail x
                                                     dist = n_euclidian pac plb 0.0
                                                  in if dist < snd acc then
                                                      find_dist' p xs (head x, dist)
                                                     else
                                                      find_dist' p xs acc

--Vamos usar a distancia euclidiana n-dimensional para calcular a distancia entre os pontos.
n_euclidian :: [String] -> [String] -> Float -> Float
n_euclidian [] [] acc = sqrt acc
n_euclidian (x:xs) (y:ys) acc = let p1 = read x :: Float
                                    p2 = read y :: Float
                                in n_euclidian xs ys acc+(p1-p2)**2.0

--Nao consegui importar o modulo Numeric.Limits entao vou adicionar a funcao que preciso aqui:

-- | The maximum finite value for the type.
{-# SPECIALIZE maxValue :: Double #-}
{-# SPECIALIZE maxValue :: Float #-}
maxValue :: (RealFloat a) => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)
