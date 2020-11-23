data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

--> 1 - acha um item numa arvore de busca binaria

finditem :: (Ord a) => a -> Tree a -> Bool
finditem _ EmptyTree = False
finditem i (Node a l r)
  | a == i = True
  | a < i = finditem i r
  | a > i = finditem i l

--> 2 - verifica se uma arvore é um abb

verbst :: (Ord a) => Tree a -> Bool
verbst EmptyTree = True
verbst (Node _ EmptyTree EmptyTree) = True
verbst (Node a EmptyTree r) = verbst r && a < menor r
verbst (Node a l EmptyTree) = verbst l && a > maior l
verbst (Node a l r) = verbst l && verbst r && a < menor r && a > maior l 

maior (Node a _ EmptyTree) = a
maior (Node a _ r) = maior r

menor (Node a EmptyTree _) = a
menor (Node a l _) = menor l

--> 3 - insere um item numa abb

insertitem :: (Ord a) => a -> Tree a -> Tree a
insertitem i EmptyTree = singleton i
insertitem i (Node a l r)
  | a == i = Node a l r
  | a < i = Node a l (insertitem i r)
  | a > i = Node a (insertitem i l) r

--> 4 - remove um item de uma abb

remitem :: (Ord a) => a -> Tree a -> Tree a
remitem _ EmptyTree = EmptyTree
remitem i (Node a l r)
  | a == i = auxrem (Node a l r)
  | a < i = Node a l (remitem i r)
  | a > i = Node a (remitem i l) r

auxrem :: (Ord a) => Tree a -> Tree a
auxrem (Node a EmptyTree EmptyTree) = EmptyTree
auxrem (Node a l EmptyTree) = l
auxrem (Node a EmptyTree r) = r
auxrem (Node a l r) = Node (maior r) l (delrleaf r)

delrleaf (Node a _ EmptyTree) = EmptyTree 
delrleaf (Node a _ r) = if delrleaf r == EmptyTree
                        then r 
                        else r

--> 5 - calcula a profundidade maxima de uma abb

maxprof :: (Ord a) => Tree a -> Integer
maxprof EmptyTree = 0
maxprof (Node a l r) = if maxl l > maxr r
                       then maxl l + 1
                       else maxr r + 1

maxl (Node a EmptyTree _) = 1
maxl (Node a l _) = 1 + maxl l

maxr (Node a _ EmptyTree) = 1
maxr (Node a _ r) = 1 + maxr r

--> 6 - coverte uma abb numa lista em ordem infixa (arvore-esquerda, no, arvore-direita)

bstinorder :: (Ord a) => Tree a -> [a]
bstinorder EmptyTree = []
bstinorder (Node a l r) = bstinorder l ++ [a] ++ bstinorder r

--> 7 - converte uma abb numa lista em ordem prefixa (no, ae, ad)

bstpreorder :: (Ord a) => Tree a -> [a]
bstpreorder EmptyTree = []
bstpreorder (Node a l r) = [a] ++ bstpreorder l ++ bstpreorder r

--> 8 - converte uma lista em uma abb

buildbst :: (Ord a) => [a] -> Tree a
buildbst [] = EmptyTree
buildbst (x:xs) = insertitem x (buildbst xs)
