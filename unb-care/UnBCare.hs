module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem 
prestados a um paciente. 

O paciente tem um receituario médico, que indica os medicamentos 
a serem tomados com seus respectivos horários durante um dia. 

Esse receituário é organizado em um plano de medicamentos que estabelece, 
por horário, quais são os remédios a serem tomados. 

Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.

Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar 
medicamento, seja comprar medicamento. 

Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano. 

O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs

Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-
  Funções auxiliares polimórficas:
-}

merge :: (Ord t) => [t] -> [t] -> (t -> t -> Bool) -> [t]
merge [] [] comparator = []
merge a  [] comparator = a
merge [] b  comparator = b
merge (a:as) (b:bs) comparator
    | comparator a b = a:merge as (b:bs) comparator
    | otherwise      = b:merge (a:as) bs comparator

mergesort :: (Ord t) => [t] -> (t -> t -> Bool) -> [t]
mergesort []     comparator = []
mergesort (a:[]) comparator = [a]
mergesort (a:as) comparator = 
    let middle = floor (fromIntegral (length (a:as))/2)
        firstHalf  = mergesort (take middle (a:as)) comparator
        secondHalf = mergesort (drop middle (a:as)) comparator
    in merge firstHalf secondHalf comparator

quicksort :: [t] -> (t -> t -> Bool) -> [t]
quicksort [] comparator = []
quicksort (x:xs) comparator = [a | a <- xs, (comparator a x)] ++ [x] ++ [a | a <- xs, not (comparator a x)]

isSorted :: [t] -> (t -> t -> Bool) -> Bool
isSorted [] comparator         = True
isSorted (hd:[]) comparator    = True
isSorted (hd:nk:tl) comparator = (comparator hd nk) && isSorted (nk:tl) comparator

isMatrixSorted :: [[t]] -> (t -> t -> Bool) -> Bool
isMatrixSorted [] comparator = True
isMatrixSorted (hd:tl) comparator = isSorted hd comparator && (isMatrixSorted tl comparator)

intersection :: (Eq t) => [t] -> [t] -> [t]
intersection [] []     = []
intersection [] b      = []
intersection (a:as) [] = []
intersection (a:as) b
  | a `elem` b = a:intersection as b
  | otherwise = intersection as b

removeDuplicates :: (Eq t) => [t] -> [t] -> [t]
removeDuplicates [] [] = []
removeDuplicates a [] = a
removeDuplicates a (b:bs)
  | not (b `elem` a) = removeDuplicates (b:a) bs
  | otherwise = removeDuplicates a bs

unionUnique :: (Ord t) => [t] -> [t] -> [t]
--unionUnique a b = reverse (removeDuplicates [] (quicksort (a ++ b) (<)))
unionUnique a b = reverse (removeDuplicates [] (mergesort (a ++ b) (<)))

-- filterMatrix :: [[t]] -> (t -> t -> Bool) -> [[t]]
-- filterMatrix [] comparator = []
-- filterMatrix (hd:tl) comparator = filter comparator hd : filterMatrix tl comparator

{-
  QUESTÃO 1, VALOR: 1,0 ponto

  Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, 
  uma quantidade e um estoque inicial de medicamentos, retorne um novo estoque de medicamentos 
  contendo o medicamento adicionado da referida quantidade. 

  Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada 
  no novo estoque.

  Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio 
  e sua quantidade como cabeça.
-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q [] = (m, q):[]
comprarMedicamento m q ((fm, fq):em)
  | m /= fm && (not (m `elem` [x | (x, y) <- em])) = [(m, q)] ++ ((fm, fq):em)
  | m == fm = (fm, fq + q):em
  | otherwise = (fm, fq):(comprarMedicamento m q em)

{-
  QUESTÃO 2, VALOR: 1,0 ponto

  Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de 
  um estoque de medicamentos, retorna um novo estoque de medicamentos, resultante de 1 comprimido do 
  medicamento ser ministrado ao paciente.

  Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar 
  Just v, onde v é o novo estoque.
-}

maybeEstoque :: (Medicamento, Quantidade) -> Maybe EstoqueMedicamentos -> Maybe EstoqueMedicamentos
maybeEstoque _  Nothing   = Nothing
maybeEstoque mq (Just em) = Just (mq : em)

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento m [] = Nothing
tomarMedicamento m ((fm, fq):em)
  | m == fm && fq > 0 = Just ((fm, fq - 1):em)
  | m /= fm || (m == fm && fq == 0) = maybeEstoque (fm, fq) (tomarMedicamento m em)

{-
  QUESTÃO 3  VALOR: 1,0 ponto

  Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento 
  e de um estoque de medicamentos, retorne a quantidade desse medicamento no estoque.

  Se o medicamento não existir, retorne 0.
-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento m [] = 0
consultarMedicamento m ((fm, fq):em)
  | m == fm = fq
  | otherwise = consultarMedicamento m em

{-
  QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos 
  os medicamentos por um dia a partir do receituario. 
  
  O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente pelo nome do 
  medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de 
  vezes que cada remédio é tomado.
-}

comparadorEstoqueNomeEq :: (Medicamento, Quantidade) -> (Medicamento, Quantidade) -> Bool
comparadorEstoqueNomeEq (m1, q1) (m2, q2) = m1 <= m2 

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos []          = []
demandaMedicamentos ((m, hs):r) = 
  --quicksort ((m, length hs):demandaMedicamentos r) comparadorEstoqueNomeEq
  mergesort ((m, length hs):demandaMedicamentos r) comparadorEstoqueNomeEq

{-
  QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

  Um receituário é válido se, e somente se, todos os medicamentos são distintos e estão ordenados
  lexicograficamente e, para cada medicamento, seus horários também estão ordenados e são distintos.

  Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também 
  estão ordenados e são distintos, e para cada horário, os medicamentos são distintos e são ordenados 
  lexicograficamente.

  Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos 
  tipos são dados abaixo:
 -}

receituarioValido :: Receituario -> Bool
receituarioValido []           = True
receituarioValido ((m, hs):rt) = 
  isSorted       (m:[x | (x, _) <- rt])   (<) &&
  isMatrixSorted (hs:[x | (_ , x) <- rt]) (<)

planoValido :: PlanoMedicamento -> Bool
planoValido []           = True
planoValido ((h, ms):pt) =
  isSorted       (h:[x | (x, _) <- pt])   (<) &&
  isMatrixSorted (ms:[x | (_ , x) <- pt]) (<)

{-
  QUESTÃO 6  VALOR: 1,0 ponto,

  Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

  1. Os horários da lista são distintos e estão em ordem crescente;
  2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento 
    (e.g. `[Comprar m1, Medicar m1 x]`);
  3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

  Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:
 -}

getCuidadoMedicamento :: Cuidado -> String
getCuidadoMedicamento (Comprar m _) = m
getCuidadoMedicamento (Medicar m)   = m

getCuidadoQuantidade :: Cuidado -> Int
getCuidadoQuantidade (Comprar _ q) = q

getCuidadoType :: Cuidado -> String
getCuidadoType (Comprar _ _) = "Comprar"
getCuidadoType (Medicar _)   = "Medicar"

plantaoValido :: Plantao -> Bool
plantaoValido []           = True
plantaoValido ((h, cr):pl) =
  isSorted ([x | (x, _) <- ((h, cr):pl)]) (<) &&

  (foldr (+) 0 [length (intersection [getCuidadoMedicamento x 
  | x <- y, getCuidadoType x == "Medicar"] [getCuidadoMedicamento x 
  | x <- y, getCuidadoType x == "Comprar"]) 
  | y <- [z | (_, z) <- ((h, cr):pl)]]) == 0 &&

  isMatrixSorted [[getCuidadoMedicamento x 
  | x <- y, getCuidadoType x == "Medicar"] 
  | y <- [z | (_, z) <- ((h, cr):pl)]] (<)

{-
  QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario 
  válido, retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de 
  medicamentos  é uma disposição ordenada por horário de todos os remédios que devem ser tomados 
  pelo paciente em um certo horário.
-}

filtraHorarios :: [Horario] -> Receituario -> PlanoMedicamento
filtraHorarios [] r      = []
filtraHorarios (hh:ht) r = (hh, [x | (x, y) <- r, hh `elem` y]):filtraHorarios ht r

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario ((m, hs):r) = 
  filtraHorarios (foldr unionUnique [] [x | (_, x) <- ((m, hs):r)]) ((m, hs):r)


{- 
  QUESTÃO 8  VALOR: 1,0 ponto

  Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um 
  receituário válido a partir de um plano de medicamentos válido.

  Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? 
  Caso exista, essa simetria permite compararmos a função geraReceituarioPlano com a função 
  geraPlanoReceituario ? Em outras palavras, podemos definir geraReceituarioPlano com base em 
  geraPlanoReceituario ?
-}

filtraMedicamentos :: [Medicamento] -> PlanoMedicamento -> Receituario
filtraMedicamentos [] pl      = []
filtraMedicamentos (mh:mt) pl = (mh, [x | (x, y) <- pl, mh `elem` y]):filtraMedicamentos mt pl

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano ((h, ms):pl) = 
  filtraMedicamentos (foldr unionUnique [] [x | (_, x) <- ((h, ms):pl)]) ((h, ms):pl)

{-
  QUESTÃO 9 VALOR: 1,0 ponto

  Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido 
  a partir de um estoque de medicamentos, resultando em novo estoque. A execução consiste em 
  desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
  
  Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser 
  Nothing. Caso contrário, o resultado deve ser Just v, onde v é o valor final do estoque de 
  medicamentos
-}

getCurrentEstoque :: Maybe EstoqueMedicamentos -> EstoqueMedicamentos
getCurrentEstoque (Just v) = v

executaCuidados :: [Cuidado] -> Maybe EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidados _ Nothing = Nothing
executaCuidados [] (Just em) = Just em
executaCuidados (ch:ct) (Just em)
  | getCuidadoType ch == "Medicar" = executaCuidados ct (tomarMedicamento (getCuidadoMedicamento ch) (getCurrentEstoque (Just em)))
  | otherwise = executaCuidados ct (Just (comprarMedicamento (getCuidadoMedicamento ch) (getCuidadoQuantidade ch) (getCurrentEstoque (Just em))))

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] em = Just em
executaPlantao pl em =
  executaCuidados (foldr (++) [] [x | (_, x) <- pl]) (Just em)

{-
  QUESTÃO 10 VALOR: 1,0 ponto

  Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido 
  satisfaz um plano de medicamento válido para um certo estoque, ou seja, a função "satisfaz" 
  deve verificar se a execução do plantão implica terminar com estoque diferente de Nothing e 
  administrar os medicamentos prescritos no plano.

  Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução 
  do plantão.

  Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
  juntamente com ministrar medicamento.
-}

checkHorarioPlano :: (Horario, [Cuidado]) -> PlanoMedicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
checkHorarioPlano (h, cr) [] em = Nothing
checkHorarioPlano (h, cr) ((hpl, mpl):plt) em
  | h == hpl && [getCuidadoMedicamento x | x <- cr, getCuidadoType x == "Medicar"] == mpl = executaPlantao [(h, cr)] em
  | otherwise = checkHorarioPlano (h, cr) plt em

removePlanoMedicamento :: Horario -> PlanoMedicamento -> PlanoMedicamento
removePlanoMedicamento h [] = []
removePlanoMedicamento h ((hh, mh):plt)
  | h == hh = removePlanoMedicamento h plt
  | otherwise = (hh, mh):removePlanoMedicamento h plt

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz [] [] _ = True
satisfaz p  [] _ = False
satisfaz [] pl _ = False
satisfaz ((h, cr):pt) pl em
  | (checkHorarioPlano (h, cr) pl em) == Nothing = False
  | otherwise = satisfaz pt (removePlanoMedicamento h pl) (getCurrentEstoque (executaPlantao [(h, cr)] em))

{-
  QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto
  
  Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que 
  satisfaz um plano de medicamentos válido e um estoque de medicamentos.
  
  Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.
-}

geraDemandaCuidados :: [Medicamento] -> EstoqueMedicamentos -> [Cuidado]
geraDemandaCuidados [] _ = []
geraDemandaCuidados (mh:mt) em
  | tomarMedicamento mh em == Nothing = (Comprar mh 1):(Medicar mh):geraDemandaCuidados mt (comprarMedicamento mh 1 em)
  | otherwise = (Medicar mh):geraDemandaCuidados mt (getCurrentEstoque (tomarMedicamento mh em))

geraNovoEstoque :: [Cuidado] -> EstoqueMedicamentos -> EstoqueMedicamentos
geraNovoEstoque [] [] = []
geraNovoEstoque [] em = em
geraNovoEstoque (ch:ct) em
  | getCuidadoType ch == "Comprar" = geraNovoEstoque ct (comprarMedicamento (getCuidadoMedicamento ch) (getCuidadoQuantidade ch) em)
  | otherwise = geraNovoEstoque ct em

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto [] _ = []
plantaoCorreto ((h, ms):plt) em = 
  let demandaCuidados = geraDemandaCuidados ms em
      novoEstoque = geraNovoEstoque demandaCuidados em
  in (h, demandaCuidados):plantaoCorreto plt novoEstoque

