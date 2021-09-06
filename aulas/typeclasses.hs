-- Válido para qualquer lista, desde que implemente Eq
member [] _ = False
member (a:as) x = (x == a) || member as x

-- Definição de classe
class Visible t where
    toString :: t -> String
    size :: t -> Int

-- Instância de classe
instance Eq Bool where
    True == True = True
    False == False = False
    _ == _ = False