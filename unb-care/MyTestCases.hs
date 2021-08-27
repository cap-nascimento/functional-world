module MyTestCases where

import ModeloDados
import UnBCare

getEstoqueMedicamento :: EstoqueMedicamentos
getEstoqueMedicamento = [("Aspirina", 4), ("Gardenal", 12), ("Tarja Preta", 7)]

getReceituario :: Receituario
getReceituario = [
    ("Tarja Preta", [1..10]), ("Aspirina", [5..21]), ("Ibuprofeno", [5..20]), 
    ("Gardenal", [4..71]), ("Aspirina", [7..12])]

-- tc1 = comprarMedicamento "Aspirina" 45 getEstoqueMedicamento -- remédio já existente
-- tc2 = comprarMedicamento "Ibuprofeno" 77 getEstoqueMedicamento -- remédio não existente
-- tc3 = comprarMedicamento "Gardenal" 45 getEstoqueMedicamento -- remédio já existente

-- tc1 = tomarMedicamento "Aspirina" getEstoqueMedicamento -- remédio já existente
-- tc2 = tomarMedicamento "Gardenal" getEstoqueMedicamento -- remédio já existente
-- tc3 = tomarMedicamento "Tarja Preta" getEstoqueMedicamento -- remédio já existente
-- tc4 = tomarMedicamento "Ibuprofeno" getEstoqueMedicamento -- remédio já existente

-- tc1 = demandaMedicamentos getReceituario

-- receituario1 :: Receituario
-- receituario1 = [
--     ("Aspirina", [1..10]), ("Bezetacil", [5..11]), ("Gardenal", [2..15]),
--     ("Hidroxicloroquina", [12..16]), ("Ibuprofeno", [7, 8, 9]), ("Paracetamol", [16..20])]

-- receituario2 :: Receituario
-- receituario2 = [
--     ("Aspirina", [1..10]), ("Aspirina", [5..11]), ("Gardenal", [2..15]),
--     ("Hidroxicloroquina", [12..16]), ("Ibuprofeno", [7, 8, 9]), ("Paracetamol", [16..20])]

-- receituario3 :: Receituario
-- receituario3 = [
--     ("Aspirina", [1, 2, 3, 1]), ("Bezetacil", [2, 3, 4, 5])]

-- receituario4 :: Receituario
-- receituario4 = [
--     ("Aspirina", [4, 3, 2, 1]), ("Bezetacil", [2, 3, 4, 5])]

-- receituario5 :: Receituario
-- receituario5 = [
--     ("Paracetamol", [1..10]), ("Ibuprofeno", [5..11]), ("Hidroxicloroquina", [2..15]),
--     ("Gardenal", [12..16]), ("Bezetacil", [7, 8, 9]), ("Aspirina", [16..20])]

-- tc1 = receituarioValido receituario1 -- True
-- tc2 = receituarioValido receituario2 -- False
-- tc3 = receituarioValido receituario3 -- False
-- tc4 = receituarioValido receituario4 -- False
-- tc5 = receituarioValido receituario5 -- false

planoMedicamento1 :: PlanoMedicamento
planoMedicamento1 = [
   (4, ["Aspirina", "Bezetacil", "Gardenal"]), (5, ["Paracetamol", "Ibuprofeno", "Cloroquina"])]

planoMedicamento2 :: PlanoMedicamento
planoMedicamento2 = [
   (4, ["Aspirina", "Bezetacil", "Gardenal"]), (5, ["Cloroquina", "Ibuprofeno", "Paracetamol"])]

planoMedicamento3 :: PlanoMedicamento
planoMedicamento3 = [
   (12, ["Aspirina", "Bezetacil", "Gardenal"]), (10, ["Cloroquina", "Ibuprofeno", "Paracetamol"])]

planoMedicamento4 :: PlanoMedicamento
planoMedicamento4 = [
   (10, ["Aspirina", "Bezetacil", "Gardenal"]), (10, ["Cloroquina", "Ibuprofeno", "Paracetamol"])]

planoMedicamento5 :: PlanoMedicamento
planoMedicamento5 = [
   (23, ["Aspirina", "Aspirina", "Gardenal"]), (24, ["Cloroquina", "Ibuprofeno", "Paracetamol"])]

tc1 = planoValido planoMedicamento1 -- False
tc2 = planoValido planoMedicamento2 -- True
tc3 = planoValido planoMedicamento3 -- False
tc4 = planoValido planoMedicamento4 -- False
tc5 = planoValido planoMedicamento5 -- False