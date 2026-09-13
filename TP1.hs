{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

module TP1 where

import Data.List
import Data.Ord (comparing)

data Caja = Bombilla Bool | Nada
  deriving (Eq)

instance Show Caja where
  show :: Caja -> String
  show = showDeCaja

showDeCaja :: Caja -> String
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito
  = Caja Caja
  | Serie Circuito Circuito
  | Paralelo Caja Circuito Circuito Caja
  deriving (Eq)

instance Show Circuito where
  show :: Circuito -> String
  show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada)
    ++ "{"
    ++ (showDeCircuito circuitoIzquierdo)
    ++ "}"
    ++ "{"
    ++ (showDeCircuito circuitoDerecho)
    ++ "}"
    ++ (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) =
  "("
    ++ (showDeCircuitoConEstructura circuitoInicial)
    ++ "-"
    ++ (showDeCircuitoConEstructura circuitoFinal)
    ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada)
    ++ "{"
    ++ (showDeCircuitoConEstructura circuitoIzquierdo)
    ++ "}"
    ++ "{"
    ++ (showDeCircuitoConEstructura circuitoDerecho)
    ++ "}"
    ++ (showDeCaja cajaSalida)

on :: Caja
on = Bombilla True

off :: Caja
off = Bombilla False

cajaOn :: Circuito
cajaOn = Caja on

cajaOff :: Circuito
cajaOff = Caja off

cajaNada :: Circuito
cajaNada = Caja Nada

-- 1: recCircuito

recCircuito :: (Caja -> b) -> (b -> b -> Circuito -> Circuito -> b) -> (b -> b -> b -> b -> Caja -> Circuito -> Circuito -> Caja -> b) -> Circuito -> b
recCircuito fCaja fSerie fParalelo circ = case circ of
  Caja c -> fCaja c
  Serie a b -> fSerie (rec a) (rec b) a b
  Paralelo a c1 c2 b -> fParalelo (fCaja a) (rec c1) (rec c2) (fCaja b) a c1 c2 b
  where
    rec = recCircuito fCaja fSerie fParalelo

-- 2: foldCircuito

foldCircuito :: (Caja -> b) -> (b -> b -> b) -> (b -> b -> b -> b -> b) -> Circuito -> b
foldCircuito fCaja fSerie fParalelo = recCircuito fCaja (\rx ry _ _ -> fSerie rx ry) (\rc1 rx ry rc2 _ _ _ _ -> fParalelo rc1 rx ry rc2)

-- recursivoCir1 recursivoCir2 cri1circ2
-- caja1 recCircIzq recCircDer caja2

-- 3 invertido

invertido :: Circuito -> Circuito
invertido = recCircuito Caja (\rx ry x y -> Serie ry rx) (\rc1 rx ry rc2 c1 x y c2 -> Paralelo c2 ry rx c1)

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado =
  foldCircuito
    ( \c -> case c of
        Bombilla True -> True
        Bombilla False -> False
        Nada -> False
    )
    (\rx ry -> rx && ry)
    (\rc1 rx ry rc2 -> rc1 && (rx || ry) && rc2)

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas =
  foldCircuito
    ( \c -> case c of
        Bombilla True -> 1
        Bombilla False -> 0
        Nada -> 0
    )
    (\rx ry -> rx + ry)
    (\rc1 rx ry rc2 -> rc1 + rx + ry + rc2)

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito =
  foldCircuito
    (\x -> [x])
    (\w x -> w ++ x)
    (\rc1 rx ry rc2 -> rc1 ++ rx ++ ry ++ rc2)

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo =
  recCircuito
    (\x -> True)
    ( \rx ry x y -> case y of
        Serie a b -> False
        _ -> True && rx
    )
    (\rc1 rx ry rc2 c1 x y c2 -> rx && ry)

-- 8: circuitoEmprolijado
circuitoRaro :: Circuito
circuitoRaro = Serie (Serie cajaOn cajaOff) cajaNada

circuitoEmprolijado :: a
circuitoEmprolijado = undefined -- ESTE EJERCICIO NO SE HACE

-- 9: tienenLaMismaEstructura
mapEstructura :: Circuito -> [String]
mapEstructura = foldCircuito (\x -> ["Caja"]) (\x y -> x ++ ["Serie"] ++ y) (\w x y z -> ["Paralelo"] ++ x ++ y)

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura c1 c2 = mapEstructura c1 == mapEstructura c2

-- tienenLaMismaEstructura2 :: Circuito -> Circuito -> Bool
-- tienenLaMismaEstructura2 c1 c2 = foldCircuito

-- 10: subCircuitoMásResistente

resistenciaCircuito :: Circuito -> Float
resistenciaCircuito c = case c of
  Caja c -> case c of
    Bombilla True -> 1.0
    Bombilla False -> 2.0
    Nada -> 10.0
  Serie a b -> -0.4 * rec a + 2 * rec b
  Paralelo c1 ci cd c2 -> 1.5 * rec ci + 0.5 * rec cd
  where
    rec = resistenciaCircuito

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente =
  recCircuito
    (\x -> Caja x)
    (\recx recy x y -> maximumBy (comparing resistenciaCircuito) [Serie x y, recx, recy])
    (\rc1 rx ry rc2 c1 x y c2 -> maximumBy (comparing resistenciaCircuito) [Paralelo c1 x y c2, rx, ry])

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) = Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

---

       not (not True)
{NT} = not False
{NF} = True

       not (not False)
{NT} = not True
{NF} = False

Por ambos casos podemos ver qué
-> {NB} not (not booleano) = booleano

---

        (cajaAlternada . cajaAlternada) Nada
{C}   = cajaAlternada (cajaAlternada Nada)
{CAN} = cajaAlternada Nada
{CAN} = Nada
{I}   = id Nada

-> {N} (cajaAlternada . cajaAlternada) Nada = id Nada

---

        (cajaAlternada . cajaAlternada) (Bombilla booleano)
{C}   = cajaAlternada (cajaAlternada (Bombilla booleano))
{CAB} = cajaAlternada (Bombilla (not booleano))
{CAB} = Bombilla (not (not booleano))
{NB}  = Bombilla booleano
{I}   = id (Bombilla booleano)

{L1} -> (cajaAlternada . cajaAlternada) (Bombilla booleano) = id (Bombilla booleano)

---

Por ambos casos donde Caja es Nada o un Bombilla, (cajaAlternada . cajaAlternada) = id

---

       (alternado . alternado) (Caja caja)
{C}  = alternado (alternado (Caja caja))
{AC} = alternado (Caja (cajaAlternada caja))
{AC} = Caja (cajaAlternada (cajaAlternada caja))
{C}  = Caja ((cajaAlternada . cajaAlternada) caja)
{L1} = Caja (id caja)
{I}  = Caja caja
{I}  = id (Caja caja)

{L2} -> (alternado . alternado) (Caja caja) = id (Caja caja)

---

           (alternado . alternado) (Serie ci cf)
{C}      = alternado (alternado (Serie ci cf))
{AS}     = alternado (Serie (alternado ci) (alternado cf))
{AS}     = Serie (alternado (alternado ci)) (alternado (alternado cf))
{C}      = Serie ((alternado . alternado) ci) ((alternado . alternado) cf)
{L1, L2} = Serie (id ci) (id cf)
{I}      = Serie ci cf
{I}      = id (Serie ci cf)

{L3} -> (alternado . alternado) (Serie ci cf) = id (Serie ci cf)

---

           (alternado . alternado) (Paralelo ce ci cd cs)
{C}      = alternado (alternado (Paralelo ce ci cd cs))
{AP}     = alternado (Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs))
{AP}     = Paralelo (cajaAlternada (cajaAlternada ce)) (alternado (alternado ci)) (alternado (alternado cd)) (cajaAlternada (cajaAlternada cs))
{C}      = Paralelo ((cajaAlternada . cajaAlternada) ce) ((alternado . alternado) ci) ((alternado . alternado) cd) ((cajaAlternada . cajaAlternada) cs)
{L1}     = Paralelo (id ce) ((alternado . alternado) ci) ((alternado . alternado) cd) (id cs)
{L2, L3} = Paralelo (id ce) (id ci) (id cd) (id cs)
{I}      = Paralelo ce ci cd cs
{I}      = id (Paralelo ce ci cd cs)

{L4} -> (alternado . alternado) (Paralelo ce ci cd cs) = id (Paralelo ce ci cd cs)

---

Con lemmas 1-4, podemos ver que por todos casos, (alternado . alternado) = id

--}
