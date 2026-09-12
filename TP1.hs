{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

module TP1 where

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
foldCircuito fCaja fSerie fParalelo = recCircuito fCaja (\w x y z -> fSerie w x) (\a b c d e f g h -> fParalelo a b c d)

-- 3 invertido

invertido :: Circuito -> Circuito
invertido = recCircuito Caja (\rx ry x y -> Serie ry rx) (\rc1 rx ry rc2 c1 x y c2 -> Paralelo c2 ry rx c1)

circuitolol :: Circuito
circuitolol =
  Serie
    cajaOn
    (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada off) on)

circuitodadovuelta :: Circuito
circuitodadovuelta =
  Serie
    ( Paralelo
        on
        (Paralelo off cajaNada cajaOn on)
        (Paralelo Nada cajaOn cajaOff Nada)
        on
    )
    cajaOn

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado =
  foldCircuito
    ( \c -> case c of
        Bombilla True -> True
        Bombilla False -> False
        Nada -> False
    )
    (\rx ry -> rx || ry)
    (\rw rx ry rz -> rw || rx || ry || rz)

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

test1 :: Circuito
test1 =
  invertido
    ( Serie
        cajaOn
        (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada off) on)
    )

oraculocaja :: [Caja]
oraculocaja = [on, off, Nada, on, on, Nada, on, off, Nada, on, on]

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo =
  recCircuito
    (\x -> True)
    ( \rx ry x y -> case y of
        Serie a b -> False
        _ -> True
    )
    (\rc1 rx ry rc2 c1 x y c2 -> rx && ry)

circuitoProlijo :: Circuito
circuitoProlijo = Serie (Serie cajaOn cajaOff) cajaOn

circuitoNOProlijo :: Circuito
circuitoNOProlijo = Serie cajaOn (Serie cajaOff cajaOn)

-- 8: circuitoEmprolijado

circuitoEmprolijado :: a
circuitoEmprolijado = undefined -- ESTE EJERCICIO NO SE HACE

-- 9: tienenLaMismaEstructura
mapEstructura :: Circuito -> [String]
mapEstructura = foldCircuito (\x -> ["Caja"]) (\x y -> x ++ y) (\w x y z -> ["Paralelo"] ++ x ++ y)

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura c1 c2 = mapEstructura c1 == mapEstructura c2

-- 10: subCircuitoMásResistente

resistenciaCircuito :: Circuito -> Float
resistenciaCircuito _ = 100 -- Temporario, dado

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente =
  foldCircuito
    Caja
    Serie
    (\_ a b _ -> if resistenciaCircuito a > resistenciaCircuito b then a else b)

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

-> {L1} (cajaAlternada . cajaAlternada) (Bombilla booleano) = id (Bombilla booleano)

---

Por ambos casos donde Caja es Nada o un Bombilla, (cajaAlternada . cajaAlternada) = id

---

       (alternado . alternado) (Caja caja)
{C}  = alternado (alternado (Caja caja))
{AC  = alternado (Caja (cajaAlternada caja))
{AC  = Caja (cajaAlternada (cajaAlternada caja))
{C}  = Caja ((cajaAlternada . cajaAlternada) caja)
{L1} = Caja (id caja)
{I}  = Caja caja
{I}  = id (Caja caja)

-> {L2} (alternado . alternado) (Caja caja) = id (Caja caja)

---

       (alternado . alternado) (Serie ci cf)
{C}  = alternado (alternado (Serie ci cf))
{AS  = alternado (Serie (alternado ci) (alternado cf))
{AS  = Serie (alternado (alternado ci)) (alternado (alternado cf))
{C}  = Serie ((alternado . alternado) ci) ((alternado . alternado) cf)
{HI} = Serie (id ci) (id cf)
{I}  = Serie ci cf
{I}  = id (Serie ci cf)

-> {L3} (alternado . alternado) (Serie ci cf) = id (Serie ci cf)

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

-> {L4} (alternado . alternado) (Paralelo ce ci cd cs) = id (Paralelo ce ci cd cs)

---

Con lemmas 1-4, podemos ver que por todos casos, (alternado . alternado) = id

--}