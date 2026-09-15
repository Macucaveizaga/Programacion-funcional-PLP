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
circuitoEmprolijado :: a
circuitoEmprolijado = undefined -- ESTE EJERCICIO NO SE HACE

-- 9: tienenLaMismaEstructura
mapEstructura :: Circuito -> [String]
mapEstructura = foldCircuito (\x -> ["Caja"]) (\x y -> x ++ ["Serie"] ++ y) (\w x y z -> ["Paralelo"] ++ x ++ y)

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura c1 c2 = mapEstructura c1 == mapEstructura c2

-- 10: subCircuitoMásResistente

-- función custom propia, auxiliar para punto 10
resistenciaCircuito :: Circuito -> Float
resistenciaCircuito c = case c of
  Caja c -> case c of
    Bombilla True -> 1.0
    Bombilla False -> 2.0
    Nada -> 10.0
  Serie a b -> -0.4 * rec a + 2 * rec b
  Paralelo c1 ci cd c2 -> 1.5 * rec ci + 0.5 * rec cd + rec (Caja c1) - 3 * rec (Caja c2) -- las lamparas, a pesar de no ser circuitos, afectan la resistencia del paralelo.
  where
    rec = resistenciaCircuito

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente =
  recCircuito
    (\x -> Caja x)
    (\recx recy x y -> maximumBy (comparing resistenciaCircuito) [Serie x y, recx, recy])
    (\rc1 rx ry rc2 c1 x y c2 -> maximumBy (comparing resistenciaCircuito) [Paralelo c1 x y c2, rx, ry])

{-- 11: Demostrar: alternado . alternado = id

-Enunciado:

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

-Auxiliares:

QVQ ∀ b :: Bool, not (not b) = b {NB}
Por lema de generación, tenemos que un booleano puede ser True o False

Caso b = True:

       not (not True)
{NT} = not False
{NF} = True

Caso b = False:

       not (not False)
{NT} = not True
{NF} = False

Por ambos casos podemos ver que ∀ b :: Bool
-> not (not b) = b {NB}

---
QVQ cajaAlternada . cajaAlternada = id {CAID}
Por extensionalidad:
QVQ ∀ caja :: Caja, (cajaAlternada . cajaAlternada) caja = id caja

Caso caja = Nada:

        (cajaAlternada . cajaAlternada) Nada
{C}   = cajaAlternada (cajaAlternada Nada)
{CAN} = cajaAlternada Nada
{CAN} = Nada
{I}   = id Nada

-> (cajaAlternada . cajaAlternada) Nada = id Nada {CAIDN}

Caso caja = Bombilla b, para cualquier b :: Bool:

        (cajaAlternada . cajaAlternada) (Bombilla b)
{C}   = cajaAlternada (cajaAlternada (Bombilla b))
{CAB} = cajaAlternada (Bombilla (not b))
{CAB} = Bombilla (not (not b))
{NB}  = Bombilla b
{I}   = id (Bombilla b)

-> (cajaAlternada . cajaAlternada) (Bombilla b) = id (Bombilla b) {CAIDB}

Por ambos casos {CAIDN} y {CAIDB}, vemos que (cajaAlternada . cajaAlternada) = id {CAID}
---
-Demostración:

QVQ alternado . alternado = id
Por extensionalidad, es equivalente a demostrar que: ∀ c :: Circuito, alternado . alternado c = id c
Vamos a hacer inducción estructural sobre c:
QVQ
P(c) = alternada . alternada c = id c

Caso base: c = Caja caja, para cualquier caja::Caja

       (alternado . alternado) (Caja caja)
{C}    = alternado (alternado (Caja caja))
{AC}   = alternado (Caja (cajaAlternada caja))
{AC}   = Caja (cajaAlternada (cajaAlternada caja))
{C}    = Caja ((cajaAlternada . cajaAlternada) caja)
{CAID} = Caja (id caja)
{I}    = Caja caja
{I}    = id (Caja caja)

-> (alternado . alternado) (Caja caja) = id (Caja caja)

Caso inductivo: c = Serie ci cf
∀ ci,cf :: Circuito. P(ci), P(cf) {HI-S} => P(Serie ci cf)

           (alternado . alternado) (Serie ci cf)
{C}      = alternado (alternado (Serie ci cf))
{AS}     = alternado (Serie (alternado ci) (alternado cf))
{AS}     = Serie (alternado (alternado ci)) (alternado (alternado cf))
{C}      = Serie ((alternado . alternado) ci) ((alternado . alternado) cf)
{HI-S}x2 = Serie (id ci) (id cf)
{I}      = Serie ci cf
{I}      = id (Serie ci cf)

-> (alternado . alternado) (Serie ci cf) = id (Serie ci cf)

Caso inductivo: c = Paralelo ce ci cd cs
∀ ce,cs :: Caja, ∀ ci,cd :: Circuito. P(ci), P(cd) {HI-P} => P(Paralelo ce ci cd cs)

           (alternado . alternado) (Paralelo ce ci cd cs)
{C}      = alternado (alternado (Paralelo ce ci cd cs))
{AP}     = alternado (Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs))
{AP}     = Paralelo (cajaAlternada (cajaAlternada ce)) (alternado (alternado ci)) (alternado (alternado cd)) (cajaAlternada (cajaAlternada cs))
{C}      = Paralelo ((cajaAlternada . cajaAlternada) ce) ((alternado . alternado) ci) ((alternado . alternado) cd) ((cajaAlternada . cajaAlternada) cs)
{CAID}x2 = Paralelo (id ce) ((alternado . alternado) ci) ((alternado . alternado) cd) (id cs)
{HI-P}x2 = Paralelo (id ce) (id ci) (id cd) (id cs)
{I}x4    = Paralelo ce ci cd cs
{I}      = id (Paralelo ce ci cd cs)

-> (alternado . alternado) (Paralelo ce ci cd cs) = id (Paralelo ce ci cd cs)

Habiendo probado el caso base y los dos casos inductivos (y por extensionalidad), demostramos que (alternado . alternado) = id

--}
