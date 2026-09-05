module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

recCircuito :: (Caja -> b) -> (b -> b -> Circuito -> Circuito -> b) -> (b -> b -> b -> b -> Caja -> Circuito -> Circuito ->Caja -> b )-> Circuito -> b
recCircuito fCaja fSerie fParalelo circ = case circ of 
  Caja c -> fCaja c
  Serie a b -> fSerie (rec a) (rec b) a b 
  Paralelo a c1 c2 b -> fParalelo (fCaja a) (rec c1) (rec c2) (fCaja b) a c1 c2 b
  where rec = recCircuito fCaja fSerie fParalelo 


-- 2: foldCircuito

foldCircuito :: (Caja -> b) -> (b-> b -> b) -> (b -> b -> b -> b -> b) -> Circuito -> b
foldCircuito fCaja fSerie fParalelo circ = case circ of
  Caja c -> fCaja c 
  Serie a b -> recCircuito fCaja (\recx recy x y -> (fSerie x recx) (fSerie y recy)) a b 
  Paralelo c1 a b c2 -> fParalelo (fCaja c1) (rec a) (rec b) (fCaja c2)
  where rec = recCircuito fCaja fSerie fParalelo 


foldr1 :: (a -> b -> b) -> b -> [a] -> b
foldr1 f z lista = recr z (\x xs rec -> f x rec) lista

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x : xs) = f x xs (recr z f xs)
-- 3 invertido

invertido = undefined -- TODO: COMPLETAR

-- 4: hayCaminoIluminado

hayCaminoIluminado = undefined -- TODO: COMPLETAR

-- 5: cantidadPrendidas

cantidadPrendidas = undefined -- TODO: COMPLETAR

-- 6: cajasDeCircuito

cajasDeCircuito = undefined -- TODO: COMPLETAR

-- 7: esCircuitoProlijo

esCircuitoProlijo = undefined -- TODO: COMPLETAR

-- 8: circuitoEmprolijado

circuitoEmprolijado = undefined -- TODO: COMPLETAR

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura = undefined -- TODO: COMPLETAR

-- 10: subCircuitoMásResistente

subCircuitoMásResistente = undefined -- TODO: COMPLETAR

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

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

-- TODO: COMPLETAR

--}