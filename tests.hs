
import TP1
import Test.HUnit
import TP1 (esCircuitoProlijo)

-- TESTS

--constantes
circuitoEjemplo :: Circuito
circuitoEjemplo =
  Serie
    ( Paralelo
        on
        (Paralelo off cajaNada cajaOn on)
        (Paralelo Nada cajaOn cajaOff Nada)
        on
    )
    cajaOn



testsInvertido :: Test
testsInvertido = TestList
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn
  , "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff
  , "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada
  , "Invertir Ejemplo del TP"
    ~: invertido circuitoEjemplo
    ~?=     Serie
    cajaOn
    (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada off) on)
  ]


testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList
  [ "En una caja con bombilla encendida hay camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
    , "En una caja sin bombilla no hay camino iluminado"
    ~: hayCaminoIluminado cajaNada
    ~?= False
    , "ejemplo inicial no tiene camino iluminado"
    ~: hayCaminoIluminado circuitoEjemplo
    ~?= False 
    , "ejemplo inicial modificado tiene camino iluminado"
    ~: hayCaminoIluminado   Serie
    cajaOn
    (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada on) on)
    ~?= True 
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
    , "Caja con nada es 0"
    ~: cantidadPrendidas cajaNada
    ~?= 0,
     "Cantidad prendida en ejemplo es 6"
    ~: cantidadPrendidas circuitoEjemplo
    ~?= 6
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on],
    "La lista de cajas de una serie es correcta"
    ~: cajasDeCircuito Serie cajaOff cajaNada
    ~?= [off, Nada]
    ,"La lista de cajas de un paralelo es correcto"
    ~: cajasDeCircuito Paralelo cajaOff cajaNada cajaOn cajaNada
    ~?= [off, Nada, on, Nada]
    ,"La lista de cajas de circuito ejemplo es correcta"
    ~: cajasDeCircuito circuitoEjemplo
    ~?= [on, off, Nada, on, on, Nada, on, off, nada, on, on] 
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
    ,"Circuito basico es prolijo"
    ~: esCircuitoProlijo Serie cajaOff cajaNada
    ~?= True
    ,"Circuito prolijo básico es prolijo"
    ~: esCircuitoProlijo Serie (Serie cajaOn cajaOff) cajaOn
    ~?= True
    ,"Circuito desprolijo básico es desprolijo"
    ~:Serie cajaOn (Serie cajaOff cajaOn)
    ~?=False
    , "Caja Prolija 3 Series"
    ~: esCircuitoProlijo Serie (Serie (Serie cajaOn cajaOff) cajaOn) cajaOff
    ~?=True
    , "Caja No Prolijo 4 Series"
    ~: esCircuitoProlijo Serie (Serie (Serie cajaOn cajaOff) (Serie cajaNada cajaNada)) cajaOff
    ~?=False
    ,"Paralelo simple es prolijo"
    ~: esCircuitoProlijo Paralelo cajaNada cajaOn
    ~?=True
    ,"Paralelo con serie prolija es prolijo"
    ~: es CircuitoProlijo Paralelo cajaNada Serie (Serie cajaOn cajaOff) cajaOn
    ~?=True
    ,"Paralelo con serie desprolija es deprolijo"
    ~: es CircuitoProlijo Paralelo cajaNada Serie cajaOn (Serie cajaOn cajaOff)
    ~?=False
  ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
  -- "show = showDeCircuitoConEstructura".
  -- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado = TestList -- TODO: AGREGAR
  [ "La versión emprolijada de una caja es la misma caja"
    ~: circuitoEmprolijado cajaOn
    ~?= cajaOn
  ]


testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList -- TODO: AGREGAR
  [
    
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList -- TODO: AGREGAR
  [
    
  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  , TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  , TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  , TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  , TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  , TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print