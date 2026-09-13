import TP1
import Test.HUnit
import TP1 (resistenciaCircuito, on, subCircuitoMásResistente)

-- TESTS

-- constantes
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
testsInvertido =
  TestList
    [ "Caja invertida (1)"
        ~: invertido cajaOn
        ~?= cajaOn,
      "Caja invertida (2)"
        ~: invertido cajaOff
        ~?= cajaOff,
      "Caja invertida (3)"
        ~: invertido cajaNada
        ~?= cajaNada,
      "Invertir Ejemplo del TP"
        ~: invertido circuitoEjemplo
        ~?= Serie
          cajaOn
          (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada off) on)
    ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado =
  TestList
    [ "En una caja con bombilla encendida hay camino iluminado"
        ~: hayCaminoIluminado cajaOn
        ~?= True,
      "En una caja sin bombilla no hay camino iluminado"
        ~: hayCaminoIluminado cajaNada
        ~?= False,
      "ejemplo inicial no tiene camino iluminado"
        ~: hayCaminoIluminado circuitoEjemplo
        ~?= False,
      "ejemplo inicial modificado tiene camino iluminado"
        ~: hayCaminoIluminado (Serie cajaOn (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada on) on))
        ~?= True
    ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas =
  TestList
    [ "Cantidad prendidas en caja prendida es 1"
        ~: cantidadPrendidas cajaOn
        ~?= 1,
      "Caja con nada es 0"
        ~: cantidadPrendidas cajaNada
        ~?= 0,
      "Cantidad prendida en ejemplo es 6"
        ~: cantidadPrendidas circuitoEjemplo
        ~?= 6
    ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito =
  TestList
    [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
        ~: cajasDeCircuito cajaOn
        ~?= [on],
      "La lista de cajas de una serie es correcta"
        ~: cajasDeCircuito (Serie cajaOff cajaNada)
        ~?= [off, Nada],
      "La lista de cajas de un paralelo es correcto"
        ~: cajasDeCircuito (Paralelo off cajaNada cajaOn Nada)
        ~?= [off, Nada, on, Nada],
      "La lista de cajas de circuito ejemplo es correcta"
        ~: cajasDeCircuito circuitoEjemplo
        ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on]
    ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo =
  TestList
    [ "Una caja es prolija"
        ~: esCircuitoProlijo cajaOn
        ~?= True,
      "Circuito basico es prolijo"
        ~: esCircuitoProlijo (Serie cajaOff cajaNada)
        ~?= True,
      "Circuito prolijo básico es prolijo"
        ~: esCircuitoProlijo (Serie (Serie cajaOn cajaOff) cajaOn)
        ~?= True,
      "Circuito desprolijo básico es desprolijo"
        ~: esCircuitoProlijo (Serie cajaOn (Serie cajaOff cajaOn))
        ~?= False,
      "Caja Prolija 3 Series"
        ~: esCircuitoProlijo (Serie (Serie (Serie cajaOn cajaOff) cajaOn) cajaOff)
        ~?= True,
      "Caja No Prolijo 4 Series"
        ~: esCircuitoProlijo (Serie (Serie (Serie cajaOn cajaOff) (Serie cajaNada cajaNada)) cajaOff)
        ~?= False,
      "Paralelo simple es prolijo"
        ~: esCircuitoProlijo (Paralelo off cajaNada cajaOn Nada)
        ~?= True,
      "Paralelo con serie prolija es prolijo"
        ~: esCircuitoProlijo (Paralelo Nada (Serie (Serie cajaOn cajaOff) cajaOn) cajaOn Nada)
        ~?= True,
      "Paralelo con serie desprolija es deprolijo"
        ~: esCircuitoProlijo (Paralelo Nada (Serie cajaOn (Serie cajaOn cajaOff)) cajaNada on)
        ~?= False
    ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
-- "show = showDeCircuitoConEstructura".
-- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado =
  TestList -- TODO: AGREGAR
    [ "La versión emprolijada de una caja es la misma caja"
        ~: circuitoEmprolijado cajaOn
        ~?= cajaOn
    ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura =
  TestList -- TODO: AGREGAR
    [ "rompiendo 1"
        ~: tienenLaMismaEstructura (Serie cajaOn (Serie (Paralelo Nada cajaNada cajaNada Nada) (Paralelo Nada cajaNada cajaNada Nada))) (Serie cajaOn (Serie (Paralelo Nada cajaNada cajaNada Nada) (Paralelo Nada cajaNada cajaNada Nada)))
        ~?= True,
      "Mepa que rompe: 4 series es un paralelo"
        ~: tienenLaMismaEstructura
          (Serie (Serie (Serie cajaOn cajaOn) cajaOn) cajaOn)
          (Paralelo on cajaOn cajaOn on)
        ~?= False,
      "Misma estructura paralelo invertido"
        ~: tienenLaMismaEstructura (Paralelo Nada cajaNada (Serie cajaNada cajaNada) Nada) (invertido (Paralelo Nada (Serie cajaNada cajaNada) cajaNada Nada))
        ~?= True
    ]

testResistenciaCustomFunciona :: Test
testResistenciaCustomFunciona = 
  TestList
  [ "Caja tiene el valor correcto"
    ~:  [resistenciaCircuito cajaOn, resistenciaCircuito cajaOff, resistenciaCircuito cajaNada]
    ~?= [1.0, 2.0, 10.0]
  ,"Serie basica tiene la resistencia esperada"
    ~: resistenciaCircuito (Serie cajaOn cajaNada)
    ~?= 19.6
  ,"Serie basica puede tener resistencia negativa"
    ~: resistenciaCircuito (Serie cajaNada cajaOn)
    ~?= -2.0
  ,"Paralelo prendido da cero"
    ~: resistenciaCircuito (Paralelo on cajaOn cajaOn on)
    ~?= 0.0
  ,"Paralelo random da el valor correcto"
    ~: resistenciaCircuito (Paralelo Nada cajaOn cajaOff on)
    ~?= 9.5
  ,"Paralelo random espejado da valor distinto"
    ~: resistenciaCircuito (Paralelo on cajaOff cajaOn Nada)
    ~?= -25.5
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente =
  TestList -- TODO: AGREGAR
    [ "Caja se devuelve a sí misma"
        ~: subCircuitoMásResistente cajaOn
        ~?= cajaOn,
      "Serie donde gana el primero"
        ~: subCircuitoMásResistente (Serie cajaNada cajaOn)
        ~?= cajaNada,
      "Serie donde gana el segundo"
        ~: subCircuitoMásResistente (Serie cajaOff cajaOn)
        ~?= cajaOff,
      "Serie donde gana la serie"
        ~: subCircuitoMásResistente (Serie cajaOff cajaNada)
        ~?= Serie cajaOff cajaNada
      ,"Paralelo donde gana el paralelo"
        ~: subCircuitoMásResistente (Paralelo Nada cajaNada cajaNada on)
        ~?= Paralelo Nada cajaNada cajaNada on
      ,"Paralelo donde gana la izquierda"
        ~: subCircuitoMásResistente (Paralelo on cajaNada cajaOff Nada)
        ~?= cajaNada
      ,"Paralelo donde gana la derecha"
        ~: subCircuitoMásResistente (Paralelo on cajaOff cajaNada Nada)
        ~?= cajaNada
      ,"Ejemplo de inicio funciona correctamente"
        ~: subCircuitoMásResistente circuitoEjemplo --me tomo 30 minutos y lo calcule mal (?)
        ~?= Paralelo off cajaNada cajaOn on
    ]

tests :: Test
tests =
  TestList
    [ TestLabel "invertido" testsInvertido,
      TestLabel "hayCaminoIluminado" testsHayCaminoIluminado,
      TestLabel "cantidadPrendidas" testsCantidadPrendidas,
      TestLabel "cajasDeCircuito" testsCajasDeCircuito,
      TestLabel "esCircuitoProlijo" testsEsCircuitoProlijo,
      -- TestLabel "circuitoEmprolijado" testsCircuitoEmprolijado,
      TestLabel "ResistenciaCustomFunciona" testResistenciaCustomFunciona,
      TestLabel "tienenLaMismaEstructura" testsTienenLaMismaEstructura,
      TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
    ]

main :: IO ()
main = runTestTT tests >>= print