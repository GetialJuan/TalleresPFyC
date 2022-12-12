import kmedianas._
import Benchmark._
import scala.collection.parallel.CollectionConverters._

//puntos secuenciales (se utilizara .par en el caso paralelo)
val puntosSeq1 = generarPuntosSeq(2, 100)
val puntosSeq2 = generarPuntosSeq(4, 100)
val puntosSeq3 = generarPuntosSeq(8, 1000)
val puntosSeq4 = generarPuntosSeq(16, 1000)
val puntosSeq5 = generarPuntosSeq(32, 1000)

//medianas secuenciales (se utilizara .par en el caso paralelo)
val medianasSeq1 = inicializarMedianasSeq(2, puntosSeq1)
val medianasSeq2 = inicializarMedianasSeq(4, puntosSeq2)
val medianasSeq3 = inicializarMedianasSeq(8, puntosSeq3)
val medianasSeq4 = inicializarMedianasSeq(16, puntosSeq4)
val medianasSeq5 = inicializarMedianasSeq(32, puntosSeq5)

//1.1. Funcion clasificar
//secuencial
val clasifSeq1 = clasificarSeq(puntosSeq1, medianasSeq1)
val clasifSeq2 = clasificarSeq(puntosSeq2, medianasSeq2)
val clasifSeq3 = clasificarSeq(puntosSeq3, medianasSeq3)
val clasifSeq4 = clasificarSeq(puntosSeq4, medianasSeq4)
val clasifSeq5 = clasificarSeq(puntosSeq5, medianasSeq5)
//paralelo
val clasifPar1 = clasificarPar(puntosSeq1.par, medianasSeq1.par)
val clasifPar2 = clasificarPar(puntosSeq2.par, medianasSeq2.par)
val clasifPar3 = clasificarPar(puntosSeq3.par, medianasSeq3.par)
val clasifPar4 = clasificarPar(puntosSeq4.par, medianasSeq4.par)
val clasifPar5 = clasificarPar(puntosSeq5.par, medianasSeq5.par)

//1.2. Funcion actualizar
//secuencial
val medianasNuevasSeq1 = actualizarSeq(clasifSeq1, medianasSeq1)
val medianasNuevasSeq2 = actualizarSeq(clasifSeq2, medianasSeq2)
val medianasNuevasSeq3 = actualizarSeq(clasifSeq3, medianasSeq3)
val medianasNuevasSeq4 = actualizarSeq(clasifSeq4, medianasSeq4)
val medianasNuevasSeq5 = actualizarSeq(clasifSeq5, medianasSeq5)
//paralelo
val medianasNuevasPar1 = actualizarPar(clasifPar1, medianasSeq1.par)
val medianasNuevasPar2 = actualizarPar(clasifPar2, medianasSeq2.par)
val medianasNuevasPar3 = actualizarPar(clasifPar3, medianasSeq3.par)
val medianasNuevasPar4 = actualizarPar(clasifPar4, medianasSeq4.par)
val medianasNuevasPar5 = actualizarPar(clasifPar5, medianasSeq5.par)

//1.3. Funcion hayConvergencia
//secuencial
hayConvergenciaSeq(0.01, medianasSeq1, medianasNuevasSeq1)
hayConvergenciaSeq(0.001, medianasSeq2, medianasNuevasSeq2)
hayConvergenciaSeq(10, medianasSeq3, medianasNuevasSeq3)
hayConvergenciaSeq(10, medianasSeq4, medianasNuevasSeq4)
hayConvergenciaSeq(100, medianasSeq5, medianasNuevasSeq5)
//paralelo
hayConvergenciaPar(0.01, medianasSeq1.par, medianasNuevasPar1)
hayConvergenciaPar(0.001, medianasSeq2.par, medianasNuevasPar2)
hayConvergenciaPar(10, medianasSeq3.par, medianasNuevasPar3)
hayConvergenciaPar(10, medianasSeq4.par, medianasNuevasPar4)
hayConvergenciaPar(100, medianasSeq5.par, medianasNuevasPar5)

//1.4. Funcion kMedianas
//secuencial
kMedianasSeq(puntosSeq1, medianasSeq1, 0.01)
kMedianasSeq(puntosSeq2, medianasSeq2, 0.01)
kMedianasSeq(puntosSeq3, medianasSeq3, 0.001)
kMedianasSeq(puntosSeq4, medianasSeq4, 0.001)
kMedianasSeq(puntosSeq5, medianasSeq5, 0.001)
//paralelo
kMedianasPar(puntosSeq1.par, medianasSeq1.par, 0.01)
kMedianasPar(puntosSeq2.par, medianasSeq2.par, 0.01)
kMedianasPar(puntosSeq3.par, medianasSeq3.par, 0.001)
kMedianasPar(puntosSeq4.par, medianasSeq4.par, 0.001)
kMedianasPar(puntosSeq5.par, medianasSeq5.par, 0.001)

//##EVALUACION COMPARATIVA##//
/*
def compararAlgoritmos(k:Int, n:Int, eta:Double)={
  val puntosSeq = generarPuntosSeq(k, n)
  val medianasSeq = inicializarMedianasSeq(k, puntosSeq)
  val puntosPar = puntosSeq.par
  val medianasPar = medianasSeq.par
  (
    (k,n,eta),
    probarAlgoritmoSeq(puntosSeq,medianasSeq,eta),
    probarAlgoritmoPar(puntosPar,medianasPar,eta)
  )
}

for (n <- Vector(100,1000,100000,1000000)) yield compararAlgoritmos(4, n, 0.001)

for (n <- Vector(100,1000,100000,1000000)) yield compararAlgoritmos(8, n, 0.001)

for (n <- Vector(100,1000,100000,1000000)) yield compararAlgoritmos(16, n, 0.001)

for (n <- Vector(100,1000,100000,1000000)) yield compararAlgoritmos(32, n, 0.001)
*/
/*
for (n <- (100,1000,100000,1000000); k <- (2,4,8,16,32,64)) yield
  compararAlgoritmos(k, n, 00.1)
 */