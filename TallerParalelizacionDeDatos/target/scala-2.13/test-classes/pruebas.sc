import kmedianas._
import Benchmark._
import scala.collection.parallel.CollectionConverters._
//import kmedianas2D._
/*
//puntos secuanciales
val puntosSeq = generarPuntosSeq(32, 100)
val medianasSeq = inicializarMedianasSeq(2, puntosSeq)

//puntos paralelos
val puntosPar = generarPuntosPar(32, 100)
val medianasPar = inicializarMedianasPar(2, puntosPar)

//Funcion clasificar
val clasifSeq = clasificarSeq(puntosSeq, medianasSeq)
val clasifPar = clasificarPar(puntosPar, medianasPar)

//Funcion actualizar
val medianasNuevasSeq = actualizarSeq(clasifSeq, medianasSeq)
val medianasNuevasPar = actualizarPar(clasifPar, medianasPar)

//Funcion hayConvergencia
hayConvergenciaSeq(0.01, medianasSeq, medianasNuevasSeq)
hayConvergenciaPar(0.01, medianasPar, medianasNuevasPar)

//Funcion kMedianasSeq
kMedianasSeq(puntosSeq, medianasSeq, 0.01)
kMedianasPar(puntosPar, medianasPar, 0.01)
*/

val k = 8
val n = 1000
val eta = 0.01

val puntosSeq = generarPuntosSeq(k, n)
val medianasSeq = inicializarMedianasSeq(k, puntosSeq)

val puntosPar = puntosSeq.par
val medianasPar = medianasSeq.par
/*
val timePar = probarAlgoritmoPar(puntosPar, medianasPar, eta)
val timeSeq = probarAlgoritmoSeq(puntosSeq, medianasSeq, eta)
//timeSeq/timePar
*/


probarAlgoritmoSeq(puntosSeq, medianasSeq, eta)
probarAlgoritmoPar(puntosPar, medianasPar, eta)