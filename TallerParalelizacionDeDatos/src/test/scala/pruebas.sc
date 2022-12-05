//import kmedianas._
import kmedianas2D._

/*
var l1= List("anc", "ahg", "tyh")
l1.groupBy(x => x.charAt(0))

var list1= List("amit", "sumit", "vinit", "ajit", "kavit", "lalit", "lalit", "vinit", "vinit")
var g = list1.groupBy(x => x)

val l2 = List(1,2,3)
l2.groupBy(n => l1.length)*/
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

val puntosSeq = generarPuntosSeq(32, 100)
val medianasSeq = inicializarMedianasSeq(2, puntosSeq)

kMedianasSeq(puntosSeq, medianasSeq, 0.01)