import kmedianas2D._
import org.scalameter._
import common._
import scala.collection.parallel.{ParSeq,ParMap}
import plotly.element._, plotly.layout._, plotly._
import scala.collection.parallel.CollectionConverters._


def tiempoDe[T](   body : => T) = {
  val timeA1 = config(
    KeyValue(Key.exec.minWarmupRuns -> 20),
    KeyValue(Key.exec.maxWarmupRuns -> 60),
    KeyValue(Key.verbose -> false)
  ) withWarmer(new Warmer.Default) measure (body)
  timeA1
}

val k = 8
val numPuntos = 1000
val eta = 0.01

//def probarKmedianas (numPuntos : Int , eta : Double , k : Int ) = {
// Probarlo secuencial

val puntosSeq = generarPuntosSeq(k, numPuntos)
val medianasSeq = inicializarMedianasSeq(k, puntosSeq)
val medianasSeqfin = kMedianasSeq(puntosSeq, medianasSeq, eta)
val clasifFinalSeq = clasificarSeq(puntosSeq, medianasSeqfin)
val tiempoSeq = tiempoDe(kMedianasSeq(puntosSeq, medianasSeq, eta))

val puntosPar = puntosSeq.par
// val medianasPar = inicializarMedianasPar( k , puntosPar )
val medianasPar = medianasSeq.par
val medianasParfin = kMedianasPar(puntosPar, medianasPar, eta)
val clasifFinalPar = clasificarPar(puntosPar, medianasParfin)
val tiempoPar = tiempoDe(kMedianasPar(puntosPar, medianasPar, eta))



def graficoSeq(puntosSeq: Seq[Punto], medianasSeq: Seq[Punto], medianasSeqfin: Seq[Punto], clasifFinalSeq: Map[Punto,Seq[Punto]]) : Unit = {

  // Hacer grafica de los resultados del proceso secuencial
  val trazosSeq = for {
    (p, pseq) <- clasifFinalSeq
    ejeXseq = for {
      pto <- pseq
    } yield pto.x
    ejeYseq = for {
      pto <- pseq
    } yield pto.y
  } yield Scatter (
    ejeXseq,
    ejeYseq
  ).withMode (ScatterMode (ScatterMode.Markers) ).withName (s"Puntos: " + p.x + " , " + p.y)

  val ejeXMedianasSeq = for {
    p <- medianasSeq
  } yield p.x

  val ejeYMedianasSeq = for {
    p <- medianasSeq
  } yield p.y

  val ejeXMedianasFinSeq = for {
    p <- medianasSeqfin
  } yield p.x

  val ejeYMedianasFinSeq = for {
    p <- medianasSeqfin
  } yield p.y

  val trazo2Seq = Scatter (
    ejeXMedianasSeq,
    ejeYMedianasSeq
  ).withMode (ScatterMode (ScatterMode.Markers) ).withName ("Medianas")

  val trazo3Seq = Scatter (
    ejeXMedianasFinSeq,
    ejeYMedianasFinSeq
  ).withMode (ScatterMode (ScatterMode.Markers) ).withName ("Medianas Finales ")

  val dataSeq = trazo2Seq +: (trazo3Seq +: trazosSeq.toSeq)

  val layoutSeq = Layout ().withTitle ("Plotting de puntos al azar y medianas iniciales y finales - Secuencial ")

  Plotly.plot ("C:\\Users\\Juan\\Documents\\PFyC(Talleres)\\kmedianasSeq.html", dataSeq, layoutSeq)
}

def graficoPar(puntosPar: ParSeq[Punto], medianasPar: ParSeq[Punto], medianasParfin: ParSeq[Punto], clasifFinalPar: ParMap[Punto,ParSeq[Punto]]): Unit = {
  // Probarlo paralelo
  // val puntosPar = generarPuntosPar ( k , numPuntos )
  // Hacer grafica de los resultados del proceso paralelo
  val trazosPar = for {
    (p, ppar) <- clasifFinalPar
    ejeXpar = for {
      pto <- ppar.seq
    } yield pto.x
    ejeYpar = for {
      pto <- ppar.seq
    } yield pto.y
  } yield Scatter(
    ejeXpar.toSeq,
    ejeYpar.toSeq
  ).withMode(ScatterMode(ScatterMode.Markers)).withName(s"Puntos: " + p.x + " , " + p.y)

  val ejeXMedianasPar = for {
    p <- medianasPar
  } yield p.x

  val ejeYMedianasPar = for {
    p <- medianasPar
  } yield p.y

  val ejeXMedianasFinPar = for {
    p <- medianasParfin
  } yield p.x

  val ejeYMedianasFinPar = for {
    p <- medianasParfin
  } yield p.y

  val trazo2Par = Scatter(
    ejeXMedianasPar.seq,
    ejeYMedianasPar.seq
  ).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas")

  val trazo3Par = Scatter(
    ejeXMedianasFinPar.seq,
    ejeYMedianasFinPar.seq
  ).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas Finales")

  val dataPar = (trazo2Par +: (trazo3Par +: trazosPar.toSeq)).seq

  val layoutPar = Layout().withTitle("Plotting de puntos al azar y medianas iniciales y finales - Paralela")
  Plotly.plot("C:\\Users\\Juan\\Documents\\PFyC(Talleres)\\kmedianasPar.html", dataPar.toSeq, layoutPar)
}
val v = for(i <- clasifFinalSeq.values) yield i.toSeq
val vp = for(i <- clasifFinalSeq.values) yield i.par
val k = for(i <- clasifFinalSeq.keys) yield i
val c = (k zip v).toMap
val cp = (k zip vp).toMap.par

(tiempoSeq,tiempoPar,tiempoSeq.value/tiempoPar.value)

val (t1,t2) = parallel(
  graficoSeq(puntosSeq.toSeq,medianasSeq.toSeq,medianasSeqfin.toSeq,c),
  graficoPar(puntosPar.par,medianasPar.par,medianasParfin.par,cp)
)
//}