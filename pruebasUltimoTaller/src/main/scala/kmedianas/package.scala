import scala.util.Random
import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.util.Random
import scala.math._

package object kmedianas {
  //## Definiciones ##//
  //Punto
  class Punto(val x:Double, val y: Double, val z: Double){
    private def cuadrado(v:Double): Double = v*v

    def distanciaAlCuadrado(that:Punto): Double =
      cuadrado(that.x - x) + cuadrado(that.y - y) + cuadrado(that.z - z)

    private def round(v:Double):Double = (v*100).toInt / 100.0

    override def toString = s"(${round(x)},${round(y)},${round(z)})"
  }

  //hallarPuntoMasCercano
  //Cual mediana esta mas cerca al punto p
  def hallarPuntoMasCercano(p:Punto, medianas:IterableOnce[Punto]):Punto = {
    val it = medianas.iterator
    assert(it.nonEmpty)
    var puntoMasCercano = it.next()
    var minDistancia = p.distanciaAlCuadrado(puntoMasCercano)
    while(it.hasNext){
      val point = it.next()
      val distancia = p.distanciaAlCuadrado(point)
      if(distancia < minDistancia) {
        minDistancia = distancia
        puntoMasCercano = point
      }
    }
    puntoMasCercano
  }

  //generarPuntos

  //Secuanciales
  def generarPuntosSeq(k:Int, num:Int):Seq[Punto] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Punto(x, y, z)
      }).to(mutable.ArrayBuffer)
  }

  //Paralelos
  def generarPuntosPar(k: Int, num: Int): ParSeq[Punto] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Punto(x, y, z)
      }).to(mutable.ArrayBuffer).par
  }

  //inicializarMedianas

  //secuenciales
  def inicializarMedianasSeq(k:Int, puntos: Seq[Punto]): Seq[Punto] = {
    val rand = new Random(7)
    (0 until k).map(_ => puntos(rand.nextInt(puntos.length))).to(mutable.ArrayBuffer)
  }

  //paralelas
  def inicializarMedianasPar(k: Int, puntos: ParSeq[Punto]): ParSeq[Punto] = {
    val rand = new Random(7)
    (0 until k).map(_ => puntos(rand.nextInt(puntos.length))).to(mutable.ArrayBuffer).par
  }

  //calcularPromedio

  //secuencial
  def calculePromedioSeq(medianaVieja:Punto, puntos:Seq[Punto]):Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      var x = 0.0
      var y = 0.0
      var z = 0.0
      puntos.foreach { p =>
        x += p.x
        y += p.y
        z += p.z
      }
      new Punto(x/puntos.length, y/puntos.length, z/puntos.length)
    }
  }

  //paralelo
  def calculePromedioPar(medianaVieja: Punto, puntos: ParSeq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      var x = 0.0
      var y = 0.0
      var z = 0.0
      puntos.foreach { p =>
        x += p.x
        y += p.y
        z += p.z
      }
      new Punto(x / puntos.length, y / puntos.length, z / puntos.length)
    }
  }

  //## Ejercicios ##//

  /*
    1.1. Clasificando los puntos
   */
  //Version secuencial
  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    puntos.groupBy(p => hallarPuntoMasCercano(p, medianas))
  }

  //Version paralela
  def clasificarPar(puntos: ParSeq[Punto], medianas: ParSeq[Punto]): ParMap[Punto, ParSeq[Punto]] = {
    puntos.groupBy(p => hallarPuntoMasCercano(p, medianas))
  }

  /*
    1.2. Actualizando las medianas
   */
  //Version Secuancial
  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    def nuevaMediana(mediana: Punto): Punto = {
      //Se verfica que la mediana no se haya quedado rezagada debido a no tener ningun punto cerca
      if (clasif.contains(mediana)) calculePromedioSeq(mediana, clasif(mediana))
      else mediana
    }

    medianasViejas.map(nuevaMediana)
  }

  //Version Paralela
  def actualizarPar(clasif: ParMap[Punto, ParSeq[Punto]], medianasViejas: ParSeq[Punto]): ParSeq[Punto] = {
    def nuevaMediana(mediana: Punto): Punto = {
      //Se verfica que la mediana no se haya quedado rezagada debido a no tener ningun punto cerca
      if (clasif.contains(mediana)) calculePromedioPar(mediana, clasif(mediana))
      else mediana
    }

    medianasViejas.map(nuevaMediana)
  }

  /*
    1.3. Detectando convergencia
   */
  //Version secuencial
  def hayConvergenciaSeq(eta:Double, medianasViejas:Seq[Punto], medianasNuevas:Seq[Punto]):Boolean = {
    val l = medianasViejas.length
    !(for (i <- 0 until l) yield
      medianasViejas(i).distanciaAlCuadrado(medianasNuevas(i)) < (eta*eta)
      ).contains(false)
  }

  //Version paralela
  def hayConvergenciaPar(eta: Double, medianasViejas: ParSeq[Punto], medianasNuevas: ParSeq[Punto]): Boolean = {
    val l = medianasViejas.length
    !(for (i <- 0 until l) yield
      medianasViejas(i).distanciaAlCuadrado(medianasNuevas(i)) < (eta * eta)
      ).contains(false)
  }

  /*
    1.4. Implementando el algoritmo kmeans
   */
  //Version secuancial
  @tailrec
  final def kMedianasSeq(puntos:Seq[Punto], medianas:Seq[Punto], eta:Double):Seq[Punto] = {
    //CLasificacion
    val clasif = clasificarSeq(puntos, medianas)
    //Actualizacion
    val medianasNuevas = actualizarSeq(clasif, medianas)

    //Convergencia
    if(hayConvergenciaSeq(eta, medianas, medianasNuevas)) medianasNuevas
    else kMedianasSeq(puntos, medianasNuevas, eta)
  }

  //Version paralela
  @tailrec
  final def kMedianasPar(puntos: ParSeq[Punto], medianas: ParSeq[Punto], eta: Double): ParSeq[Punto] = {
    //CLasificacion
    val clasif = clasificarPar(puntos, medianas)
    //Actualizacion
    val medianasNuevas = actualizarPar(clasif, medianas)

    //Convergencia
    if (hayConvergenciaPar(eta, medianas, medianasNuevas)) medianasNuevas
    else kMedianasPar(puntos, medianasNuevas, eta)
  }



}
