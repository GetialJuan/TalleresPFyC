import scala.util.Random
import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.util.Random

package object kmedianas {
  //## Definiciones ##//
  //Punto
  class Punto(val x:Double, val y: Double, val z: Double){
    private def cuadrado(v:Double): Double = v*v

    def distanciaAlCuadrado(that:Punto): Double =
      cuadrado(that.x - x) + cuadrado(that.y - y) + cuadrado(that.z - z)

    private def round(v:Double):Double = (v*100).toInt / 100.0

    override def toString = s"(_{round(x)},__{round(y)},__{round(z)})"
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

  //## Ejercicios ##//

  /*
    1.1. Clasificando los puntos
   */
  //Version secuencial
  /*def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    puntos.groupBy(p => p.)
  }*/



}
