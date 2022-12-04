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

  //## Ejercicios ##//

  /*
    1.1. Clasificando los puntos
   */
  //Version secuencial




}
