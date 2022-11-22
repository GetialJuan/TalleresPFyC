import common._
import scala.util.Random
package object Matrices {
  //Random
  val random = new Random()

  //Matriz
  type Matriz = Vector[Vector[Int]]

  //Matriz aleatoria
  def matrizAlAzar(long:Int, vals:Int): Matriz = {
    //Crea una matriz de enteros cuadrada de long x long,
    //con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long){random.nextInt(vals)}
    v
  }

  //Producto punto entre 2 vectores
  def prodEscalar(v1:Vector[Int], v2:Vector[Int]):Int = {
    (v1 zip v2).map({case (i, j) => (i*j)}).sum
  }

  //Transpuesta de una matriz
  def transpuesta(m:Matriz):Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  ///////////////////Ejercicios//////////////////////

  /*
  1.1.1. Version estandar secuencial
   */
  def multMatriz(m1:Matriz, m2:Matriz):Matriz = {
    val l = m1.length
    val m2T = transpuesta(m2)
    Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2T(j)))
  }

  /*
    1.1.2. Version estandar paralela
  */
  def multMatrizPar(m1:Matriz, m2:Matriz):Matriz = {
    val l1 = m1.length
    val l2 = m2.length
    val m2T = transpuesta(m2)

    if (l1>=4){
      val mid = l1/2
      val (m1R, m1L) = m1 splitAt(mid)
      val (r1, r2) = parallel(multMatrizPar(m1R, m2), multMatrizPar(m1L, m2))
      r1 ++ r2
    }
    else {
      Vector.tabulate(l1, l2)((i, j) => prodEscalar(m1(i), m2T(j)))
    }
  }

  /*
      1.2.1. Extrayendo submatrices
  */
  def subMatriz(m:Matriz, i:Int, j:Int, l:Int):Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i+x)(y+j))
  }

  /*
    1.2.2. Sumando matrices
  */
  def sumMatriz(m1:Matriz, m2:Matriz):Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) + m2(i)(j))
  }

  /*
    1.2.3. Multiplicando matrices recursivamente, version secuencial
  */
  def multMatrizRec(m1:Matriz, m2:Matriz):Matriz = {
    //type Matriz = Vector[Vector[Int]]
    def unirMatriz(m1:Matriz, m2:Matriz):Matriz = {
      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }

    val l = m1.length
    if(l==1) Vector(Vector( m1(0)(0)*m2(0)(0) ))
    else {
      val multsLeft = for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield multMatrizRec(subMatriz(m1,l*i/2,0,l/2), subMatriz(m2,0,l*j/2,l/2))
      val multsRight = for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield multMatrizRec(subMatriz(m1,l*i/2,l/2,l/2), subMatriz(m2,l/2,l*j/2,l/2))
      val subMTotales = for (i <- (0 to 3).toVector) yield sumMatriz(multsLeft(i), multsRight(i))

      unirMatriz(subMTotales(0), subMTotales(1)) ++ unirMatriz(subMTotales(2), subMTotales(3))

    }
  }

  /*
    1.2.4. Multiplicando matrices recursivamente, version paralela
  */
  def multMatrizRecPar(m1:Matriz, m2:Matriz):Matriz = {
    //Por terminar....
    Vector(Vector(1))
  }

  /*
    1.3.1. Restando matrices
  */
  def restaMatriz(m1:Matriz, m2:Matriz):Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) - m2(i)(j))
  }
}
