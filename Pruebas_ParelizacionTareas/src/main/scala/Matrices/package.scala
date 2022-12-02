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
      val multsLeft = for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        multMatrizRec(subMatriz(m1,l*i/2,0,l/2), subMatriz(m2,0,l*j/2,l/2))
      val multsRight = for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        multMatrizRec(subMatriz(m1,l*i/2,l/2,l/2), subMatriz(m2,l/2,l*j/2,l/2))
      val subMTotales = for (i <- (0 to 3).toVector) yield sumMatriz(multsLeft(i), multsRight(i))

      unirMatriz(subMTotales(0), subMTotales(1)) ++ unirMatriz(subMTotales(2), subMTotales(3))

    }
  }

  /*
    1.2.4. Multiplicando matrices recursivamente, version paralela
  */
  def multMatrizRecPar(m1:Matriz, m2:Matriz):Matriz = {
    def unirMatriz(m1: Matriz, m2: Matriz): Matriz = {
      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }

    val l = m1.length
    if (l == 1) Vector(Vector(m1(0)(0) * m2(0)(0)))
    else {
      val multsLeft = task(for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        multMatrizRecPar(subMatriz(m1, l * i / 2, 0, l / 2), subMatriz(m2, 0, l * j / 2, l / 2)))
      val multsRight = task(for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        multMatrizRecPar(subMatriz(m1, l * i / 2, l / 2, l / 2), subMatriz(m2, l / 2, l * j / 2, l / 2)))

      val multsL = multsLeft.join()
      val multsR = multsRight.join()

      val subMTotales = for (i <- (0 to 3).toVector) yield sumMatriz(multsL(i), multsR(i))

      unirMatriz(subMTotales(0), subMTotales(1)) ++ unirMatriz(subMTotales(2), subMTotales(3))

    }
  }

  /*
    1.3.1. Restando matrices
  */
  def restaMatriz(m1:Matriz, m2:Matriz):Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) - m2(i)(j))
  }

  /*
    1.3.2. Algoritmo de Strassen, version secuencial
  */
  def multStrassen(m1:Matriz, m2:Matriz):Matriz = { //m1 = A, m2, = B
    def unirMatriz(m1: Matriz, m2: Matriz): Matriz = {
      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }
    val l = m1.length
    if (l < 2) {
      Vector(Vector( m1(0)(0)*m2(0)(0) ))
    }
    else {
      val subMatricesM1 = for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        subMatriz(m1, l * i / 2, l * j / 2, l / 2) // [A11, A12, A21, A22]
      val subMatricesM2 = for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        subMatriz(m2, l * i / 2, l * j / 2, l / 2) // [B11, B12, B21, B22]

      val s1 = restaMatriz(subMatricesM2(1), subMatricesM2(3)) //B12 - B22
      val s2 = sumMatriz(subMatricesM1(0), subMatricesM1(1)) //A11 + A12
      val s3 = sumMatriz(subMatricesM1(2), subMatricesM1(3)) //A21 + A22
      val s4 = restaMatriz(subMatricesM2(2), subMatricesM2(0)) //B21 - B11
      val s5 = sumMatriz(subMatricesM1(0), subMatricesM1(3)) //A11 + A22
      val s6 = sumMatriz(subMatricesM2(0), subMatricesM2(3)) //B11 + B22
      val s7 = restaMatriz(subMatricesM1(1), subMatricesM1(3)) //A12 - A22
      val s8 = sumMatriz(subMatricesM2(2), subMatricesM2(3)) //B21 + B22
      val s9 = restaMatriz(subMatricesM1(0), subMatricesM1(2)) //A11 - A21
      val s10 = sumMatriz(subMatricesM2(0), subMatricesM2(1)) //B11 + B12

      val p1 = multStrassen( subMatricesM1(0), s1 ) //A11*S1
      val p2 = multStrassen( s2, subMatricesM2(3) ) //s2*B22
      val p3 = multStrassen( s3, subMatricesM2(0) ) //s3*B11
      val p4 = multStrassen( subMatricesM1(3), s4 ) //A22*S4
      val p5 = multStrassen( s5, s6 ) //s5*s6
      val p6 = multStrassen( s7, s8 ) //s7*S8
      val p7 = multStrassen( s9, s10 ) //s9*s10

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      unirMatriz(c11, c12) ++ unirMatriz(c21, c22)
    }

  }

  /*
    1.3.3. Algoritmo de Strassen, version paralela
  */
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    def unirMatriz(m1: Matriz, m2: Matriz): Matriz = {
      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }

    val l = m1.length
    if (l < 2) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
    else {
      val subMatricesM1 = task(for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        subMatriz(m1, l * i / 2, l * j / 2, l / 2)) // [A11, A12, A21, A22]
      val subMatricesM2 = task(for (i <- (0 to 1).toVector; j <- (0 to 1).toVector) yield
        subMatriz(m2, l * i / 2, l * j / 2, l / 2)) // [B11, B12, B21, B22]

      val subMatrices1 = subMatricesM1.join()
      val subMatrices2 = subMatricesM2.join()

      val s1 = restaMatriz(subMatrices2(1), subMatrices2(3)) //B12 - B22
      val s2 = sumMatriz(subMatrices1(0), subMatrices1(1)) //A11 + A12
      val s3 = sumMatriz(subMatrices1(2), subMatrices1(3)) //A21 + A22
      val s4 = restaMatriz(subMatrices2(2), subMatrices2(0)) //B21 - B11
      val s5 = sumMatriz(subMatrices1(0), subMatrices1(3)) //A11 + A22
      val s6 = sumMatriz(subMatrices2(0), subMatrices2(3)) //B11 + B22
      val s7 = restaMatriz(subMatrices1(1), subMatrices1(3)) //A12 - A22
      val s8 = sumMatriz(subMatrices2(2), subMatrices2(3)) //B21 + B22
      val s9 = restaMatriz(subMatrices1(0), subMatrices1(2)) //A11 - A21
      val s10 = sumMatriz(subMatrices2(0), subMatrices2(1)) //B11 + B12

      val p1 = task(multStrassenPar(subMatrices1(0), s1)) //A11*S1
      val p2 = task(multStrassenPar(s2, subMatrices2(3))) //s2*B22
      val p3 = task(multStrassenPar(s3, subMatrices2(0))) //s3*B11
      val p4 = task(multStrassenPar(subMatrices1(3), s4)) //A22*S4
      val p5 = task(multStrassenPar(s5, s6)) //s5*s6
      val p6 = task(multStrassenPar(s7, s8)) //s7*S8
      val p7 = task(multStrassenPar(s9, s10)) //s9*s10

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()), p6.join())
      val c12 = sumMatriz(p1.join(), p2.join())
      val c21 = sumMatriz(p3.join(), p4.join())
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()), p3.join()), p7.join())

      unirMatriz(c11, c12) ++ unirMatriz(c21, c22)
    }
  }
}
