import scala.util.Random
import scala.math._
import common._

package object Matrices {

  type Matriz = Vector[Vector [Int]]

  /** matrizAlAzar: Crea una matriz (Vector de Vectores) de un tamaño, con valores aleatorios
   *
   * Recibe: Dos enteros, long y vals, correspondientes al tamaño, y al valor aleatorio máximo
   *
   * Retorna : Un vector de vectores, correspondiente a la matriz en cuestión
   *
   * Ejemplo: matrizAlAzar(4,2) -> ( (1, 0, 1, 1), (0, 0, 0, 1), (0, 0, 0, 0), (1, 1, 0, 1) )
   */
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long, long) { Random.nextInt(vals) }
    v
  }

  /** prodEscalar: Realiza el producto punto entre dos vectores
   *
   * Recibe: Los dos vectores a operar
   *
   * Retorna : El entero resultante de la operación
   *
   * Ejemplo: prodEscalar((1,1), (0,0)) -> 0
   */
  def prodEscalar(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case(i, j) => i * j }).sum
  }

  /** prodEscalar: Halla la transpuesta de una matriz
   *
   * Recibe: La matriz a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: transpuesta( ((1,1), (0,0)) ) -> ((1,0), (1,0))
   */
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  /** multMatriz: Calcula la multiplicación entre dos matrices
   *
   * Recibe: La dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: multMatriz( ((0,0), (1,1)), ((0,0), (1,1)) ) -> ((0,0), (1,1))
   */
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val m2T = transpuesta(m2)
    Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2T(j)))
  }

  /** multMatrizPar: Calcula la multiplicación entre dos matrices paralelamente, haciendo el cálculo por mitades
   *
   * Recibe: La dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: multMatrizPar( ((0,0), (1,1)), ((0,0), (1,1)) ) -> ((0,0), (1,1))
   */
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length / 2
    if(l < 1) {
      multMatriz(m1, m2)
    } else {
      val m2T = transpuesta(m2)
      val (izquierda, derecha) = m1.splitAt(l)

      val (prodIzq, prodDer) = parallel(
        Vector.tabulate(l, l * 2)((i, j) => prodEscalar(izquierda(i), m2T(j))),
        Vector.tabulate(l, l * 2)((i, j) => prodEscalar(derecha(i), m2T(j)))
      )

      prodIzq ++ prodDer
    }
  }

  /** subMatriz: Divide una matriz cuadrada en una submatriz comprendia entre los límites especificados
   *
   * Recibe: La matriz a dividir, la posición (i,j) de comienzo, y el tamaño esperado
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: subMatriz( ((1, 0, 1, 1), (0, 0, 0, 1), (0, 0, 0, 0), (1, 1, 0, 1)), 0, 0, 2 ) -> ((1,0), (0,0))
   */
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i+x)(j+y))
  }

  /** sumMatriz: Calcula la suma entre dos matrices
   *
   * Recibe: Las dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: sumMatriz( ((1, 0), (0, 0)), ((0, 0), (1, 1)) ) -> ((1,0), (1,1))
   */
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) + m2(i)(j))
  }

  /** multMatrizRec: Calcula la multiplicación entre dos matrices, dividiendo cada matriz en otras más pequeñas
   *
   * Recibe: La dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: multMatrizRec( ((0,0), (1,1)), ((0,0), (1,1)) ) -> ((0,0), (1,1))
   */
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length / 2

    if(l <= 1){
      multMatriz(m1, m2)
    } else {

      def multMatrizAux(mA: Vector[Matriz], mB: Vector[Matriz]): Matriz = {
        sumMatriz(multMatrizRec(mA(0), mB(0)), multMatrizRec(mA(1), mB(1)))
      }

      val mm1 = Vector.tabulate(2, 2)((i, j) => subMatriz(m1, i * l, j * l, l))
      val mm2 = Vector.tabulate(2, 2)((i, j) => subMatriz(m2, j * l, i * l, l))

      val mm3 = Vector.tabulate(1, 2)((_, j) => multMatrizAux(mm1(0), mm2(j)))
      val mm4 = Vector.tabulate(1, 2)((_, j) => multMatrizAux(mm1(1), mm2(j)))

      val mm5 = mm3(0)(0) ++ mm4(0)(0) zip mm3(0)(1) ++ mm4(0)(1)
      (for (n <- mm5.indices) yield mm5(n)._1 ++ mm5(n)._2).toVector
    }
  }

  /** multMatrizRecPar: Calcula la multiplicación entre dos matrices, dividiendo el problema paralelamente
   *
   * Recibe: La dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: multMatrizRecPar( ((0,0), (1,1)), ((0,0), (1,1)) ) -> ((0,0), (1,1))
   */
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length / 2

    if (l <= 1) {
      multMatriz(m1, m2)
    } else if(l < 32) {
      multMatrizRec(m1, m2)
    } else {

      def multMatrizAux(mA: Vector[Matriz], mB: Vector[Matriz]): Matriz = {
        val (izquierda, derecha) = parallel (
          multMatrizRecPar(mA(0), mB(0)),
          multMatrizRecPar(mA(1), mB(1))
        )
        sumMatriz(izquierda, derecha)
      }

      val mm1 = Vector.tabulate(2, 2)((i, j) => subMatriz(m1, i * l, j * l, l))
      val mm2 = Vector.tabulate(2, 2)((i, j) => subMatriz(m2, j * l, i * l, l))

      val mm3 = Vector.tabulate(1, 2)((_, j) => multMatrizAux(mm1(0), mm2(j)))
      val mm4 = Vector.tabulate(1, 2)((_, j) => multMatrizAux(mm1(1), mm2(j)))

      val (izq, der) = parallel (
        mm3(0)(0) ++ mm4(0)(0),
        mm3(0)(1) ++ mm4(0)(1)
      )

      val mm5 = izq zip der
      (for (n <- mm5.indices) yield mm5(n)._1 ++ mm5(n)._2).toVector

    }
  }

  /** restaMatriz: Calcula la resta entre dos matrices
   *
   * Recibe: Las dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: restaMatriz( ((1, 0), (0, 0)), ((0, 0), (1, 1)) ) -> ((1,0), (-1,-1))
   */
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) - m2(i)(j))
  }

  /** multStrassen: Calcula la multiplicación entre dos matrices, usando el método de Strassen
   *
   * Recibe: La dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: multStrassen( ((0,0), (1,1)), ((0,0), (1,1)) ) -> ((0,0), (1,1))
   */
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length / 2

    if (l <= 1) {
      multMatriz(m1, m2)
    } else {

      val mm1 = Vector.tabulate(2, 2)((i, j) => subMatriz(m1, i * l, j * l, l))
      val mm2 = Vector.tabulate(2, 2)((i, j) => subMatriz(m2, i * l, j * l, l))

      val (a11, a12, a21, a22) = (mm1(0)(0), mm1(0)(1), mm1(1)(0), mm1(1)(1))
      val (b11, b12, b21, b22) = (mm2(0)(0), mm2(0)(1), mm2(1)(0), mm2(1)(1))

      val (s1, s2) = (restaMatriz(b12, b22), sumMatriz(a11, a12))
      val (s3, s4) = (sumMatriz(a21, a22), restaMatriz(b21, b11))
      val (s5, s6) = (sumMatriz(a11, a22), sumMatriz(b11, b22))
      val (s7, s8) = (restaMatriz(a12, a22), sumMatriz(b21, b22))
      val (s9, s10) = (restaMatriz(a11, a21), sumMatriz(b11, b12))

      val (p1, p2) = (multStrassen(a11, s1), multStrassen(s2, b22))
      val (p3, p4) = (multStrassen(s3, b11), multStrassen(a22, s4))
      val (p5, p6) = (multStrassen(s5, s6), multStrassen(s7, s8))
      val p7 = multStrassen(s9, s10)

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      val mm = (c11 ++ c21) zip (c12 ++ c22)
      (for (n <- mm.indices) yield mm(n)._1 ++ mm(n)._2).toVector
    }
  }

  /** multStrassenPar: Calcula la multiplicación entre dos matrices, usando el método de Strassen, paralelamente
   *
   * Recibe: La dos matrices a operar
   *
   * Retorna : La matriz resultante
   *
   * Ejemplo: multStrassenPar( ((0,0), (1,1)), ((0,0), (1,1)) ) -> ((0,0), (1,1))
   */
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length / 2

    if (l == 1) {
      multMatriz(m1, m2)
    } else if (l < 32) {
      multStrassen(m1, m2)
    } else {

      val mm1 = Vector.tabulate(2, 2)((i, j) => subMatriz(m1, i * l, j * l, l))
      val mm2 = Vector.tabulate(2, 2)((i, j) => subMatriz(m2, i * l, j * l, l))

      val (a11, a12, a21, a22) = (mm1(0)(0), mm1(0)(1), mm1(1)(0), mm1(1)(1))
      val (b11, b12, b21, b22) = (mm2(0)(0), mm2(0)(1), mm2(1)(0), mm2(1)(1))

      val (s1, s2) = (restaMatriz(b12, b22), sumMatriz(a11, a12))
      val (s3, s4) = (sumMatriz(a21, a22), restaMatriz(b21, b11))
      val (s5, s6) = (sumMatriz(a11, a22), sumMatriz(b11, b22))
      val (s7, s8) = (restaMatriz(a12, a22), sumMatriz(b21, b22))
      val (s9, s10) = (restaMatriz(a11, a21), sumMatriz(b11, b12))

      val (p1, p2) = parallel(multStrassenPar(a11, s1), multStrassenPar(s2, b22))
      val (p3, p4) = parallel(multStrassenPar(s3, b11), multStrassenPar(a22, s4))
      val (p5, p6) = parallel(multStrassenPar(s5, s6), multStrassenPar(s7, s8))
      val p7 = multStrassenPar(s9, s10)

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      val mm = (c11 ++ c21) zip (c12 ++ c22)
      (for (n <- mm.indices) yield mm(n)._1 ++ mm(n)._2).toVector
    }
  }

}