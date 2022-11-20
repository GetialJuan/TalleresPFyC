package object Recursion {
  /**
   * Ejercicio 1.1.1
   * maximo comun divisor a partir del teorema fundamental de la aritmetica
   */
  def mcdTFA(ln: List[Int], lm: List[Int], primos: List[Int]): Int = {
    if (primos.tail.isEmpty) Math.pow(primos.head, Math.min(ln.head, lm.head)).toInt
    else Math.pow(primos.head, Math.min(ln.head, lm.head)).toInt * mcdTFA(ln.tail, lm.tail, primos.tail)
  }

  /**
   * Ejercicio 1.1.2
   * maximo comun divisor a partir del teorema de Euclides con coefiecientes de Bezout
   */
  def mcdEBez(n: Int, m: Int): (Int, Int, Int) = {
    def mcdBZ(r1: Int, r2: Int, x1: Int, x2: Int, y1: Int, y2: Int): (Int, Int, Int) = {
      if (r2 == 0) (r1, x1, y1)
      else {
        val q = Math.round(r1 / r2)
        mcdBZ(r2, r1 % r2, x2, x1 - (q * x2), y2, y1 - (q * y2))
      }
    }

    mcdBZ(n, m, 1, 0, 0, 1)
  }

  /**
   * Ejercicio 1.2.1
   * fibonacci recursivo de arbol
   */
  def fibonacciA(n: Int): Int = {
    if (n == 0 || n == 1) 1
    else fibonacciA(n - 1) + fibonacciA(n - 2)
  }

  /**
   * Ejercicio 1.2.2
   * fibonacci iterativo
   */
  def fibonacciI(n: Int): Int = {
    def fibI(m: Int, x: Int, y: Int): Int = if (m > n) y else fibI(m + 1, x + y, x)

    fibI(1, 1, 1)
  }
}
