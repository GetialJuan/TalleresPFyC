package object Derivacion {

  /**
   * Ejercicio 1.1 El derivador generico
   * derivada aproximada de una funcion segun la ecuacion de los 5 puntos
   */
  def derivada(f:Double => Double) : Double => Double = {
    val h = 0.1
    (x0) => ( f(x0-(2*h)) - 8*f(x0-h) + 8*f(x0+h) - f(x0+(2*h)) )/(12*h)
  }

  /**
   * Ejercicio 1.2.1 Derivada de la suma
   * derivada aproximada de la suma de dos funciones dadas
   */
  def derivadaSuma(f:Double=>Double, g:Double=>Double) : Double=>Double = {
    (x) => derivada(f)(x) + derivada(g)(x)
  }

  /**
   * Ejercicio 1.2.2 Derivada de la resta
   * derivada aproximada de la resta de dos funciones dadas
   */
  def derivadaResta(f:Double=>Double, g:Double=>Double) : Double=>Double = {
    (x) => derivada(f)(x) - derivada(g)(x)
  }

  /**
   * Ejercicio 1.2.3 Derivada de la multiplicacion
   * derivada aproximada de la multiplicacion de dos funciones dadas
   */
  def derivadaMult(f: Double => Double, g: Double => Double): Double => Double = {
    (x) => ( derivada(f)(x) * g(x) ) + ( derivada(g)(x) * f(x) )
  }

  /**
   * Ejercicio 1.2.4 Derivada de la division
   * derivada aproximada de la division de dos funciones dadas
   */
  def derivadaDiv(f: Double => Double, g: Double => Double): Double => Double = {
    (x) => ( (derivada(f)(x) * g(x)) - (derivada(g)(x) * f(x)) )/math.pow(g(x),2)
  }
}
