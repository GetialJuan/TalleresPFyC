package object Newton {

  //Expresiones
  trait Expr
  case class Numero(d:Double) extends Expr
  case class Atomo(x:Char) extends Expr
  case class Suma(e1:Expr, e2:Expr) extends Expr
  case class Prod(e1:Expr, e2:Expr) extends Expr
  case class Resta(e1:Expr, e2:Expr) extends Expr
  case class Div(e1:Expr, e2:Expr) extends Expr
  case class Expo(e1:Expr, e2:Expr) extends Expr
  case class Logaritmo(e1:Expr) extends Expr

  /**
   * Ejercicio 1.1
   * funcion que dada una expresion e devuelve una cadena que
   *  representa la definicion simbolica de la funcion
   */
   def mostrar(e:Expr):String = e match {
     case Numero(d) => d.toString
     case Atomo(x) => x.toString
     case Suma(e1, e2) => "("+ mostrar(e1) +"+"+ mostrar(e2) +")"
     case Prod(e1, e2) => "("+ mostrar(e1) +"*"+ mostrar(e2) +")"
     case Resta(e1, e2) => "("+ mostrar(e1) +"-"+ mostrar(e2) +")"
     case Div(e1, e2) => "("+ mostrar(e1) +"/"+ mostrar(e2) +")"
     case Expo(e1, e2) => "("+ mostrar(e1) +"^"+ mostrar(e2) +")"
     case Logaritmo(e1) => "(lg("+ mostrar(e1)+"))"
   }

  /**
   * Ejercicio 1.2
   * funcion que dada una funcion representada por la expresion
   *  f y un atomo a (que representa la variable de la funcion), retorna la expresion correspondiente a su derivada 1
   *  con respecto a esa variable
   */
  def derivar(f:Expr, a:Atomo):Expr = f match {
    case Numero(d) => Numero(0)
    case Atomo(x) => if (f == a) Numero(1) else Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
    case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Div(Prod(derivar(e1, a), e2), e1), Prod(derivar(e2, a), Logaritmo(e1))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }

  /**
   * Ejercicio 1.3
   * funcion que dada una funcion representada por la expresion
   *  f y un atomo a (que representa la variable de la funcion), y un valor v flotante, calcula la
   *  evaluacion de la funcion en v
   */
  def evaluar(f:Expr, a:Atomo, v:Double):Double = f match {
    case Numero(d) => d
    case Atomo(x) => v
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
  }

  /**
   * Ejercicio 1.4
   * funcion limpiar que dada una funcion representada por la expresion
   *  f, retorna una formula equivalente pero que no contenga ceros ni unos innecesarios.
   */
  def limpiar(f:Expr):Expr = {
    def limpiarAux(f:Expr): Expr = f match {
      case Numero(d) => Numero(d)
      case Atomo(x) => Atomo(x)
      case Suma(e1, e2) =>
        if (e1 == Numero(0)) limpiarAux(e2)
        else if (e2 == Numero(0)) limpiarAux(e1)
        else Suma(limpiarAux(e1), limpiarAux(e2))
      case Resta(e1, e2) =>
        if (e1 == Numero(0)) Prod(limpiarAux(e2), Numero(-1))
        else if (e2 == Numero(0)) limpiarAux(e1)
        else Resta(limpiarAux(e1), limpiarAux(e2))
      case Prod(e1, e2) =>
        if (e1 == Numero(0) || e2 == Numero(0)) Numero(0)
        else if (e1 == Numero(1)) limpiarAux(e2)
        else if (e2 == Numero(1)) limpiarAux(e1)
        else Prod(limpiarAux(e1), limpiarAux(e2))
      case Div(e1, e2) =>
        if (e1 == Numero(0)) Numero(0)
        else if (e2 == Numero(1)) limpiarAux(e1)
        else Div(limpiarAux(e1), limpiarAux(e2))
      case Expo(e1, e2) =>
        if (e1 == Numero(0)) Numero(0)
        else if (e1 == Numero(1) || e2 == Numero(0)) Numero(1)
        else if (e2 == Numero(1)) limpiarAux(e1)
        else Expo(limpiarAux(e1), limpiarAux(e2))
      case Logaritmo(e1) => Logaritmo(limpiarAux(e1))
    }
    val expresionLimpia = limpiarAux(f)
    if(expresionLimpia == f) expresionLimpia
    else limpiar(expresionLimpia)
  }

  /**
   * Ejercicio 1.5
   * funcion que que dada una funcion representada
   *  por la expresion f y un atomo a (que representa la variable de la funcion), y un valor
   *  x0 flotante, candidato a raız de f, y una funcion booleana ba, retorna una raız, r, de la
   *  funcion usando el metodo de Newton
   */
  //Funcion auxiliar(ba)
  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    evaluar(f, a, d) < 0.001
  }
  def raizNewton(f:Expr, a:Atomo, x0:Double, ba:(Expr, Atomo, Double)=>Boolean):Double = {
    if(ba(f, a, x0)) x0
    else {
      val x1 = x0 - (evaluar(f, a, x0)/evaluar(derivar(f, a), a, x0))
      raizNewton(f, a, x1, ba)
    }
  }


}
