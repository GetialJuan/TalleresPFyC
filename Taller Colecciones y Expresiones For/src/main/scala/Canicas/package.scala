package object Canicas {

  type Frasco = (Int, Int)
  type Distr = List[Frasco]

  def canicasPosiblesFrasco(f:Int, c:Int) : List[Frasco] = {
    for (e <- (0 to c).toList) yield (f,e)
  }

  def canicasPorFrasco(n:Int, c:Int) : List[Distr] = {
    for (f <- (1 to n).toList) yield canicasPosiblesFrasco(f, c)
  }

  def mezclarLCanicas(lc:List[Distr]) : List[Distr] = {

    def mezclarLC(l1:List[Distr], l2: List[Distr]): List[Distr] = {
      if (l2.isEmpty) l1
      else mezclarLC(aux(l1, l2.head), l2.tail)
    }

    def aux(l1:List[Distr], d2:Distr) : List[Distr] = {
      if(l1.isEmpty) for(f <- d2) yield f::Nil
      else for (d <- l1; f <- d2) yield d:+f
    }

    mezclarLC(aux(Nil, lc.head), lc.tail)

  }

  def distribucion(m:Int, n:Int, c:Int) : List[Distr] = {

    def condicion(d:Distr) : Boolean = (for (f <- d) yield f._2).sum == m

    val posiblesCasos = mezclarLCanicas(canicasPorFrasco(n, c))
    for (d <- posiblesCasos if condicion(d)) yield d
  }

  def agrupaciones(m:Int) : List[List[Int]] = {

    def noTieneReps(li:List[Int]):Boolean = {
      li.size == li.toSet.size
    }

    def nMax(m:Int): Int = {
      def nM(acum: Int, a: Int, b: Int): Int = {
        if (acum > a) b
        else nM(acum + b, a, b + 1)
      }
      nM(1, m, 1)
    }

    val agr = for (d <- distribucion(m, nMax(m),m)) yield for (f <- d; if f._2 != 0) yield f._2
    val agr2 = for (li <- agr; if noTieneReps(li)) yield li.toSet
    for (li <- agr2.distinct) yield li.toList
  }
}