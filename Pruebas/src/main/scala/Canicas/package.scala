package object Canicas {
  type Frasco = (Int, Int)

  type Distr = List [Frasco]

  //Ejercicio 1.1.1

  def canicasPosiblesFrasco(f:Int, c:Int):List[Frasco] =
    {
       (for (ca <- (0 to c)) yield (f, ca)).toList
    }

  //Ejercicio 1.1.2

  def canicasPorFrasco(n:Int, c:Int):List[Distr] =
  {
    (for (fr <- (1 to n)) yield ( canicasPosiblesFrasco(fr, c))).toList
  }

 //Ejercicio 1.1.3

  def auxMezclarLCanicas(fr:Frasco,start:List[Distr]):List[Distr]=
    {
      for (i <- start) yield fr::i
    }

  def aux2MezclarLCanicas(d:Distr, ld:List[Distr]): List[Distr] =
    {
       if (d ==Nil) List()
       else auxMezclarLCanicas(d.head, ld)++aux2MezclarLCanicas(d.tail, ld)
    }

  def mezclarLCanicas(lc: List[Distr]): List[Distr] = {
    def iter(lc: List[Distr], start: List[Distr]): List[Distr] = {
      if (lc == Nil) start
      else iter(lc.init, aux2MezclarLCanicas(lc.last, start))
    }

    iter(lc, List(List()))
  }

  //Ejercicio 1.1.4

  def sumarCanicas(lc:Distr, acum:Int):Int =
    {
      if (lc == Nil) acum
      else sumarCanicas(lc.tail, lc.head._2 + acum);
    }


  def distribucion(m:Int, n:Int, c:Int): List[Distr] =
    {
      for (d <- mezclarLCanicas(canicasPorFrasco(n,c)) if (sumarCanicas(d,0)==m && (d forall (_._2 <= c)))) yield d
    }

  //Ejercicio 1.2

  def ascendingOrderAux(l:List[Int], pivot:Int): Boolean =
  {
    if (l == Nil) true
    else if (pivot >= l.head) false
    else ascendingOrderAux(l.tail, l.head)
  }

  def ascendingOrder(l:List[List[Int]]):List[List[Int]] =
    {
      for (e <- l if ascendingOrderAux(e, 0)) yield e
    }

  def removeLeftZerosAux(l:List[Int]):List[Int] =
    {
      if (l == Nil) Nil
      else if (l.head == 0) removeLeftZerosAux(l.tail)
      else l
    }


  def removeLeftZeros(l:List[List[Int]]):List[List[Int]]=
    {
      for (i <- l) yield removeLeftZerosAux(i)
    }

  def convert(d:List[Distr]):List[List[Int]] =
  {
    for (i <- d) yield for (e <- i) yield e._2
  }


  def agrupaciones(m:Int):List[List[Int]]=
    {
      if (m==1) List(List(1))
      else ascendingOrder(removeLeftZeros(convert(distribucion(m, m-1, m))))
    }
}
