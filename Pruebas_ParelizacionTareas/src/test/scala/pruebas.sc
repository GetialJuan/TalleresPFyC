import Matrices._
import Benchmark._
import org.scalameter._

/*val resultados = for {
  i <- 1 to 10
  m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
  m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
} yield ( compararAlgoritmos ( multMatriz, multMatrizPar)(m1,m2) ,
  compararAlgoritmos ( multMatrizRec, multMatrizRecPar)(m1,m2),
  compararAlgoritmos ( multStrassen, multStrassenPar)(m1,m2),
  math.pow(2, i).toInt)

println(resultados)*/

val m1 = matrizAlAzar(4, 2)
val m2 = matrizAlAzar(4, 2)

multMatriz(m1, m2)
multMatrizPar(m1, m2)
multMatrizRec(m1, m2)
multMatrizRecPar(m1, m2)
multStrassen(m1, m2)
multStrassenPar(m1, m2)