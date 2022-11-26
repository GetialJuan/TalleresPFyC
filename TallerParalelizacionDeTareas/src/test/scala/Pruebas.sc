import Matrices._
import Benchmark._
/*
//Matrices 1x1
val m1_1 = matrizAlAzar(1,2)
val m1_2 = matrizAlAzar(1,2)

//Matrices 2x2
val m2_1 = matrizAlAzar(2,2)
val m2_2 = matrizAlAzar(2,2)

//Matrices 4x4
val m4_1 = matrizAlAzar(4,2)
val m4_2 = matrizAlAzar(4,2)

//Matrices 8x8
val m8_1 = matrizAlAzar(8,2)
val m8_2 = matrizAlAzar(8,2)


//1.1.1. multMatriz
multMatriz(m1_1,m1_2)
multMatriz(m2_1,m2_2)
multMatriz(m4_1,m4_2)
multMatriz(m4_2,m4_1)
multMatriz(m8_1,m8_2)

//1.1.2. multMatrizPar
multMatrizPar(m1_1,m1_2)
multMatrizPar(m2_1,m2_2)
multMatrizPar(m4_1,m4_2)
multMatrizPar(m4_2,m4_1)
multMatrizPar(m8_1,m8_2)

//1.2.1 subMatriz
subMatriz(m4_1,0,0, m4_1.length/2)
subMatriz(m4_1,0,m4_1.length/2, m4_1.length/2)
subMatriz(m4_1,m4_1.length/2,0, m4_1.length/2)
subMatriz(m4_1,m4_1.length/2,m4_1.length/2, m4_1.length/2)

//1.2.2 sumMatriz
sumMatriz(m1_1,m1_2)
sumMatriz(m2_1,m2_2)
sumMatriz(m4_1,m4_2)
sumMatriz(m4_2,m4_1)
sumMatriz(m8_1,m8_2)

//1.2.3 multMatrizRec
multMatrizRec(m1_1,m1_2)
multMatrizRec(m2_1,m2_2)
multMatrizRec(m4_1,m4_2)
multMatrizRec(m4_2,m4_1)
multMatrizRec(m8_1,m8_2)

//1.2.4 multMatrizRecPar
multMatrizRecPar(m1_1,m1_2)
multMatrizRecPar(m2_1,m2_2)
multMatrizRecPar(m4_1,m4_2)
multMatrizRecPar(m4_2,m4_1)
multMatrizRecPar(m8_1,m8_2)

//1.3.1 restaMatriz
restaMatriz(m1_1,m1_2)
restaMatriz(m2_1,m2_2)
restaMatriz(m4_1,m4_2)
restaMatriz(m4_2,m4_1)
restaMatriz(m8_1,m8_2)

//1.3.2 multStrassen
multStrassen(m1_1,m1_2)
multStrassen(m2_1,m2_2)
multStrassen(m4_1,m4_2)
multStrassen(m4_2,m4_1)
multStrassen(m8_1,m8_2)

//1.3.3 multStrassenPar
multStrassenPar(m1_1,m1_2)
multStrassenPar(m2_1,m2_2)
multStrassenPar(m4_1,m4_2)
multStrassenPar(m4_2,m4_1)
multStrassenPar(m8_1,m8_2)
*/
///////EVALUACION COMPARATIVA///////

def evaluacion(f:(Matriz,Matriz) => Matriz) = {
  def auxEval(f:(Matriz,Matriz) => Matriz, n:Int) = {
    def m: Matriz = matrizAlAzar(math.pow(2, n).toInt, 2)
    for (
      i <- (1 to 5).toVector
    ) yield probarAlgoritmo(f)(m, m)
  }

  for (
    i <- (1 to 5)
  ) yield (math.pow(2, i).toInt, auxEval(f,i))
}

def evaluacionGeneral = {
  //promedio de velocidad de ejecucion
  def promVel(vels:Vector[Double]):Double = {
    vels.sum / vels.length
  }

  def

  for (
    i <- (1 to 5);
    m1 = matrizAlAzar(math.pow(2, i).toInt, 2);
    m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
  ) yield (

  )

}
def evaluacionComparativa(f1:(Matriz,Matriz) => Matriz, f2:(Matriz,Matriz) => Matriz) = {
  for (
    i <- (1 to 10 );
    m1 = matrizAlAzar(math.pow(2, i).toInt, 2);
    m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
  ) yield (compararAlgoritmos(f1, f2)(m1, m2), math.pow(2, i).toInt)
}

//multMatriz
//evaluacion(multMatriz)

//multMatrizPar
//evaluacion(multMatrizPar)

//multMatrizRec
//evaluacion(multMatrizRec)

//multMatrizRecPar
//evaluacion(multMatrizRecPar)

//multStrassen
//evaluacion(multStrassen)

//multStrassenPar
//evaluacion(multStrassenPar)

//multMatriz vs multMatrizPar
//evaluacionComparativa(multMatriz, multMatrizPar)

//multMatrizRec vs multMatrizRecPar
//evaluacionComparativa(multMatrizRec, multMatrizRecPar)

//multStrassen vs multStrassenPar
//evaluacionComparativa(multStrassen, multStrassenPar)
