import Matrices._
import Benchmark._
/*
val m1 = matrizAlAzar(1,2)
val m2 = matrizAlAzar(2,2)
val m3 = matrizAlAzar(4,2)
val m4 = matrizAlAzar(4,2)
*/
//1.1.1. multMatriz

//1.1.2. multMatrizPar
//multMatrizPar(m3,m4)

//1.2.1 subMatriz

//1.2.2 sumMatriz

//1.2.3 multMatrizRec

//1.2.4 multMatrizRecPar

//1.3.1 restaMatriz

//1.3.2 multStrassen

//1.3.3 multStrassenPar

///////EVALUACION COMPARATIVA///////

def evaluacionComparativa(f1:(Matriz,Matriz) => Matriz, f2:(Matriz,Matriz) => Matriz) = {
  for (
    i <- (1 to 10 );
    m1 = matrizAlAzar(math.pow(2, i).toInt, 2);
    m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
  ) yield (compararAlgoritmos(f1, f2)(m1, m2), math.pow(2, i).toInt)
}

//multMatriz vs multMatrizPar
//evaluacionComparativa(multMatriz, multMatrizPar)

//multMatrizRec vs multMatrizRecPar
//evaluacionComparativa(multMatrizRec, multMatrizRecPar)

//multStrassen vs multStrassenPar
evaluacionComparativa(multStrassen, multStrassenPar)
