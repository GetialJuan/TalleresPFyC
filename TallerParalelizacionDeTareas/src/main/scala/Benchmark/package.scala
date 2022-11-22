import Matrices._
import org.scalameter._
package object Benchmark {
  type AlgoritmoMult = (Matriz, Matriz) => Matriz

  def compararAlgoritmos(a1:AlgoritmoMult, a2:AlgoritmoMult)
                        (m1:Matriz, m2:Matriz):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(m1,m2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(m1,m2))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
}
