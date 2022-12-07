import kmedianas._
import org.scalameter._
import scala.util.Random
import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.util.Random
import scala.math._

import scala.collection.parallel.ParSeq
package object Benchmark {

  def probarAlgoritmoSeq(puntos:Seq[Punto], medianas:Seq[Punto], eta:Double):Double = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 1),
      KeyValue(Key.exec.maxWarmupRuns -> 1),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (kMedianasSeq(puntos, medianas, eta))

    timeA1.value
  }

  def probarAlgoritmoPar(puntos: ParSeq[Punto], medianas: ParSeq[Punto], eta: Double): Double = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 30),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (kMedianasPar(puntos, medianas, eta))

    timeA1.value
  }
}
