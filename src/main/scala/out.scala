import java.io.File
import nbayes._

object out extends App {

  val test = new Test
  val counts: Map[test.Category, test.Document] = test.trainCounts
  val toPredict: Map[test.Category, List[File]] = test.testFiles
  val predictions: Map[test.Category, List[Boolean]] = toPredict.map{case (cat: test.Category, fl: List[File]) => {
    (cat, fl.map(f => test.predictCategory(test.fileToDocument(f), cat, counts)))
  }}
  val summary: Map[test.Category, (Int, Int)] = predictions.mapValues(lb => lb.foldLeft((0, 0)){(l, r) => {
    if (r) (l._1 + 1, l._2)
    else (l._1, l._2 + 1)
  } })

  println("Class: (Correct, Incorrect)")
  println("Spam: ($summary.getOrElse(\"spam\", \"\")._1, $summary.getOrElse(\"spam\", \"\")._2)")
  println("Ham: ($summary.getOrElse(\"ham\", \"\")._1, $summary.getOrElse(\"ham\", \"\")._2)")

}

