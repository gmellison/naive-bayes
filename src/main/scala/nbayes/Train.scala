package nbayes

import java.io.File
import scalaz._
import Scalaz._
import scala.collection.immutable.HashMap


trait TrainUtils extends Files {

  // Defines values and functions for "sampling" train and test document sets, &
  // computing conditional word probabilities (pWH & pWS) &
  // category probabilities (pS & pH)

  // a couple "inputs" -- categories, and proportion of documents to use for training
  val trainProp: Double = 0.75
  val categories = Set("spam", "ham")

  // list all files (as a map with category name as key) & then turn them into documents
  val files: Map[Category, List[File]] = categories.map(cat => cat -> getFiles(cat)).toMap

  val trainFiles: Map[Category, List[File]] = files.mapValues(fl => fl.take( math.floor(fl.length * trainProp).toInt ))
  val testFiles: Map[Category, List[File]] = files.mapValues(fl => fl.drop( math.floor(fl.length * trainProp).toInt ))

//  // this uses too much memory??
//  val catDocuments: Map[Category, List[Document]] = files.mapValues(file => file.map(f => fileToDocument(f)))

  // count all documents, and count documents in each category
  val catDocCount: Map[String, Int] = trainFiles.mapValues(list => list.length)
  val totalDocCount: Int = catDocCount.values.sum

  // this takes a list of documents and aggregates them into a single doc
  def combineDocs(doc1: Document, doc2: Document): Document = {
    doc1 |+| doc2
  }

  def wCP(w: Word, c: Category, allCounts: Map[Category, Document]): Double = {
    val doc: Document = allCounts apply c
    val n: Int = catDocCount apply c

    val wc: Int = try {
      doc apply w
    } catch {
      case e: java.util.NoSuchElementException => 0
    }
      wc.toDouble / n
  }

//  def dCP(d: Document, c: Category, allCounts: Map[Category, Document]): Double = {
//    // reduce over all word/count pairs in the document
//    d.foldLeft(1.0) {
//
//      //for each pair, do...
//      (aggProd, pair) => {
//
//        // compute numerator (p(w_i | c) for a given word
//        val num: Double = aggProd * wCP(pair._1, c, allCounts)
//
//        // compute denominator (p_w_i | !c) for the same word by summing prob given all non c categories.
//        val denom: Double = categories.dropWhile(s => s != c).foldLeft(1.0) {
//          (aggSum, cat) => aggSum + wCP(pair._1, cat, allCounts)
//        }
//        // then aggregate the prod of the log.
//        aggProd * Math.log(num / denom)
//      }
//    }
//  }
}


//  // list of counts aggregating over all categories -- treats all files as one big document
//  val overallWordCounts: Document = catWordCounts.values.reduce((docs1, docs2) => docs1 |+| docs2)
//  val wordProbs: Map[Word, Double] = catWordCounts.mapValues(count => count.toDouble




trait Trains extends Files with TrainUtils {
//  val trainDocs: Map[Category, Document] =

  lazy val trainCounts: Map[Category, Document] = trainFiles.mapValues(fl => fl.foldLeft(Map.empty[String, Int]) {
    (left, right) => combineDocs(left, fileToDocument(right))
  })

  lazy val testWords: Set[Word] = testFiles.mapValues(fl => fl.flatMap(f => docListWords(f))).foldLeft(Set.empty[Word]){
    (left, right) => left ++ right._2
  }

  def catWordProbMap(cat: Category): Map[Word, Double] = {
    testWords.map(word => word -> wCP(word, cat, trainCounts)).toMap
  }

  lazy val testProbs: Map[Category, Map[Word, Double]] = categories.map(cat => {
    (cat -> catWordProbMap(cat))
  }).toMap
}

class Test extends Trains {

   def predictCategory(doc: Document, c: Category, counts: Map[Category, Document]): Boolean = {

     val notC: Set[Category] = categories.filter(cat => cat != c)
     val catProbRatio: Double = math.log(catDocCount(c) + 1 / (notC.map(cat => catDocCount(cat)).sum + 1))

     val logProbRatio = doc.foldLeft(0.0) { (aggSum, docPair) => {
//       val num = wCP(docPair._1, c, counts)
//       val denom = notC.map(cat => wCP(docPair._1, cat, counts)).sum
//
//       val term = docPair._2 * math.log(num + 1 / (denom + 1))

       val num = try {
         testProbs apply c apply docPair._1
       } catch {
         case e: java.util.NoSuchElementException => 0
       }
       val denom = notC.map(cat => {
         try {
           testProbs apply cat apply docPair._1
         } catch {
           case e: java.util.NoSuchElementException => 0
         }
       }).sum

       val term = docPair._2 * math.log(num + 1 / (denom + 1))

       aggSum + term
     }}

     catProbRatio + logProbRatio > 0
   }

}