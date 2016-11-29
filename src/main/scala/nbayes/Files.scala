package nbayes

import java.io._
import java.nio.charset.MalformedInputException

trait Files {

  type Category = String
  type Word = String
  type Document = Map[Word, Int]


  private val path = "/Users/gregellison/Documents/nbayes/src/main/resources/"
  private val dir = new File(path)

  def getFiles(c: String): List[File] = {
    val filePath = path + c

    val f = new File(filePath)
    f.listFiles.toList
  }

  def using[A <: { def close(): Unit }, B](resource: A)(f:A => B): B = {
    try f(resource)
    finally resource.close()
  }


  def docListWords(f: File):List[Word] = {
    using(io.Source.fromFile(f)) { source => {
      try {
        val words = for {
          line <- source.getLines
        } yield line
        words.toList.flatMap(str => str
          .toLowerCase
          .replaceAll("[:.,)(?!]", "")
          .replaceAll("[0-9]*", "")
          .split(" ")
          .filter(s => s != ""))
      } catch {
        case e: MalformedInputException => List()
      }}
    }
  }

  def fileToDocument(f: File): Document = {
    // read the file (in a try/catch in case of weird encodings) and clean
    val documentWords: List[Word] = docListWords(f)

    // turn string into map of word -> count with a foldLeft
    documentWords.foldLeft(Map.empty[Word, Int]) {
      (counts, word) => counts + (word -> (counts.getOrElse(word, 0) + 1))
    }
  }



}





