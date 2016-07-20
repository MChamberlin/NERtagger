package util

import java.io.File

import scala.io.Source
import scala.util.{Try,Success,Failure}

abstract class Document {
  def getLineIter: Iterator[String]
  def getSentIter: SentIter
}


case class ResourceDocument(filename: String) extends Document {

  def getLineIter: Iterator[String] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(filename))
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
        yield line.trim
  }

  def getSentIter: SentIter = {
    for (line <- getLineIter)
    yield line.split(" ").toIterator
  }

}

case class SpaceSepDocument(file: File) extends Document {

  def getSource: Try[Source] = {
    Try(Source.fromFile(file))
  }

  def getLineIter: Iterator[String] = {
    getSource match {
      case Failure(f) => println(f); Iterator.empty
      case Success(s) => for (line <- s.getLines() if line.trim.nonEmpty)
        yield line.trim
    }
  }

  def getSentIter: SentIter = {
    for (line <- getLineIter)
      yield line.split(" ").toIterator
  }
}