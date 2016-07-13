package util

import scala.io.Source


abstract class Document {
  def getSentIter: SentIter
}

object GeneDev extends Document {
  val filename = "genetag.dev.txt"
  val source = Source.fromFile(filename)

  def getSentIter: SentIter = {
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
      yield line.split(" ").toIterator
  }

}
