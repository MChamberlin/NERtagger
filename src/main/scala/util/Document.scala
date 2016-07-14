package util

import scala.io.Source


abstract class Document {
  def getSentIter: SentIter
}

object GeneDev extends Document {
  val filename = "/genetag.dev.txt"

  def getSentIter: SentIter = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(filename))
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
      yield line.split(" ").toIterator
  }

}
