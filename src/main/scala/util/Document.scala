package util

import scala.io.Source


abstract class Document {
  val filename: String

  def getSentIter: SentIter = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(filename))
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
    yield line.split(" ").toIterator
  }
}

object GeneDev extends Document {
  val filename = "/genetag.dev.txt"
}

object WikiDev extends Document {
  val filename = "/wikitag.dev.txt"
}
