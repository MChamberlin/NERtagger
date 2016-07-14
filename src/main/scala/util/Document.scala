package util

import scala.io.Source


abstract class Document {
  val filename: String

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

object GeneDev extends Document {
  val filename = "/genetag.dev.txt"
}

object GeneRules extends Document {
  val filename = "/genetag.rules.txt"
}

object WikiDev extends Document {
  val filename = "/wikitag.dev.txt"
}

object WikiRules extends Document {
  val filename = "/wikitag.rules.txt"
}
