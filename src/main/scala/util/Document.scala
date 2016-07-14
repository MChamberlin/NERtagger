package util

import scala.io.Source


case class Document(filename: String) {

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