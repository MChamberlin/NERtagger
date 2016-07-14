package util

import java.io.{IOException, FileNotFoundException}

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.HashSet


abstract class TaggedCorpus {
  val posSymbSet: HashSet[String]
  def getSentIter: TaggedSentIter
}

abstract class BarSepTagCorpus extends TaggedCorpus {
  val pattern = new Regex("""([^\s\|]+)\|([^\s\|]+)""", "word", "symb")
  val filename: String
  val source: Source

  // TODO: wrap in try/catch block or figure out how to deal with `source`
  def getSentIter: TaggedSentIter = {
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
      yield pattern.findAllMatchIn(line).map( m => TagTuple(m.group("word"),m.group("symb")) )
  }
}

abstract class GeneCorpus extends BarSepTagCorpus {
  val posSymbSet = HashSet("I-GENE")
}

object GeneTrainingCorpus extends GeneCorpus {
  override val filename = "genetag.train.txt"
  override val source = Source.fromURL(ClassLoader.getSystemResource(filename))
}

object GeneKey extends GeneCorpus {
  override val filename = "genetag.key.txt"
  override val source = Source.fromURL(ClassLoader.getSystemResource(filename))
}