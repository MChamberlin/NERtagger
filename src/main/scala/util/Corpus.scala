package util

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.HashSet


abstract class TaggedCorpus {
  val posSymbSet: HashSet[String]
  def getSentIter: TaggedSentIter
}

abstract class PatternCorpus extends TaggedCorpus {
  val filename: String
  val pattern: Regex

  // TODO: wrap in try/catch block or figure out how to deal with `source`
  def getSentIter: TaggedSentIter = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(filename))
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
      yield pattern.findAllMatchIn(line).map( m => TagTuple(m.group("word"),m.group("symb")) )
  }
}

case class GeneCorpus(filename: String) extends PatternCorpus {
  val pattern = new Regex("""([^\s\|]+)\|([^\s\|]+)""", "word", "symb")
  val posSymbSet = HashSet("I-GENE")
}

case class WikiCorpus(filename: String) extends PatternCorpus {
  val pattern = new Regex("""([^\s\|]+)\|([^\|]+)\|([^\s\|]+)""", "word", "POS", "symb")
  val posSymbSet = HashSet("I-MISC","I-PER","I-ORG","I-LOC")
}
