package util

import java.io.{IOException, FileNotFoundException}

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.HashSet


abstract class TaggedCorpus {
  val posSymbSet: HashSet[String]
  def getSentIter: TaggedSentIter
}

class BarSepTagCorpus extends TaggedCorpus {
  val posSymbSet = HashSet("I-GENE")
  val filename = "genetag.train.txt"
  val pattern = new Regex("""([^\s\|]+)\|([^\s\|]+)""", "word", "symb")

  // TODO: wrap in try/catch block or figure out how to deal with `source`
  def getSentIter: TaggedSentIter = {
    val source = Source.fromFile(filename)
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
      yield pattern.findAllMatchIn(line).map( m => TagTuple(m.group("word"),m.group("symb")) )
  }
}

object GeneTrainingCorpus extends BarSepTagCorpus {
  override val filename = "genetag.train.txt"
}

object GeneKey extends BarSepTagCorpus {
  override val filename = "genetag.key.txt"
}