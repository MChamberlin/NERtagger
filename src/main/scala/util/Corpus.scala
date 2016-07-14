package util

import java.io.{File, FileInputStream, IOException, FileNotFoundException}

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.HashSet


abstract class TaggedCorpus {
  val posSymbSet: HashSet[String]
  def getSentIter: TaggedSentIter
}

abstract class BarSepTagCorpus extends TaggedCorpus {
  private val pattern = new Regex("""([^\s\|]+)\|([^\s\|]+)""", "word", "symb")
  val filename: String

  // TODO: wrap in try/catch block or figure out how to deal with `source`
  def getSentIter: TaggedSentIter = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(filename))
    for (line <- source.reset().getLines() if line.trim.nonEmpty)
      yield pattern.findAllMatchIn(line).map( m => TagTuple(m.group("word"),m.group("symb")) )
  }
}

abstract class GeneCorpus extends BarSepTagCorpus {
  val posSymbSet = HashSet("I-GENE")
}

case object GeneTrainingCorpus extends GeneCorpus {
  override val filename = "/genetag.train.txt"
}

object GeneKey extends GeneCorpus {
  override val filename = "/genetag.key.txt"
}