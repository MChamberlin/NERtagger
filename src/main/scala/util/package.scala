package object util {
  case class TagTuple(word: String, symb: String)
  type TagIter = Iterator[TagTuple]
  type SentIter = Iterator[Iterator[String]]
  type TaggedSentIter = Iterator[Iterator[TagTuple]]

  abstract class DocSet {
    val name: String
    val ruleDoc: Document
    val corpus: TaggedCorpus
    val keyDoc: TaggedCorpus
    val devDoc: Document
  }

  case object WikiDocSet extends DocSet {
    val name = "Wikipedia Documents"
    val ruleDoc = WikiRules
    val corpus = WikiTrainingCorpus
    val keyDoc = WikiKey
    val devDoc = WikiDev
  }

  case object GeneDocSet extends DocSet {
    val name = "BioCreative Gene Documents"
    val ruleDoc = GeneRules
    val corpus = GeneTrainingCorpus
    val keyDoc = GeneKey
    val devDoc = GeneDev
  }
}