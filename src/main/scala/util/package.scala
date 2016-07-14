package object util {
  case class TagTuple(word: String, symb: String)
  type TagIter = Iterator[TagTuple]
  type SentIter = Iterator[Iterator[String]]
  type TaggedSentIter = Iterator[Iterator[TagTuple]]

  abstract class DocSet {
    val corpus: TaggedCorpus
    val keyDoc: TaggedCorpus
    val devDoc: Document
  }

  object WikiDocSet extends DocSet {
    val corpus = WikiTrainingCorpus
    val keyDoc = WikiKey
    val devDoc = WikiDev
  }

  object GeneDocSet extends DocSet {
    val corpus = GeneTrainingCorpus
    val keyDoc = GeneKey
    val devDoc = GeneDev
  }
}