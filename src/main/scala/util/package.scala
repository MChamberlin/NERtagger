package object util {
  case class TagTuple(word: String, symb: String)
  type TagIter = Iterator[TagTuple]
  type SentIter = Iterator[Iterator[String]]
  type TaggedSentIter = Iterator[Iterator[TagTuple]]
}