package util


abstract class DocSet {
  val name: String
  val ruleDoc: Document
  val corpus: TaggedCorpus
  val keyDoc: TaggedCorpus
  val devDoc: Document
  val preprocessor: Preprocessor
}

case object WikiDocSet extends DocSet {
  val name = "Wikipedia Documents"
  val corpus = WikiCorpus("/wikitag.train.txt")
  val keyDoc = WikiCorpus("/wikitag.key.txt")
  val devDoc = Document("/wikitag.dev.txt")
  val ruleDoc = Document("/wikitag.rules.txt")
  val preprocessor = PatternPreprocessor
}

case object GeneDocSet extends DocSet {
  val name = "BioCreative Gene Documents"
  val corpus = GeneCorpus("/genetag.train.txt")
  val keyDoc = GeneCorpus("/genetag.key.txt")
  val devDoc = Document("/genetag.dev.txt")
  val ruleDoc = Document("/genetag.rules.txt")
  val preprocessor = ReplacePreprocessor
}