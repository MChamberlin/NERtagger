package util


abstract class DocSet {
  val name: String
  val ruleDoc: ResourceDocument
  val corpus: TaggedCorpus
  val keyDoc: TaggedCorpus
  val devDoc: ResourceDocument
  val preprocessor: Preprocessor
}

case object WikiDocSet extends DocSet {
  val name = "Wikipedia Documents"
  val corpus = WikiCorpus("/wikitag.train.txt")
  val keyDoc = WikiCorpus("/wikitag.key.txt")
  val devDoc = ResourceDocument("/wikitag.dev.txt")
  val ruleDoc = ResourceDocument("/wikitag.rules.txt")
  val preprocessor = PatternPreprocessor
}

case object GeneDocSet extends DocSet {
  val name = "BioCreative Gene Documents"
  val corpus = GeneCorpus("/genetag.train.txt")
  val keyDoc = GeneCorpus("/genetag.key.txt")
  val devDoc = ResourceDocument("/genetag.dev.txt")
  val ruleDoc = ResourceDocument("/genetag.rules.txt")
  val preprocessor = ClassifyPreprocessor
}