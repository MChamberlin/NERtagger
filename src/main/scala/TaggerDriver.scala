import java.io.File

import scopt.OptionParser
import tagger.{TaggerEvaluator, Tagger}
import util._

object TaggerDriver extends App {
  // TODO: add commands to train, score, and tag separately using train/dev/key args
  case class Config(outFile: File = new File("."),
                    docSet: DocSet = WikiDocSet,
                    rareThreshold: Int = 5,
                    maxNGramSize: Int = 3,
                    preprocessor: Preprocessor = PatternPreprocessor)

  val parser = new OptionParser[Config]("NERTagger") {
    head("NER Tagger", "1.0")

    opt[File]('o', "out").required().valueName("<file>").
      action( (x, c) => c.copy(outFile = x) ).
      validate( x =>
        if (x.canWrite) success
        else failure(s"${x.getPath} is not writeable")).
      text("output file name")

    opt[String]('c', "corpus").optional().valueName("<str>").
      validate( x =>
        if (x == "gene" | x == "wiki") success
        else failure("Corpus must be one of {wiki|gene}")).
      action( (x, c) => x match {
        case "wiki" => c.copy(docSet = WikiDocSet)
        case "gene" => c.copy(docSet = GeneDocSet)}).
      text("training corpus; must be one of {wiki|gene}")

    opt[Int]('r', "rare").optional().valueName("<int>").
      action( (x, c) => c.copy(rareThreshold = x) ).
      validate( x =>
        if (x > 0) success
        else failure(s"Rare theshold $x must be > 0")).
      text("rare count threshold")

    opt[Int]('n',"ngram").optional().valueName("<int>").
      action( (x, c) => c.copy(maxNGramSize = x) ).
      validate( x =>
        if (x == 3) success
        else failure(s"Only n = 3 supported currently")).
      text("max n-gram size")

  }

  // parser.parse returns Option[C]
  parser.parse(args, Config()).map { config =>
    val tagger = new Tagger(config.rareThreshold, config.maxNGramSize, config.preprocessor)
    println(s"Training tagger...")
    tagger.train(config.docSet.corpus)
    println(s"Writing tagged sentences...")
    tagger.writeDocumentTags(config.docSet.devDoc, config.outFile)
    println(s"Tagged sentences written to ${config.outFile.getName}")
    val scorer = new TaggerEvaluator(tagger)
    scorer.score(config.docSet.devDoc, config.docSet.keyDoc)
  } getOrElse {
    // arguments are bad, usage message will have been displayed
  }
}