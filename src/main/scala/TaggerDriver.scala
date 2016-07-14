import java.io.File

import scopt.OptionParser
import tagger.{TaggerEvaluator, Tagger}
import util._

object TaggerDriver extends App {
  // TODO: add commands to train, score, and tag separately using train/dev/key args
  case class Config(outFile: File = new File("."),
                    trainingCorpus: TaggedCorpus = GeneTrainingCorpus,
                    rareThreshold: Int = 5,
                    maxNGramSize: Int = 3,
                    preprocessor: Preprocessor = PatternPreprocessor,
                    devDoc: Document = GeneDev,
                    keyDoc: TaggedCorpus = GeneKey
                     )

  val parser = new OptionParser[Config]("NERTagger") {
    head("NER Tagger", "1.0")

    opt[File]('o', "out").required().valueName("<file>").
      action( (x, c) => c.copy(outFile = x) ).
//      validate( x =>
//        if (x.canWrite) success
//        else failure(s"${x.getPath} is not writeable")).
      text("output file name")

    opt[Int]('r', "rare").optional().valueName("<int>").
      action( (x, c) => c.copy(rareThreshold = x) ).
      validate( x =>
      if (x > 0) success
      else failure(s"Rare theshold $x must be > 0")).
      text("rare count threshold")

  }

  // parser.parse returns Option[C]
  parser.parse(args, Config()).map { config =>
    val tagger = new Tagger (
      config.trainingCorpus,
      config.rareThreshold,
      config.maxNGramSize,
      config.preprocessor)
    tagger.writeDocumentTags(config.devDoc, config.outFile)
    println(s"Tagged sentences written to ${config.outFile.getName}")
    val scorer = new TaggerEvaluator(tagger)
    scorer.score(config.devDoc, config.keyDoc)
  } getOrElse {
    // arguments are bad, usage message will have been displayed
  }
}