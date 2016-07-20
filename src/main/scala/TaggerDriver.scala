import java.io.File

import scopt.OptionParser
import tagger.{TaggerEvaluator, Tagger}
import util._

/**
 * Main application driver providing access to the NER tagger from the command line
 */
object TaggerDriver extends App {
  case class Config(outFile: Option[File] = None,
                    inFile: Option[File] = None,
                    docSet: DocSet = WikiDocSet,
                    rareThreshold: Int = 5,
                    maxNGramSize: Int = 3,
                    preprocessor: Preprocessor = PatternPreprocessor,
                    train: Boolean = false,
                    mode: String = "score",
                    text: String = "")

  val parser = new OptionParser[Config]("NERtagger") {
    head("NERtagger", "1.0")

    help("help").text("ouptut usage information")

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

    opt[Unit]('t', "train").optional().action( (_, c) => c.copy(train = true) ).
      text("flag indicating whether or not to retrain the model")

    opt[String]('p', "pp").optional().valueName("<str>").
      validate( x =>
        if (Set("pattern","replace","classify").contains(x)) success
        else failure("Preprocessor must be one of {pattern|replace}")).
      action( (x, c) => x match {
      case "pattern" => c.copy(preprocessor = PatternPreprocessor)
      case "replace" => c.copy(preprocessor = ReplacePreprocessor)
      case "classify" => c.copy(preprocessor = ClassifyPreprocessor)}).
      text("Rare word preprocessor; must be one of {pattern|replace}")

    cmd("score").action( (_, c) => c.copy(mode = "score") ).
      children(
        opt[File]('o', "out").optional().valueName("<file>").
          action( (x, c) => c.copy(outFile = Some(x)) ).
          validate( x =>
            if (x.canWrite) success
            else failure(s"${x.getPath} is not writeable")).
          text("output file name (optional)")
      ).text("Scores tagger on dev/key set and outputs tagged development set to file if provided.")

    cmd("save").action( (_, c) => c.copy(mode = "save") ).
      children(
        opt[File]('o', "out").required().valueName("<file>").
          action( (x, c) => c.copy(outFile = Some(x)) ).
          validate( x =>
            if (x.canWrite) success
            else failure(s"${x.getPath} is not writeable")).
          text("output file name")
      ).text("Saves model rule counts to file for quicker loading")

    cmd("tag").action( (_, c) => c.copy(mode = "tag") ).
      children(
        arg[String]("<text>").optional().
          action( (x, c) => c.copy(text = x)).
          validate( x =>
            if (x.nonEmpty) success
            else failure("<text> must be non-empty")
          ).text("text to tag"),
        opt[File]('i', "input").optional().valueName("<file>").
          action( (x, c) => c.copy(inFile = Some(x)) ).
          validate( x =>
            if (x.canRead) success
            else failure(s"${x.getPath} is not readable")).
          text("input file name"),
        opt[File]('o', "out").optional().valueName("<file>").
          action( (x, c) => c.copy(outFile = Some(x)) ).
          validate( x =>
            if (x.canWrite) success
            else failure(s"${x.getPath} is not writeable")).
          text("output file name (optional)"),
        checkConfig( c =>
          if (c.inFile.isEmpty && c.text.isEmpty) failure("must provide either text or input file")
          else if (c.inFile.nonEmpty && c.text.nonEmpty) failure("must provide either text or input file, but not both")
          //else if (c.inFile.nonEmpty && c.outFile.isEmpty) failure("Must provide output file when tagging input file")
          else success )
      ).text("Tag sentence using trained model; output to file if provided")
  }

  parser.parse(args, Config()).map { config =>
    println(s"Using ${config.docSet.name}")
    val tagger: Tagger = if (config.train) {
      val tagger = new Tagger(config.preprocessor)
      println("Training tagger...")
      tagger.train(config.docSet.corpus, config.rareThreshold)
      tagger
    } else {
      val tagger = new Tagger(config.docSet.preprocessor)
      println("Loading tagger...")
      tagger.load(config.docSet.ruleDoc)
      tagger
    }
    if (config.mode == "score") {
      if (config.outFile.nonEmpty) {
        println(s"Writing tagged sentences...")
        tagger.writeDocumentTags(config.docSet.devDoc, config.outFile.get)
        println(s"Tagged sentences written to ${config.outFile.get.getName}")
      }
      println("Scoring tagger...")
      val scorer = new TaggerEvaluator(tagger)
      scorer.score(config.docSet.devDoc, config.docSet.keyDoc)
    } else if (config.mode == "save") {
      tagger.save(config.outFile.get)
    } else if (config.mode == "tag") {
      val tags: TaggedSentIter = if (config.inFile.nonEmpty) {
        tagger.getDocumentTags(SpaceSepDocument(config.inFile.get))
      } else {
        tagger.getTextTags(config.text)
      }
      if (config.outFile.nonEmpty) {
        tagger.writeTags(tags, config.outFile.get)
      } else {
        print("\nOutput:\n")
        BarSepDocFormatter.formatDocumentTags(tags).foreach(x => println(x))
      }
    }
  } getOrElse {
    // arguments are bad, usage message will have been displayed
  }
}