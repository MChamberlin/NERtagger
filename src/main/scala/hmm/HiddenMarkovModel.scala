package hmm

import java.io._

import util._

import scala.collection.mutable.{HashMap, HashSet}

/** Tri-gram Hidden Markov Model
  *
  * Provides emission and transition probabilities
  * based on frequencies from a tagged Corpus
  *
  * @param pp Preprocessor used to transform rare words
  */
class HiddenMarkovModel(pp: Preprocessor = PatternPreprocessor) {
  val ngramSize = 3 // Currently only transition probabilities using trigrams are supported
  val ngramCounts = Array.fill(ngramSize)(HashMap[List[String], Int]().withDefaultValue(0))
  val emissCounts = HashMap[TagTuple, Int]().withDefaultValue(0)
  val wordSet = new HashSet[String]()
  val symbSet = new HashSet[String]()

  /** Trains the model using provided corpus and rare word threshold
    *
    * @param corpus Corpus with tagged sentences to train on
    * @param rareThreshold max word frequency in corpus for rare preprocessing
    */
  def train(corpus: TaggedCorpus, rareThreshold: Int = 3) {
    wordSet ++= collectNonRares(corpus.getSentIter, rareThreshold)
    collectCounts(corpus.getSentIter)
    symbSet ++= ngramCounts(0).keySet.flatMap(x=>x)
  }

  /** Stores emission counts for symbol/word pairs and transition counts for symbol n-grams
    *
    * @param sentences tagged sentences to collect rule frequencies from
    */
  private def collectCounts(sentences: TaggedSentIter): Unit = {
    for (sent <- sentences) {
      val wordTuples = sent.map{t => preprocessTagForRares(t)}.toList
      // update emission and 1-gram counts
      wordTuples.foreach{ tuple =>
        emissCounts(tuple) += 1
        ngramCounts(0)(List(tuple.symb)) += 1
      }
      // prepend tuple list with n-1 START symbols and append STOP symbol
      val fullTuples = List.fill(ngramSize-1)(TagTuple("","*>")) ++ wordTuples ++ List(TagTuple("", "<*"))
      // get n-grams over tuple list and increment n-gram tag counts
      for (i <- 2 to ngramSize) {
        fullTuples.sliding(i).map(x => x.toList).foreach { ngram =>
          ngramCounts(i-1)(ngram.map(t => t.symb)) += 1
        }
      }
    }
  }

  /** Returns set of words (vocabulary) occurring in corpus > rareThreshold times
    *
    * @param sentences tagged sentences to collect non-rares from
    * @param rareThreshold max word frequency to be considered rare in corpus
    * @return set of non-rare words strings
    */
  private def collectNonRares(sentences: TaggedSentIter, rareThreshold: Int): Set[String] = {
    val wordCounts = new HashMap[String, Int]().withDefaultValue(0)
    sentences.foreach(s => s.foreach(t => wordCounts(t.word)+=1))
    wordCounts.retain((k,v) => v >= rareThreshold).keySet.toSet
  }

  /** Transforms word from provided TagTuple if not in vocabulary using this Class's preprocessor
    *
    * @param tag original TagTuple with possible rare word
    * @return new TagTuple containing transformed rare word string
    */
  private def preprocessTagForRares(tag: TagTuple): TagTuple = {
    if (wordSet(tag.word)) tag else TagTuple(pp.transform(tag.word), tag.symb)
  }

  /** Transforms provided word if not in vocabulary using this Class's preprocessor
    *
    * @param word original word string
    * @return transformed word string
    */
  private def preprocessStrForRares(word: String): String = {
    if (wordSet(word)) word else pp.transform(word)
  }

  /** Returns emission probability P(word | symbol)
    *
    *
    * @param word
    * @param symb
    * @return emission probability in interval [0,1]
    */
  def getEmissProb(word: String, symb: String): Double = {
    val wordCount = emissCounts(TagTuple(preprocessStrForRares(word),symb))
    val symbCount = ngramCounts(0)(List(symb))
    wordCount.toDouble / symbCount
  }

  /** Returns transition probability P(tag2 | tag0, tag1)
    *
    * @param symbList list of tag symbol
    * @return transition probability in interval [0,1]
    */
  def getTransProb(symbList: List[String]): Double = {
    // TODO: use weighted probabilities for n-grams up to n
    val tgramCount = ngramCounts(2)(symbList.take(3))
    val bgramCount = ngramCounts(1)(symbList.take(2))
    tgramCount.toDouble / bgramCount
  }

  /** Saves model to provided file as series of transition / emission rule frequencies
    *
    * @param file File object to write rules to
    */
  def save(file: File): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(file))
      emissCounts.toIterator.foreach { case (tt, count) =>
        writer.write(s"TAGTUPLE $count ${tt.word} ${tt.symb}\n")
      }
      ngramCounts.flatMap(x=>x).foreach { case (ngram, count) =>
        writer.write(s"N-GRAM $count ${ngram.mkString(" ")}\n")
      }
      writer.close()
    } catch {
      case e: FileNotFoundException => println(s"Could not find file ${file.getName}")
      case e: IOException => println(s"IOException processing file ${file.getName}")
    }
  }

  /** Loads model with rules from provided rule Document
    *
    * @param ruleDoc Document containing transition/emission rule frequencies
    */
  def load(ruleDoc: ResourceDocument): Unit = {
    try {
      wordSet.clear()
      symbSet.clear()
      ruleDoc.getLineIter.foreach(l => parseRule(l))
      wordSet ++= emissCounts.keySet.map(x=>x.word).toSet
      symbSet ++= ngramCounts(0).keySet.flatMap(x=>x)
    } catch {
      case e: IOException => println(s"IOException processing source ${ruleDoc.filename}")
    }
  }

  // LOAD HELPER METHODS
  // TODO: push rule file logic into seperate class similar to Corpus

  private def parseNGramRule(args: List[String]): Unit = {
    val (count, ngram) = args.splitAt(1)
    ngramCounts(ngram.length-1).update(ngram, count(0).toInt)
  }

  private def parseTagTupleRule(args: List[String]): Unit = {
    emissCounts.update(new TagTuple(args(1),args(2)), args(0).toInt)
  }

  private def parseRule(line: String): Unit = {
    try {
      val (_type, args) = line.split(" ").toList.splitAt(1)
      _type(0) match {
        case "TAGTUPLE" => parseTagTupleRule(args)
        case "N-GRAM" => parseNGramRule(args)
      }
    } catch {
      case e: Any => println(s"Error: Badly formed rule: $line")
    }
  }

}