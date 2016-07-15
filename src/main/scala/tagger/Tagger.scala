package tagger

import java.io._

import hmm.HiddenMarkovModel
import util._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet}

/** Class for tagging named entities in sentences and documents using Hidden Markov Model
 *
 * @param preprocessor rare word preprocessor
 */
class Tagger(preprocessor: Preprocessor = PatternPreprocessor) {
  val model = new HiddenMarkovModel(preprocessor)

  /** Trains underlying HMM using provided corpus and rare word threshold
   *
   * @param trainingCorpus Corpus with tagged sentences to train on
   * @param rareThreshold max word frequency in corpus for rare preprocessing
   */
  def train(trainingCorpus: TaggedCorpus, rareThreshold: Int): Unit = {
    model.train(trainingCorpus, rareThreshold)
  }

  /** Loads underlying HMM with rules from provided rule Document
   *
   * @param ruleDoc Document containing transition/emission rule frequencies
   */
  def load(ruleDoc: Document): Unit = {
    model.load(ruleDoc)
  }

  /** Saves underlying HMM model to provided file as series of rule frequencies
    *
    * @param file File object to write rules to
    */
  def save(file: File): Unit = {
    model.save(file)
  }

  /** Returns inferred symbol tags for provided sentence tokens
    *
    * Performs inference using the Viterbi algorithm
    *
    * @param tokens a list of word strings
    * @return a list of symbol tag strings
    */
  protected def getSymbols(tokens: List[String]): List[String] = {
    // TODO: comment; more informative variable names?
    // initialize probabilities and back-pointer maps
    val bp = HashMap.empty[Int,HashMap[String,HashMap[String,String]]]
    val pi = HashMap.empty[Int,HashMap[String,HashMap[String,Double]]]
    pi(0) = HashMap("*>"->HashMap("*>"->1.0))

    // define allowed symbol set for each index into sentence
    def ss(index: Int) = index match {
      case -1 | 0 => HashSet("*>")
      case _ => model.symbSet
    }

    val n = tokens.length

    var q = -1.0 // trans probability -> P(tag2 | tag0, tag1)
    var e = -1.0 // emiss probability -> P(word | tag2)
    var p = -1.0 // total probability
    for (k <- 1 to n) {
      val word = tokens(k-1)
      pi(k) = HashMap.empty[String,HashMap[String,Double]]
      bp(k) = HashMap.empty[String,HashMap[String,String]]
      ss(k-1).foreach { u =>
        pi(k)(u) = HashMap.empty[String,Double]
        bp(k)(u) = HashMap.empty[String,String]
        ss(k).foreach { v =>
          var bestProb = -1.0
          var bestSymb = ""
          ss(k-2).foreach { w =>
            q = model.getTransProb(List(w,u,v))
            e = model.getEmissProb(word, v)
            p = pi(k-1)(w)(u) * q * e
            if (p > bestProb) {
              bestProb = p
              bestSymb = w
            }
          }
          pi(k)(u)(v) = bestProb
          bp(k)(u)(v) = bestSymb
        }
      }
    }
    val tags = Array.fill(n+1)("")
    var bestProb = -1.0
    var bestU = ""
    var bestV = ""
    ss(n).foreach { u =>
      ss(n-1).foreach { v =>
        q = model.getTransProb(List(u,v,"<*"))
        p = pi(n)(u)(v) * q
        if (p > bestProb) {
          bestProb = p
          bestU = u
          bestV = v
        }
      }
    }
    tags(n-1) = bestU
    tags(n)   = bestV
    for (k <- n-2 to 0 by -1) {
      tags(k) = bp(k+2)(tags(k+1))(tags(k+2))
    }
    tags.toList.drop(1)
  }

  /** Returns word/symbol TagTuple iterator for provided sentence tokens
   *
   * @param tokens A list of word strings
   * @return Iterator of TagTuples with inferred symbols
   */
  def getSentenceTags(tokens: List[String]): TagIter = {
    getSymbols(tokens).zip(tokens).map{case (symb, word) => new TagTuple(word,symb)}.toIterator
  }

  /** Returns iterator of tagged sentences for provided document
    *
    * @param doc document with iterator of sentences to tag
    * @return iterator of tagged sentences with inferred symbols
    */
  def getDocumentTags(doc: Document): TaggedSentIter = {
    for (sent <- doc.getSentIter)
      yield getSentenceTags(sent.toList)
  }

  /** Writes tagged sentences to provided file
    *
    * Tags sentences and writes them to output file using
    * DocFormatter to transform tagged sentence iterator into strings
    *
    * @param sentIter
    * @param outFile
    * @param formatter
    */
  def writeTags(sentIter: TaggedSentIter, outFile: File, formatter: DocFormatter = BarSepDocFormatter): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(outFile))
      sentIter.foreach( tagIter => writer.write(formatter.formatSentenceTags(tagIter)) )
      writer.close()
    } catch {
      case e: IOException => println(s"IOException processing file ${outFile.getName}")
    }
  }

  /** Writes tagged sentences from provided Document to output File
    *
    * Tags sentences from provided Document object and writes them to outFile
    * using DocFormatter to transform tagged sentence iterator into strings
    *
    * @param doc
    * @param outFile
    * @param formatter
    */
  def writeDocumentTags(doc: Document, outFile: File, formatter: DocFormatter = BarSepDocFormatter): Unit = {
    writeTags(getDocumentTags(doc), outFile, formatter)
  }

}
