package tagger

import java.io._

import hmm.HiddenMarkovModel
import util._

import scala.collection.mutable.{HashMap, HashSet}


class Tagger(trainingCorpus: TaggedCorpus,
             rareThreshold: Int = 5,
             maxNGramSize: Int = 3,
             preprocessor: Preprocessor = PatternPreprocessor) {
  protected val model = new HiddenMarkovModel(maxNGramSize, preprocessor)
  model.train(trainingCorpus, rareThreshold)

  protected def viterbi(tokens: List[String]): List[String] = {
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
          // println(s"pi($k)($u)($v) = $bestProb | $bestSymb")
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

  def getSentenceTags(sent: List[String]): List[TagTuple] = {
    viterbi(sent).zip(sent).map{case (symb, word) => new TagTuple(word,symb)}
  }

  def getDocumentTags(doc: Document): Iterator[List[TagTuple]] = {
    for (sent <- doc.getSentIter)
      yield getSentenceTags(sent.toList)
  }

  def writeDocumentTags(doc: Document, outFile: File): Unit = {
    // TODO: method should take an output document formatter along with outFile?
    try {
      val writer = new BufferedWriter(new FileWriter(outFile))
      getDocumentTags(doc).foreach{ sentIter =>
        writer.write(sentIter.map(t => s"${t.word}|${t.word}").mkString(" ") + "\n")
      }
      writer.close()
    } catch {
      case e: IOException => println(s"IOException processing file ${outFile.getName}")
    }
  }

}
