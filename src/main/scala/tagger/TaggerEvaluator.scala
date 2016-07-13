package tagger

import util._

import scala.collection.mutable.HashMap


protected class SymbTypeCounter {
  var truePos: Int = 0
  var trueNeg: Int = 0
  var falsePos: Int = 0
  var falseNeg: Int = 0

  def getPrecision: Double = {
    truePos / (truePos + falsePos).toDouble
  }

  def getRecall: Double = {
    truePos / (truePos + falseNeg).toDouble
  }

  def getAccuracy: Double = {
    (truePos + trueNeg) / getTotalCounts.toDouble
  }

  def getTotalCounts: Int = {
    truePos + falsePos + trueNeg + falseNeg
  }
}

class TaggerEvaluator(tagger: Tagger) {
  var truePos: Int = 0
  var trueNeg: Int = 0
  var falsePos: Int = 0
  var falseNeg: Int = 0

  def collectCounts(devDoc: Document, keyDoc: TaggedCorpus): Unit = {
    val symbCounts = new HashMap[String,SymbTypeCounter]().withDefaultValue(new SymbTypeCounter)
    devDoc.getSentIter.zip(keyDoc.getSentIter).foreach{ case (sent, trueTags) =>
      tagger.getSentenceTags(sent.toList).zip(trueTags.toList).foreach{ case (predTag,trueTag) =>
        // TODO: handle multiple classes using the SymbTypeCounts class
        if (predTag.word != trueTag.word) {
          println(s"ERROR: word mismatch (${predTag.word} != ${trueTag.word})")
        }
        if (keyDoc.posSymbSet(trueTag.symb)) {
          if (predTag.symb == trueTag.symb) {
            truePos += 1
          } else {
            falseNeg += 1
          }
        } else {
          if (predTag.symb == trueTag.symb) {
            trueNeg += 1
          } else {
            falsePos += 1
          }
        }
      }
    }
  }

  def printScores: Unit = {
    // TODO: deal with potential DivideByZero errors
    val accuracy = (truePos + trueNeg) / (truePos + trueNeg + falsePos + falseNeg).toDouble
    val precision = truePos / (truePos + falsePos).toDouble
    val recall = truePos / (truePos + falseNeg).toDouble
    val f1Score = (2 * precision * recall) / (precision + recall)
    println(s"Accuracy  | ${accuracy}")
    println(s"Precision | ${precision}")
    println(s"Recall    | ${recall}")
    println(s"F1-Score  | ${f1Score}")
  }

  def score(devDoc: Document, keyDoc: TaggedCorpus): Unit = {
    truePos = 0
    trueNeg = 0
    falsePos = 0
    falseNeg = 0
    collectCounts(devDoc, keyDoc)
    printScores
  }

}
