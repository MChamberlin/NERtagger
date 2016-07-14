package tagger

import util._


protected class SymbTypeCounter {
  // TODO: deal with potential DivideByZero errors
  var truePos: Int = 0
  var trueNeg: Int = 0
  var falsePos: Int = 0
  var falseNeg: Int = 0

  def reset(): Unit = {
    truePos = 0
    trueNeg = 0
    falsePos = 0
    falseNeg = 0
  }

  def getPrecision: Double = {
    truePos / (truePos + falsePos).toDouble
  }

  def getRecall: Double = {
    truePos / (truePos + falseNeg).toDouble
  }

  def getAccuracy: Double = {
    (truePos + trueNeg) / getTotalCounts.toDouble
  }

  def getF1Score: Double = {
    (2 * getPrecision * getRecall) / (getPrecision + getRecall)
  }

  def getTotalCounts: Int = {
    truePos + falsePos + trueNeg + falseNeg
  }

  def printScores(): Unit = {
    println(f"Accuracy  | $getAccuracy%.4f")
    println(f"Precision | $getPrecision%.4f")
    println(f"Recall    | $getRecall%.4f")
    println(f"F1-Score  | $getF1Score%.4f")
  }

}

class TaggerEvaluator(tagger: Tagger) {
  val totalCounter = new SymbTypeCounter

  private def collectCounts(devDoc: Document, keyDoc: TaggedCorpus): Unit = {
    // TODO: allow counting while writing tags so inference is done only once
    // TODO: handle multiple classes using the SymbTypeCounts class
    totalCounter.reset()
    devDoc.getSentIter.zip(keyDoc.getSentIter).foreach{ case (sent, trueTags) =>
      tagger.getSentenceTags(sent.toList).zip(trueTags.toList).foreach{ case (predTag,trueTag) =>
        if (predTag.word != trueTag.word) {
          println(s"ERROR: word mismatch (${predTag.word} != ${trueTag.word})")
        }
        if (keyDoc.posSymbSet(trueTag.symb)) {
          if (predTag.symb == trueTag.symb) {
            totalCounter.truePos += 1
          } else {
            totalCounter.falseNeg += 1
          }
        } else {
          if (predTag.symb == trueTag.symb) {
            totalCounter.trueNeg += 1
          } else {
            totalCounter.falsePos += 1
          }
        }
      }
    }
  }

  def score(devDoc: Document, keyDoc: TaggedCorpus): Unit = {
    collectCounts(devDoc, keyDoc)
    println("Total Scores")
    println("-"*18)
    totalCounter.printScores()
  }

}
