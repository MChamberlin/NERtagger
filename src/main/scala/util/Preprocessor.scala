package util

abstract class Preprocessor {
  def transform(word: String): String
}

object PatternPreprocessor extends Preprocessor {

  def transform(word: String): String = {
    val (first2, rest) = word.splitAt(2)
    var nword = "<" + first2.split("").map(x=>transformSubstring(x)).mkString
    if (rest.length > 1)
      nword ++= transformSubstring(rest.dropRight(1))
    nword ++ transformSubstring(rest.takeRight(1)) + ">"
  }

  protected def transformSubstring(str: String): String = {
    str.replaceAll("[A-Z]+","X").
      replaceAll("[a-z]+","x").
      replaceAll("[0-9]+","d").
      replaceAll("""[^\x00-\x7F]""", "x").
      distinct.sorted
  }

}

object ReplacePreprocessor extends Preprocessor {
  def transform(word: String): String = {
    "<RARE>"
  }
}

object ClassifyPreprocessor extends Preprocessor {
  def transform(word: String): String = {
    if ("[0-9]".r.findFirstIn(word).nonEmpty) {
      "<NUMERIC>"
    } else if ("^[A-Z]+$".r.findFirstIn(word).nonEmpty) {
      "<ALLCAP>"
    } else if ("[A-Z]$".r.findFirstIn(word).nonEmpty) {
      "<LASTCAP>"
    } else if ("^[A-Z]".r.findFirstIn(word).nonEmpty) {
      "<FIRSTCAP>"
    } else {
      "<RARE>"
    }
  }
}