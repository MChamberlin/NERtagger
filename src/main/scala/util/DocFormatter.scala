package util


abstract class DocFormatter {
  def formatSentenceTags(sent: TagIter): String
}

object BarSepDocFormatter extends DocFormatter {
  def formatSentenceTags(sent: TagIter): String = {
    sent.map(t => s"${t.word}|${t.symb}").mkString(" ") + "\n"
  }
}
