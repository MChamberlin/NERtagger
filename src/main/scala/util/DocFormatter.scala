package util


abstract class DocFormatter {
  def formatSentenceTags(sent: TagIter): String

  def formatDocumentTags(doc: TaggedSentIter): Iterator[String] = {
    for (sent <- doc if sent.nonEmpty)
      yield formatSentenceTags(sent)
  }
}

object BarSepDocFormatter extends DocFormatter {
  def formatSentenceTags(sent: TagIter): String = {
    sent.map(t => s"${t.word}|${t.symb}").mkString(" ")
  }
}
