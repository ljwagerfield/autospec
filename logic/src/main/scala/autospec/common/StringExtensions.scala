package autospec.common

object StringExtensions {

  implicit class RichString(val value: String) extends AnyVal {

    /**
      * NOTE: adds a space ' ' where the line breaks once were.
      */
    def stripMarginAndLineBreaks: String =
      value.stripLineEnd.stripMargin.linesIterator.mkString(" ")

  }

}
