package co.gyeongmin.lang.javadoc.codeformatter

case class Line(indent: Int, line: String)

object IndentAwareStringBuilder {
  def apply(initialIndent: Int): IndentAwareStringBuilder =
    IndentAwareStringBuilder(
      initialIndent,
      "    ",
      Vector(),
      Vector(),
      List(initialIndent)
    )
}

// This class is immutable
case class IndentAwareStringBuilder(
    initialIndent: Int,
    defaultTabString: String,
    stringBuilder: Vector[String],
    committedLines: Vector[Line] = Vector(),
    indentHistory: List[Int] = Nil
) {

  def append[T](value: T): IndentAwareStringBuilder = {
    val str = value.toString
    val tobe = str match {
      case "<"   => "&lt;"
      case ">"   => "&gt;"
      case "<<"  => "&lt;" * 2
      case ">>"  => "&gt;" * 2
      case ">>>" => "&gt;" * 3
      case _     => str
    }

    this.copy(stringBuilder = stringBuilder :+ tobe)
  }

  def tab: IndentAwareStringBuilder = {
    this.copy(indentHistory = (indentHistory.head + 1) :: indentHistory)
  }

  def untab: IndentAwareStringBuilder = {
    this.copy(indentHistory = indentHistory.drop(1))
  }

  def set(newIndent: Int): IndentAwareStringBuilder = {
    this.copy(indentHistory = newIndent :: indentHistory)
  }

  def unset: IndentAwareStringBuilder = {
    this.copy(indentHistory = indentHistory.drop(1))
  }

  def enter(): IndentAwareStringBuilder = {
    val nextLine = Line(currentIndent, stringBuilder.mkString(""))
    this.copy(
      stringBuilder = Vector(),
      committedLines = committedLines :+ nextLine
    )
  }

  private def currentIndent: Int = indentHistory.head

  def debugStringBuilder(last: Int): String = {
    val thisStringBuilder =
      if (stringBuilder.nonEmpty) {
        enter()
      } else {
        this
      }

    thisStringBuilder.committedLines
      .takeRight(last)
      .map { case Line(indent, line) =>
        s"${defaultTabString * indent}$line"
      }
      .mkString("\n")
  }

  override def toString: String = {
    val thisStringBuilder =
      if (stringBuilder.nonEmpty) {
        enter()
      } else {
        this
      }

    thisStringBuilder.committedLines
      .map { case Line(indent, line) =>
        s"${defaultTabString * indent}$line"
      }
      .mkString("\n")
  }
}
