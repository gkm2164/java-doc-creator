package com.kakao.bengo.scala.javadoc.writer

case class Line(indent: Int, line: String)

class IndentAwareStringBuilder(initialIndent: Int, defaultTabString: String = "    ") {
  val sb: StringBuilder = StringBuilder.newBuilder
  private var committedLines: Vector[Line] = Vector()
  private var indentStack: Vector[Int] = Vector(initialIndent)
  private val tabString: String = defaultTabString

  private def currentIndent: Int = indentStack.last

  def this(prev: IndentAwareStringBuilder) {
    this(prev.currentIndent, prev.tabString)
    this.sb.append(prev.sb.toString())
    this.committedLines = prev.committedLines
    this.indentStack = prev.indentStack
  }

  def append[T](value: T): IndentAwareStringBuilder = {
    sb.append(value)
    this
  }

  def overwrite(newSB: IndentAwareStringBuilder): IndentAwareStringBuilder = {
    sb.clear()
    println(s"Overwrite: [${newSB.sb.toString()}]")
    sb.append(newSB.sb.toString)
    this.committedLines = newSB.committedLines
    this.indentStack = newSB.indentStack
    this
  }

  def tab: IndentAwareStringBuilder = {
    indentStack :+= (indentStack.last + 1)
    this
  }

  def untab: IndentAwareStringBuilder = {
    indentStack = indentStack.dropRight(1)
    this
  }

  def set(newIndent: Int): IndentAwareStringBuilder = {
    indentStack :+= newIndent
    this
  }

  def unset: IndentAwareStringBuilder = {
    indentStack = indentStack.dropRight(1)
    this
  }

  def enter(): IndentAwareStringBuilder = {
    committedLines :+= Line(currentIndent, sb.toString())
    sb.clear()
    this
  }

  override def toString: String = {
    if (sb.length > 0) {
      enter()
    }

    committedLines.map { case Line(indent, line) => s"${tabString * indent}$line" }.mkString("\n")
  }
}

