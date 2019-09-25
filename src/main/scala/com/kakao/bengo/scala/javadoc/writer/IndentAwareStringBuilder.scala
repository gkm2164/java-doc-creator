package com.kakao.bengo.scala.javadoc.writer

class IndentAwareStringBuilder(initialIndent: Int, defaultTabString: String = "    ") {
  val sb: StringBuilder = StringBuilder.newBuilder
  private var currentIndent: Vector[Int] = Vector(initialIndent)
  private val tabString: String = defaultTabString

  def append[T](value: T): IndentAwareStringBuilder = {
    sb.append(value)
    this
  }

  def tab: IndentAwareStringBuilder = {
    currentIndent :+= (currentIndent.last + 1)
    this
  }

  def untab: IndentAwareStringBuilder = {
    currentIndent = currentIndent.dropRight(1)
    this
  }

  def set(newIndent: Int): IndentAwareStringBuilder = {
    currentIndent :+= newIndent
    this
  }

  def unset: IndentAwareStringBuilder = {
    currentIndent = currentIndent.dropRight(1)
    this
  }

  def enter(): IndentAwareStringBuilder = {
    sb.append("\n").append(tabString * currentIndent.last)
    this
  }

  override def toString: String = sb.toString()
}
  