package com.kakao.search.middle.javadoc

import java.io.PrintWriter

sealed trait CodeNode {
  def name: String
  def print(pw: PrintWriter): Unit
}

case class CodeLeaf(name: String, packageName: String, tokens: List[JavaSToken]) extends CodeNode {
  override def print(pw: PrintWriter): Unit = {
    pw.println(s"<p>at filename: $name, in package: $packageName</p>")
    pw.println("<div>" + analyze().show + "</div>")
  }

  def analyze(): JavaCode = JavaCode(tokens)
}

case class CodeNonLeaf(name: String, codeNodes: Map[String, CodeNode]) extends CodeNode {
  override def print(pw: PrintWriter): Unit = {
    codeNodes.foreach { case (childName, node) =>
      pw.println(s"<h2>=&gt; $childName</h2>")
      node.print(pw)
    }
  }
}