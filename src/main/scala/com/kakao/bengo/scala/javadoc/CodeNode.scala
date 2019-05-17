package com.kakao.bengo.scala.javadoc

import java.io.PrintWriter

sealed trait CodeNode {
  def name: String

  def print(pw: PrintWriter): Unit

  def buildNavTree(pw: PrintWriter)
}

case class CodeLeaf(name: String, packageName: String, tokens: List[JavaSToken]) extends CodeNode {
  lazy val code: JavaCode = JavaCode(tokens)

  override def print(pw: PrintWriter): Unit = {
    pw.println("<hr />")
    pw.println(s"<p>at filename: $name, in package: $packageName</p>")
    pw.println(s"<div>${code.show}</div>")
  }

  override def buildNavTree(pw: PrintWriter): Unit = {
    import com.kakao.bengo.javalang.JavaTokenEnum._
    def recur(definition: JavaDefinition): Unit = {
      definition match {
        case JavaClass(className, modifier, definitions, _, _) if modifier.access != PRIVATE =>
          pw.write("<li>")
          pw.write(s"<p>$className</p>")
          pw.write("<ul>")
          definitions.foreach(recur)
          pw.write("</ul>")
          pw.write("</li>")

        case JavaEnumClass(enumClassName, modifier, _, definitions) if modifier.access != PRIVATE =>
          pw.write("<li>")
          pw.write(s"<p>$enumClassName</p>")
          pw.write("<ul>")
          definitions.foreach(recur)

          pw.write("</ul>")
          pw.write("</li>")

        case JavaInterface(interfaceName, modifier, definitions, _) if modifier.access != PRIVATE =>
          pw.write("<li>")
          pw.write(s"<p>$interfaceName</p>")
          pw.write("<ul>")
          definitions.foreach(recur)
          pw.write("</ul>")
          pw.write("</li>")

        case JavaAnnotationInterface(annotationInterfaceName, modifier, definitions, _) if definitions.nonEmpty && modifier.access != PRIVATE =>
          pw.write("<li>")
          pw.write(s"<p>$annotationInterfaceName</p>")
          pw.write("<ul>")
          definitions.foreach(recur)
          pw.write("</ul>")
          pw.write("</li>")

        case JavaMethod(modifier, methodName, returnType, _) if modifier.access != PRIVATE =>
          pw.write("<li>")
          pw.write(s"<p>$methodName(): $returnType</p>")
          pw.write("</li>")

        case JavaMember(modifier, memberName, memberType) if modifier.access != PRIVATE =>
          pw.write("<li>")
          pw.write(s"<p>$memberName: $memberType</p>")
          pw.write("</li>")

        case x => println(s"nothing to do with ${x.name}")
      }
    }

    pw.write(s"<p>$name</p>")
    pw.write(s"<ul>")
    code.defs.filter(_.modifier.access != PRIVATE).foreach(recur)
    pw.write("</ul>")
  }
}

case class CodeNonLeaf(name: String, codeNodes: Map[String, CodeNode]) extends CodeNode {

  override def print(pw: PrintWriter): Unit = {
    codeNodes.foreach { case (childNode, node) =>
      println(s"printin $childNode")
      node.print(pw)
    }
  }

  override def buildNavTree(pw: PrintWriter): Unit = {
    pw.write(s"<p>$name</p>")
    pw.write("<ul>")
    codeNodes.toList.sortBy(_._1).foreach { case (_, node) =>
      pw.write("<li>")
      node.buildNavTree(pw)
      pw.write("</li>")
    }
    pw.write("</ul>")
  }
}
