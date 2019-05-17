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
        case JavaClass(className, modifier, definitions, _, _) if modifier.access == PUBLIC =>
          pw.write("<li>")
          pw.write(s"""<a href="#${definition.id}"><b>$className</b></a>""")
          if (definitions.nonEmpty) {
            pw.write("<ul>")
            definitions.foreach(recur)
            pw.write("</ul>")
          }
          pw.write("</li>")

        case JavaEnumClass(enumClassName, modifier, _, definitions) if modifier.access == PUBLIC =>
          pw.write("<li>")
          pw.write(s"""<a href="#${definition.id}">$enumClassName</a>""")
          if (definitions.nonEmpty) {
            pw.write("<ul>")
            definitions.foreach(recur)
            pw.write("</ul>")
          }
          pw.write("</li>")

        case JavaInterface(interfaceName, modifier, definitions, _) if modifier.access == PUBLIC =>
          pw.write("<li>")
          pw.write(s"""<a href="#${definition.id}">$interfaceName</a>""")
          if (definitions.nonEmpty) {
            pw.write("<ul>")
            definitions.foreach(recur)
            pw.write("</ul>")
          }
          pw.write("</li>")

        case JavaAnnotationInterface(annotationInterfaceName, modifier, definitions, _) if definitions.nonEmpty && modifier.access == PUBLIC =>
          pw.write("<li>")
          pw.write(s"""<a href="#${definition.id}">$annotationInterfaceName</a>""")
          if (definitions.nonEmpty) {
            pw.write("<ul>")
            definitions.foreach(recur)
            pw.write("</ul>")
          }
          pw.write("</li>")

        case JavaMethod(modifier, methodName, _, args) if modifier.access == PUBLIC =>
          pw.write("<li>")
//          pw.write(s"""<a href="#${definition.id}">$methodName(${args.map(x => escapeLTGT(x.name)).mkString(", ")})</a>""")
          pw.write(s"""<a href="#${definition.id}">$methodName(${args.map(x => escapeLTGT(x.name)).mkString(", ")})</a>""")

          pw.write("</li>")

        case JavaMember(modifier, memberName, _) if modifier.access == PUBLIC =>
          pw.write("<li>")
          pw.write(s"""<a href="#${definition.id}">$memberName</a>""")
          pw.write("</li>")

        case x => println(s"nothing to do with ${x.name}")
      }
    }

    code.defs.filter(_.modifier.access != PRIVATE).sortBy(_.name).foreach(recur)
  }
}

case class CodeNonLeaf(name: String, codeNodes: Map[String, CodeNode]) extends CodeNode {

  override def print(pw: PrintWriter): Unit = {
    codeNodes.toList.sortBy(_._1).foreach { case (_, node) =>
      node.print(pw)
    }
  }

  override def buildNavTree(pw: PrintWriter): Unit = {
    if (name != "") pw.write(s"<p>$name.</p>")
    pw.write("<ul class=\"non-leaf\">")

    codeNodes.toList.sortBy(_._1).foreach { case (_, node) =>
      node match {
        case _: CodeNonLeaf =>
          pw.write(s"""<li class="non-leaf">""")
          node.buildNavTree(pw)
          pw.write("</li>")
        case _: CodeLeaf =>
          node.buildNavTree(pw)
      }


    }
    pw.write("</ul>")
  }
}
