package com.kakao.bengo.scala.javadoc

import levsha.Document.{Empty, Node}
import levsha.text.symbolDsl._

sealed trait CodeNode {
  def name: String

  def print[T]: Node[T]

  def buildNavTree[T]: Node[T]
}

case class CodeLeaf(name: String, packageName: String, tokens: List[JavaSToken]) extends CodeNode {
  lazy val code: JavaCode = JavaCode(tokens)

  override def print[T]: Node[T] = 'div ('class /= "panel",
    'hr (),
    'div (code.show)
  )

  override def buildNavTree[T]: Node[T] = {
    import com.kakao.bengo.javalang.JavaTokenEnum._
    def recur[U](definition: JavaDefinition): Node[U] = {
      definition match {
        case JavaClass(className, modifier, definitions, _, _) if modifier.access == PUBLIC =>
          'li ('a ('class /= "class-name", 'href /= s"#${definition.id}", 'b (className)),
            'ul (definitions.sortBy(_.name).map(recur))
          )
        case JavaEnumClass(enumClassName, modifier, _, definitions, _) if modifier.access == PUBLIC =>
          'li ('a ('class /= "class-name", 'href /= s"#${definition.id}", enumClassName),
            'ul (definitions.sortBy(_.name).map(recur))
          )
        case JavaInterface(interfaceName, modifier, definitions, _) if modifier.access == PUBLIC =>
          'li ('a ('href /= s"#${definition.id}", interfaceName),
            'ul (definitions.sortBy(_.name).map(recur))
          )
        case JavaAnnotationInterface(annotationInterfaceName, modifier, definitions, _) if modifier.access == PUBLIC =>
          'li ('a ('href /= s"#${definition.id}", annotationInterfaceName),
            'ul (definitions.sortBy(_.name).map(recur))
          )
        case JavaMethod(modifier, methodName, _, args) if modifier.access == PUBLIC =>
          'li ('a ('class /= "method-name", 'href /= s"#${definition.id}", s" $methodName(${args.map(x => escapeLTGT(x.name)).mkString(", ")})")
          )
        case JavaMember(modifier, memberName, _) if modifier.access == PUBLIC =>
          'li (
            'a ('class /= "member-name", 'href /= s"#${definition.id}", memberName)
          )
        case x =>
          println(s"nothing to do with ${x.name}")
          Empty
      }
    }

    code.defs.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => recur(x))
  }
}

case class CodeNonLeaf(name: String, codeNodes: Map[String, CodeNode]) extends CodeNode {

  override def print[T]: Node[T] = {
    codeNodes.toList.sortBy(_._1).map { case (_, node) =>
      node.print
    }
  }

  def drawNav[T](n: String): Node[T] = Seq(
    if (n != "") 'a ('class /= "package-name", 'href /= "#", n) else Empty,
    'ul (codeNodes.toList.sortBy(_._1).map { case (_, node) =>
      node match {
        case _: CodeNonLeaf => 'li (node.buildNavTree)
        case _: CodeLeaf => node.buildNavTree
      }
    }))

  def buildNavTreeAcc[T](prefix: String): Node[T] = {
    if (codeNodes.size == 1 && codeNodes.values.head.isInstanceOf[CodeNonLeaf])
      codeNodes.values.head.asInstanceOf[CodeNonLeaf].buildNavTreeAcc((if (prefix != "") prefix + "." else "") + name)
    else drawNav(prefix + "." + name)
  }

  override def buildNavTree[T]: Node[T] = {
    if (codeNodes.size == 1) buildNavTreeAcc(name)
    else drawNav(name)
  }
}
