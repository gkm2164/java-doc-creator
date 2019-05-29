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

  override def print[T]: Node[T] =
    'div (
      'hr (),
      'p (s"at file name: $name, in package: $packageName"),
      'div (code.show)
    )

  override def buildNavTree[T]: Node[T] = {
    import com.kakao.bengo.javalang.JavaTokenEnum._
    def recur[U](definition: JavaDefinition): Node[U] = {
      definition match {
        case JavaClass(className, modifier, definitions, _, _) if modifier.access == PUBLIC =>
          'li (
            'a ('href /= s"#${definition.id}",
              'b (className)
            ),
            if (definitions.nonEmpty) 'ul (definitions.map(recur)) else Empty
          )
        case JavaEnumClass(enumClassName, modifier, _, definitions, _) if modifier.access == PUBLIC =>
          'li (
            'a ('href /= s"#${definition.id}", enumClassName),
            if (definitions.nonEmpty) 'ul (definitions.map(recur)) else Empty
          )
        case JavaInterface(interfaceName, modifier, definitions, _) if modifier.access == PUBLIC =>
          'li (
            'a ('href /= s"#${definition.id}", interfaceName),
            if (definitions.nonEmpty) 'ul (definitions.map(recur)) else Empty
          )
        case JavaAnnotationInterface(annotationInterfaceName, modifier, definitions, _) if modifier.access == PUBLIC =>
          'li (
            'a ('href /= s"#${definition.id}", annotationInterfaceName),
            if (definitions.nonEmpty)
              'ul (definitions.map(recur))
            else
              Empty
          )
        case JavaMethod(modifier, methodName, _, args) if modifier.access == PUBLIC =>
          'li (
            'a ('href /= s"#${definition.id}", s"$methodName(${args.map(x => escapeLTGT(x.name)).mkString(", ")})")
          )
        case JavaMember(modifier, memberName, _) if modifier.access == PUBLIC =>
          'li (
            'a ('href /= s"#${definition.id}", memberName)
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

  override def buildNavTree[T]: Node[T] = {
    'div (
      if (name != "") 'p (name) else Empty,
      'ul ('class /= "non-leaf",
        codeNodes.toList.sortBy(_._1).map { case (_, node) =>
          node match {
            case _: CodeNonLeaf => 'li ('class /= "non-leaf", node.buildNavTree)
            case _: CodeLeaf => node.buildNavTree
          }
        }
      )
    )
  }
}
