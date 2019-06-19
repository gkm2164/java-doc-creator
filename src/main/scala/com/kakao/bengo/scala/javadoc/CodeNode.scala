package com.kakao.bengo.scala.javadoc

import com.typesafe.scalalogging.Logger
import levsha.Document.{Empty, Node}
import levsha.text.symbolDsl._

sealed trait CodeNode {
  def name: String

  def print[T]: Node[T]

  def buildNavTree[T]: Node[T]

  def createHashMap: Map[String, List[JavaDefinition]]
}

case class CodeLeaf(name: String, packageName: String, tokens: List[JavaSToken]) extends CodeNode {
  val log: Logger = Logger("CodeLeaf")
  val code: JavaCode = JavaCode(tokens)

  override def print[T]: Node[T] = 'div ('class /= "panel",
    'hr (),
    'div (code.show)
  )

  override def buildNavTree[T]: Node[T] = {
    import com.kakao.bengo.javalang.JavaTokenEnum._
    def recur[U](definition: JavaDefinition): Node[U] = {
      definition match {
        case JavaClass(_, modifier, definitions, _, _) if modifier.access == PUBLIC =>
          'li ('span ('class /= "class-name", 'onclick /= s"highlightBlock('${definition.id}')", 'b (definition.representName)),
            'ul (definitions.sortBy(_.name).map(recur)))
        case JavaEnumClass(_, modifier, _, definitions, _) if modifier.access == PUBLIC =>
          'li ('span ('class /= "class-name enum-class", 'onclick /= s"highlightBlock('${definition.id}')",  definition.representName),
            'ul (definitions.sortBy(_.name).map(recur)))
        case JavaInterface(_, modifier, definitions, _) if modifier.access == PUBLIC =>
          'li ('span ('class /= "class-name", 'onclick /= s"highlightBlock('${definition.id}')", definition.representName),
            'ul (definitions.sortBy(_.name).map(recur)))
        case JavaAnnotationInterface(annotationInterfaceName, modifier, definitions, _) if modifier.access == PUBLIC =>
          'li ('span ('class /= "class-name", 'onclick /= s"highlightBlock('${definition.id}')", s"@$annotationInterfaceName"),
            'ul (definitions.sortBy(_.name).map(recur)))
        case JavaMethod(modifier, methodName, _, args) if modifier.access == PUBLIC =>
          'li ('span ('class /= "method-name", 'onclick /= s"highlightBlock('${definition.id}')", s" $methodName(${args.map(x => escapeLTGT(x.name)).mkString(", ")})"))
        case JavaMember(modifier, memberName, _) if modifier.access == PUBLIC =>
          'li ('span ('class /= "member-name", 'onclick /= s"highlightBlock('${definition.id}')", memberName))
        case x =>
          log.debug(s"nothing to do with ${x.name}")
          Empty
      }
    }

    code.defs.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => recur(x))
  }

  override def createHashMap: Map[String, List[JavaDefinition]] = code.defs.map(x => x.name -> x).groupBy(_._1).mapValues(_.map(_._2).toList)
}

case class CodeNonLeaf(name: String, codeNodes: Map[String, CodeNode]) extends CodeNode {
  val log = Logger("CodeNonLeaf")
  def createHashMap: Map[String, List[JavaDefinition]] = {
    val map: Map[String, Map[String, List[JavaDefinition]]] = codeNodes.mapValues(_.createHashMap)
    map.foldLeft(Map.empty[String, List[JavaDefinition]]) { case (acc, (_, elem)) =>
        elem.foldLeft(acc) { (acc, m) => acc + ((if (name != "") name + "." else "") + m._1 -> m._2) }
    }
  }

  override def print[T]: Node[T] = {
    codeNodes.toList.sortBy(_._1).map { case (_, node) =>
      node.print
    }
  }

  def drawNav[T](n: String): Node[T] = 'li(
    if (n != "") 'a ('class /= "package-name", 'href /= "#", n) else Empty,
    'ul (codeNodes.toList.sortBy(_._1).map { case (_, node) =>
      node match {
        case _: CodeNonLeaf => node.buildNavTree
        case _: CodeLeaf => node.buildNavTree
      }
    }))

  def buildNavTreeAcc[T](prefix: String): Node[T] = {
    if (codeNodes.size == 1 && codeNodes.values.head.isInstanceOf[CodeNonLeaf])
      codeNodes.values.headOption.map {
        case x: CodeNonLeaf => x.buildNavTreeAcc((if (prefix != "") prefix + "." else "") + name)
        case _ => Empty
      }.getOrElse(Empty)
    else drawNav(prefix + "." + name)
  }

  override def buildNavTree[T]: Node[T] = {
    if (codeNodes.size == 1) buildNavTreeAcc(name)
    else drawNav(name)
  }
}
