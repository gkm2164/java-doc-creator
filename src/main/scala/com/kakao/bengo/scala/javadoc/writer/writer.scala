package com.kakao.bengo.scala.javadoc

import levsha.Document.{Empty, Node}
import levsha.impl.TextPrettyPrintingConfig
import levsha.text.renderHtml

package object writer {

  trait StringWriter[T] {
    def show(indent: Indent): T
  }


  implicit class DefToNodeWriter[T](definition: JavaDefinition) extends StringWriter[Node[T]] {

    import com.kakao.bengo.javalang.JavaTokenEnum._
    import levsha.text.symbolDsl._

    def attachTo(x: String): String = {
      x.split("\\.").last match {
        case "METHOD" => "함수에 표시"
        case "PARAMETER" => "파라미터에 표시"
        case _ => ""
      }
    }

    override def show(indent: Indent): Node[T] =
      'div ('id /= s"def-${definition.id}", 'class /= "definition-box", 'style /= s"margin-left: ${indent.ch()}px;",
        'a ('id /= definition.id), definition match {
          case c@JavaClass(name, modifier, definitions, inheritClass, implementInterfaces) =>
            'div (if (c.representName != name) 'h6 ('class /= "class-role", c.representName) else Empty,
              'h5 ('class /= "java-def class-def",
                s" $name",
                if (modifier.generic.nonEmpty) 'span ('span ('class /= "generic-symbol", "&lt;"), modifier.generic.map(escapeLTGT), 'span ('class /= "generic-symbol", "&gt;")) else Empty),
              if (inheritClass.nonEmpty) 'span ('span ('class /= "reserved-keyword", " extends "), inheritClass.map(x => x.show).mkString(", "))
              else Empty,
              if (implementInterfaces.nonEmpty) 'span ('span ('class /= "reserved-keyword", " implements "), implementInterfaces.map(x => x.show).mkString(", "))
              else Empty,
              'p (modifier.commentMacros.filter(_.startsWith("//!")).map(_.drop(3)).mkString("\n")),
              if (c.exampleCodes.nonEmpty) 'pre ('code ('class /= "java", modifier.commentMacros.filter(_.startsWith("//=")).map(_.drop(3)).mkString("\n")))
              else Empty,
              'div (definitions.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => x.show(indent.inc))))
          case JavaEnumClass(name, modifier, enumTokens, definitions, implements) =>
            'div ('h4 ('class /= "java-def", name),
              'b ("enum values"),
              'ul ('class /= "enum-values java-def", enumTokens.map(x => 'li (x))),
              'div (definitions.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => x.show(indent.inc))))

          case JavaInterface(name, modifier, childDefinitions, inheritClass) =>
            'div ('h5 ('class /= "java-def interface-def", s" $name"),
              if (inheritClass.nonEmpty) 'div ('b ("inherit classes"), 'ul (inheritClass.map(x => 'li (x.show)))) else Empty)

          case a@JavaAnnotationInterface(name, modifier, definitions, inheritClass) =>
            'div ('h5 ('class /= "java-def annotation-use", s"@$name"),
              a.decideTarget.map(x => 'div(attachTo(x))).getOrElse(Empty),
              'div (definitions.map(_.show(indent.inc))))

          case m@JavaMethod(modifier, name, returnType, args) =>
            'div ('span ('class /= "full-path", modifier.fullPath),
              'p ('class /= "java-def", modifier.show, returnType.show,
                if (name == "" || name == returnType.show) "" else 'span ('class /= "method-name", s" $name"), "(",
                args.map(x => renderHtml(x.show, TextPrettyPrintingConfig.noPrettyPrinting)).mkString(", "), ")"),
              'span (modifier.commentMacros.filter(_.startsWith("//!")).map(_.drop(3)).mkString("\n")),
              if (m.exampleCode.nonEmpty) 'pre ('code ('class /= "java", modifier.commentMacros.filter(_.startsWith("//=")).map(_.drop(3)).mkString("\n"))) else Empty)

          case JavaMember(modifier, name, memberType) =>
            'div ('span ('class /= "full-path", modifier.fullPath),
              'p ('class /= "java-def", modifier.show, memberType.show, " ", 'span ('class /= "member-name", name)),
              'pre ('class /= "code", modifier.commentMacros.filter(_.startsWith("//=")).map(_.drop(3)).mkString("\n")))
          case _ => ""
        })

    private def putIf(cond: => Boolean, t: Node[T]): Node[T] = if (cond) t else Empty
  }

}