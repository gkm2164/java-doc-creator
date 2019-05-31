package com.kakao.bengo.scala

import com.kakao.bengo.javalang.JavaTokenEnum
import levsha.Document.{Empty, Node}
import levsha.impl.TextPrettyPrintingConfig
import levsha.text.symbolDsl._

import scala.annotation.tailrec
import scala.util.Random

package object javadoc {

  import com.kakao.bengo.javalang.JavaTokenEnum._

  val ASCIIValues: IndexedSeq[Char] = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')

  def escapeLTGT(str: String): String = { // 문서 상에 <, > 부호가 안보이는 경우...
    @tailrec
    def loop(str: String, acc: String): String = {
      if (str.isEmpty) acc
      else if (str.head == '<') loop(str.tail, acc + """<span class="generic-symbol">&lt;</span>""")
      else if (str.head == '>') loop(str.tail, acc + """<span class="generic-symbol">&gt;</span>""")
      else loop(str.tail, acc + str.head)
    }

    loop(str, "")
  }

  sealed trait JavaDefinition {
    def id: String

    def name: String

    def show[T](indenter: Indenter): Node[T]

    def modifier: JavaModifier
  }

  sealed trait JavaTypeDef extends JavaDefinition {
    def inheritClass: List[JavaTypeUse]

    def implementInterfaces: List[JavaTypeUse]
  }

  trait JavaMembers extends JavaDefinition

  case class Indenter(indent: Int, tab: String = "&nbsp;&nbsp;") {

    implicit class StringExtend(s: String) {
      def repeat(n: Int): String = if (n > 0) s + repeat(n - 1) else ""
    }

    def px: Int = indent * 4

    def inc: Indenter = Indenter(indent + 1, tab)
  }

  case class JavaSToken(tokenType: JavaTokenEnum, value: String)

  case class JavaModifier(commentMacros: List[String],
                          annotations: List[String],
                          access: JavaTokenEnum,
                          generic: List[String],
                          isStatic: Boolean,
                          isFinal: Boolean,
                          isAbstract: Boolean,
                          fullPath: String) {

    def show[T]: Node[T] = 'span (
      if (generic.nonEmpty) escapeLTGT(generic.mkString("<", ", ", ">")) else Empty,
      annotations.map(x => 'span ('class /= "annotation-use", s"@$x ")),
      'span ('class /= "reserved-keyword", s"${access.value} "), Empty,
      if (isStatic) 'span ('class /= "reserved-keyword", "static ") else Empty,
      if (isFinal) 'span ('class /= "reserved-keyword", "final ") else Empty,
      if (isAbstract) 'span ('class /= "reserved-keyword", "abstract ") else Empty
    )

    def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

    def appendAnnotation(annotation: String): JavaModifier = this.copy(annotations = annotations :+ annotation)

    def setAccess(access: JavaTokenEnum): JavaModifier = this.copy(access = access)

    def setGeneric(generic: List[String]): JavaModifier = this.copy(generic = generic)

    def setStatic: JavaModifier = this.copy(isStatic = true)

    def setFinal: JavaModifier = this.copy(isFinal = true)

    def setAbstract: JavaModifier = this.copy(isAbstract = true)

    def setPath(path: String): JavaModifier = this.copy(fullPath = fullPath + path)
  }

  case class JavaEnumClass(name: String, modifier: JavaModifier,
                           enumTokens: List[String],
                           definitions: List[JavaDefinition],
                           implements: List[JavaTypeUse]) extends JavaTypeDef {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def show[T](indenter: Indenter): Node[T] =
      'div ('style /= s"margin-left: ${indenter.px}ch;",
        'a ('id /= id),
        'h3 ('class /= "java-def", 'span ('class /= "reserved-keyword", "enum"), name),
        'b ("enum values"),
        'ul ('class /= "enum-values java-def", enumTokens.map(x => 'li (x))),
        'div (definitions.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => x.show(indenter.inc))))

    override def inheritClass: List[JavaTypeUse] = Nil

    override def implementInterfaces: List[JavaTypeUse] = Nil

  }

  case class JavaClass(name: String, modifier: JavaModifier,
                       definitions: List[JavaDefinition],
                       inheritClass: List[JavaTypeUse],
                       implementInterfaces: List[JavaTypeUse]) extends JavaTypeDef {

    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    lazy val exampleCodes: List[String] = modifier.commentMacros.filter(_.startsWith("//="))

    override def show[T](indenter: Indenter): Node[T] = 'div ('style /= s"margin-left: ${indenter.px}ch;",
      'a ('id /= id),
      'h3 ('class /= "java-def", 'span ('class /= "reserved-keyword", "class "), s" $name", if (modifier.generic.nonEmpty) 'span ("<", modifier.generic.map(escapeLTGT), ">") else Empty),
      showExtends,
      showImplements,
      'p (modifier.commentMacros.filter(_.startsWith("//!")).map(_.drop(3)).mkString("\n")),
      if (exampleCodes.nonEmpty) 'pre ('code ('class /= "java", modifier.commentMacros.filter(_.startsWith("//=")).map(_.drop(3)).mkString("\n"))) else Empty,
      childDefinitions(indenter)
    )

    def showExtends[T]: Node[T] = if (inheritClass.isEmpty) "" else 'span ('span ('class /= "reserved-keyword", " extends "), inheritClass.map(x => x.show).mkString(", "))

    def showImplements[T]: Node[T] = if (implementInterfaces.isEmpty) "" else 'span ('span ('class /= "reserved-keyword", " implements "), implementInterfaces.map(x => x.show).mkString(", "))

    def childDefinitions[T](indenter: Indenter): Node[T] = 'div (definitions.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => x.show(indenter.inc)))

  }

  case class JavaInterface(name: String, modifier: JavaModifier,
                           definition: List[JavaDefinition],
                           inheritClass: List[JavaTypeUse]) extends JavaTypeDef {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def implementInterfaces: List[JavaTypeUse] = Nil

    override def show[T](indenter: Indenter): Node[T] = 'div ('style /= s"margin-left: ${indenter.px}ch;",
      'a ('id /= id),
      'h3 ('class /= "java-def", 'span ('class /= "reserved-keyword", "interface "), s" $name"),
      if (inheritClass.nonEmpty) 'div (
        'b ("inherit classes"),
        'ul (inheritClass.map(x => 'li (x.show)))
      ) else Empty
    )

  }

  case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                     definitions: List[JavaDefinition],
                                     inheritClass: List[JavaTypeUse]) extends JavaTypeDef {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def implementInterfaces: List[JavaTypeUse] = Nil

    override def show[T](indenter: Indenter): Node[T] = 'div ('style /= s"margin-left: ${indenter.px}ch;",
      'a ('id /= id),
      'h3 ('class /= "java-def", 'span ('class /= "reserved-keyword", "@interface"), s" $name")
    )
  }

  case class JavaMember(modifier: JavaModifier, name: String, memberType: JavaTypeUse) extends JavaMembers {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    def setName(n: String): JavaMember = this.copy(name = n)

    def setType(t: JavaTypeUse): JavaMember = this.copy(memberType = t)

    override def show[T](indenter: Indenter): Node[T] = 'div ('style /= s"margin-left: ${indenter.px}ch;",
      'a ('id /= id),
      'span ('class /= "full-path", modifier.fullPath),
      'p ('class /= "java-def", modifier.show, memberType.show, " ", 'span ('class /= "member-name", name)),
      'pre ('class /= "code", modifier.commentMacros.filter(_.startsWith("//=")).map(_.drop(3)).mkString("\n")),
    )
  }

  case class JavaMethod(modifier: JavaModifier, name: String, returnType: JavaTypeUse, args: List[JavaArgument]) extends JavaMembers {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    import levsha.text.renderHtml

    lazy val exampleCode = modifier.commentMacros.filter(_.startsWith("//="))

    override def show[T](indenter: Indenter): Node[T] =
      'div ('style /= s"margin-left: ${indenter.px}ch;",
        'a ('id /= id),
        'span ('class /= "full-path", modifier.fullPath),
        'p ('class /= "java-def", modifier.show, returnType.show,
          if (name == "" || name == returnType.show) "" else 'span ('class /= "method-name", s" $name"), "(",
          args.map(x => renderHtml(x.show, TextPrettyPrintingConfig.noPrettyPrinting)).mkString(", "), ")"),
        'span (modifier.commentMacros.filter(_.startsWith("//!")).map(_.drop(3)).mkString("\n")),
        if (exampleCode.nonEmpty) 'pre ('code ('class /= "java", modifier.commentMacros.filter(_.startsWith("//=")).map(_.drop(3)).mkString("\n"))) else Empty)
  }


  case class JavaTypeDesignate(name: String, extend: Option[String], generics: List[JavaTypeDesignate]) {
    import levsha.text.renderHtml
    def show: String = renderHtml(showNode, TextPrettyPrintingConfig.noPrettyPrinting)

    def describeGenerics[T]: Node[T] = 'span ("<", generics.map(x => renderHtml(x.showNode, TextPrettyPrintingConfig.noPrettyPrinting)).mkString(", "), ">")

    def showNode[T]: Node[T] = {
      if (List("boolean", "void", "int", "double", "short", "char").contains(name)) Seq('span('class /= "reserved-keyword", name))
      else Seq('span ('class /= "type-keyword", name), if (generics.nonEmpty) describeGenerics else Empty)
    }
  }

  case class JavaTypeUse(typeDesignate: JavaTypeDesignate, arrayNotations: String) {

    def show: String = s"${typeDesignate.show}$arrayNotations"

  }

  case class JavaArgument(annotations: List[String], isFinal: Boolean, name: String, argumentType: JavaTypeUse) {
    def setFinal: JavaArgument = copy(isFinal = true)

    def appendAnnotation(annos: String): JavaArgument = copy(annotations = annotations :+ annos)

    def show[T]: Node[T] = 'span (
      if (annotations.nonEmpty) annotations.map(x => 'span ('class /= "annotation-def", s"@$x ")) else Empty,
      if (isFinal) 'span ('class /= "reserved-keyword", "final ") else Empty,
      'span (s"${argumentType.show} "), name
    )
  }

  case class JavaPackageDef(annotations: List[String], name: String)

  object JavaTypeUse {
    def empty: JavaTypeUse = JavaTypeUse(JavaTypeDesignate("", None, Nil), "")
  }

  object JavaModifier {
    def empty(path: String): JavaModifier = JavaModifier(Nil, Nil, PRIVATE, Nil, isStatic = false, isFinal = false, isAbstract = false, path)
  }

  object ListImplicit {

    implicit class strListExt[+A](list: List[A]) {
      def addIf[B >: A](p: => Boolean, item: B): List[B] = if (p) list :+ item else list

      def add[B >: A](item: B): List[B] = list :+ item
    }

  }

  object JavaArgument {
    def empty: JavaArgument = JavaArgument(Nil, isFinal = false, "", JavaTypeUse.empty)
  }

}
