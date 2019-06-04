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

    def modifier: JavaModifier

    def representName: String
  }

  sealed trait JavaTypeDef extends JavaDefinition {
    def inheritClass: List[JavaTypeUse]

    def implementInterfaces: List[JavaTypeUse]
  }

  trait JavaMembers extends JavaDefinition

  case class Indent(indent: Int, tab: String = "&nbsp;&nbsp;") {

    implicit class StringExtend(s: String) {
      def repeat(n: Int): String = if (n > 0) s + repeat(n - 1) else ""
    }

    def ch(unit: Int = 10): Int = (if (indent == 0) 0 else 2) * unit

    def inc: Indent = Indent(indent + 1, tab)
  }

  case class JavaSToken(tokenType: JavaTokenEnum, value: String)

  case class JavaModifier(commentMacros: List[String],
                          annotations: List[JavaAnnotationCall],
                          access: JavaTokenEnum,
                          generic: List[String],
                          isStatic: Boolean,
                          isFinal: Boolean,
                          isAbstract: Boolean,
                          fullPath: String) {

    def show[T]: Node[T] = 'span (
      if (generic.nonEmpty) 'span('span('class /= "generic-symbol", "&lt;"), escapeLTGT(generic.mkString(", ")), 'span('class /= "generic-symbol", "&gt;")) else Empty,
      annotations.map(x => 'span ('class /= "annotation-use", s"@${x.name}${x.parameters} ")),
      'span ('class /= "reserved-keyword", s"${access.value} "), Empty,
      if (isStatic) 'span ('class /= "reserved-keyword", "static ") else Empty,
      if (isFinal) 'span ('class /= "reserved-keyword", "final ") else Empty,
      if (isAbstract) 'span ('class /= "reserved-keyword", "abstract ") else Empty
    )

    def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

    def appendAnnotation(annotation: JavaAnnotationCall): JavaModifier = {
      println(annotation)
      this.copy(annotations = annotations :+ annotation)
    }

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
//    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")
    lazy val id = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"

    lazy val representName: String = modifier.commentMacros.find(_.startsWith("//name")).map(_.drop("//name".length).trim).getOrElse(name)

    override def inheritClass: List[JavaTypeUse] = Nil

    override def implementInterfaces: List[JavaTypeUse] = Nil

  }

  case class JavaClass(name: String, modifier: JavaModifier,
                       definitions: List[JavaDefinition],
                       inheritClass: List[JavaTypeUse],
                       implementInterfaces: List[JavaTypeUse]) extends JavaTypeDef {

    lazy val id = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"

    lazy val exampleCodes: List[String] = modifier.commentMacros.filter(_.startsWith("//="))
    lazy val representName: String = modifier.commentMacros.find(_.startsWith("//name")).map(_.replace("//name", "")).getOrElse(name)
  }

  case class JavaInterface(name: String, modifier: JavaModifier,
                           definition: List[JavaDefinition],
                           inheritClass: List[JavaTypeUse]) extends JavaTypeDef {
    lazy val id = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"

    lazy val representName: String = modifier.commentMacros.find(_.startsWith("//name")).map(_.replace("//name", "")).getOrElse(name)

    override def implementInterfaces: List[JavaTypeUse] = Nil
  }

  case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                     definitions: List[JavaDefinition],
                                     inheritClass: List[JavaTypeUse]) extends JavaTypeDef {
    lazy val id = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"

    lazy val representName: String = modifier.commentMacros.find(_.startsWith("//name")).map(_.replace("//name", "")).getOrElse(name)
    val annotationMeta: Map[String, List[String]] =
      modifier.annotations.groupBy(_.name).mapValues(_.map(_.parameters.replace("(", "").replace(")", "")))
    val decideTarget: Option[String] = annotationMeta.get("Target").map(_.headOption.getOrElse(""))
    override def implementInterfaces: List[JavaTypeUse] = Nil

  }

  case class JavaMember(modifier: JavaModifier, name: String, memberType: JavaTypeUse) extends JavaMembers {
    lazy val id = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"

    lazy val representName: String = modifier.commentMacros.find(_.startsWith("//name")).map(_.replace("//name", "")).getOrElse(name)

    def setName(n: String): JavaMember = this.copy(name = n)

    def setType(t: JavaTypeUse): JavaMember = this.copy(memberType = t)
  }

  case class JavaMethod(modifier: JavaModifier, name: String, returnType: JavaTypeUse, args: List[JavaArgument]) extends JavaMembers {
    lazy val id = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"

    lazy val exampleCode: List[String] = modifier.commentMacros.filter(_.startsWith("//="))

    lazy val representName: String = modifier.commentMacros.find(_.startsWith("//name")).map(_.replace("//name", "")).getOrElse(name)

  }


  case class JavaTypeDesignate(name: String, extend: Option[(String, String)], generics: List[JavaTypeDesignate]) {

    import levsha.text.renderHtml

    def show: String = renderHtml(showNode, TextPrettyPrintingConfig.noPrettyPrinting)

    def genericSymbol[T](word: String): Node[T] = 'span ('class /= "generic-symbol", word)

    def describeGenerics[T]: Node[T] = 'span (
      genericSymbol("&lt;"),
      generics.map(x => renderHtml(x.showNode, TextPrettyPrintingConfig.noPrettyPrinting)).mkString(", "),
      genericSymbol("&gt;"))

    def showNode[T]: Node[T] = {
      if (List("boolean", "void", "int", "double", "short", "char").contains(name)) Seq('span ('class /= "reserved-keyword", name))
      else Seq('span ('class /= "type-keyword", name), if (generics.nonEmpty) describeGenerics else Empty)
    }
  }

  case class JavaTypeUse(typeDesignate: JavaTypeDesignate, arrayNotations: String) {
    def show: String = s"${typeDesignate.show}$arrayNotations"
  }

  case class JavaArgument(annotations: List[JavaAnnotationCall], isFinal: Boolean, name: String, argumentType: JavaTypeUse) {
    def setFinal: JavaArgument = copy(isFinal = true)

    def appendAnnotation(annotation: JavaAnnotationCall): JavaArgument = copy(annotations = annotations :+ annotation)

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
    def empty(path: String, annotationsBuf: List[JavaAnnotationCall] = Nil): JavaModifier = JavaModifier(Nil, annotationsBuf, PRIVATE, Nil, isStatic = false, isFinal = false, isAbstract = false, path)
  }

  case class JavaAnnotationCall(name: String, parameters: String)

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
