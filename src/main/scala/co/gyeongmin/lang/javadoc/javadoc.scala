package co.gyeongmin.lang

import co.gyeongmin.lang.javadoc.codeformatter.JavaCodeFormatter
import co.gyeongmin.lang.javalang.JavaTokenEnum
import levsha.Document._
import levsha.impl.TextPrettyPrintingConfig
import levsha.text.symbolDsl._
import scala.annotation.tailrec

package object javadoc {

  import JavaTokenEnum._

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
    def representName: String = modifier.commentMacros
      .find(_.startsWith("//name"))
      .map(_.replace("//name", ""))
      .getOrElse(name)

    final def id: String = {
      val ret = s"${modifier.fullPath.toLowerCase.replace(".", "-")}-${name.toLowerCase}"
      this match {
        case JavaMethod(_, _, _, args, _) if args.nonEmpty => ret + "-" + args.map(_.name.toLowerCase).mkString("-")
        case _ => ret

      }
    }

    def name: String

    def modifier: JavaModifier
  }

  sealed trait JavaTypeDef extends JavaDefinition {

    def inheritClass: Vector[JavaTypeUse]

    def implementInterfaces: Vector[JavaTypeUse]
  }

  sealed trait JavaMembers extends JavaDefinition {
  }

  final case class Indent(indent: Int, tab: String = "&nbsp;&nbsp;") {

    implicit class StringExtend(s: String) {
      def repeat(n: Int): String = if (n > 0) s + repeat(n - 1) else ""
    }

    def ch(unit: Int = 10): Int = (if (indent == 0) 0 else 2) * unit

    def inc: Indent = Indent(indent + 1, tab)
  }

  final case class JavaSToken(tokenType: JavaTokenEnum, value: String)

  final case class JavaModifier(commentMacros: Vector[String],
                                annotations: Vector[JavaAnnotationCall],
                                access: JavaTokenEnum,
                                generic: Vector[JavaTypeDesignate],
                                isStatic: Boolean,
                                isFinal: Boolean,
                                isAbstract: Boolean,
                                fullPath: String) {

    def show[T]: Node[T] = 'span(
      if (generic.nonEmpty) 'span('span('class /= "generic-symbol", "&lt;"), escapeLTGT(generic.mkString(", ")), 'span('class /= "generic-symbol", "&gt;")) else Empty,
      annotations.map(x => 'span('class /= "annotation-use", s"@${x.name}${x.parameters} ")),
      'span('class /= "reserved-keyword", s"${access.value} "), Empty,
      if (isStatic) 'span('class /= "reserved-keyword", "static ") else Empty,
      if (isFinal) 'span('class /= "reserved-keyword", "final ") else Empty,
      if (isAbstract) 'span('class /= "reserved-keyword", "abstract ") else Empty
    )

    def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

    def appendAnnotation(annotation: JavaAnnotationCall): JavaModifier = this.copy(annotations = annotations :+ annotation)

    def setAccess(access: JavaTokenEnum): JavaModifier = this.copy(access = access)

    def setGeneric(generic: Vector[JavaTypeDesignate]): JavaModifier = this.copy(generic = generic)

    def setStatic: JavaModifier = this.copy(isStatic = true)

    def setFinal: JavaModifier = this.copy(isFinal = true)

    def setAbstract: JavaModifier = this.copy(isAbstract = true)

    def setPath(path: String): JavaModifier = this.copy(fullPath = fullPath + path)
  }

  final case class JavaEnumClass(name: String, modifier: JavaModifier,
                                 enumTokens: Vector[String],
                                 definitions: Vector[JavaDefinition],
                                 implements: Vector[JavaTypeUse]) extends JavaTypeDef {

    override def inheritClass: Vector[JavaTypeUse] = Vector.empty

    override def implementInterfaces: Vector[JavaTypeUse] = Vector.empty

  }

  final case class JavaClass(name: String, modifier: JavaModifier,
                             definitions: Vector[JavaDefinition],
                             inheritClass: Vector[JavaTypeUse],
                             implementInterfaces: Vector[JavaTypeUse]) extends JavaTypeDef {

    lazy val exampleCodes: Vector[String] = modifier.commentMacros.filter(_.startsWith("//="))
  }

  final case class JavaInterface(name: String, modifier: JavaModifier,
                                 definition: Vector[JavaDefinition],
                                 inheritClass: Vector[JavaTypeUse]) extends JavaTypeDef {

    override def implementInterfaces: Vector[JavaTypeUse] = Vector.empty
  }

  final case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                           definitions: Vector[JavaDefinition],
                                           inheritClass: Vector[JavaTypeUse]) extends JavaTypeDef {
    val annotationMeta: Map[String, Vector[String]] =
      modifier.annotations.groupBy(_.name).mapValues(_.map(_.parameters.replace("(", "").replace(")", "")))
    val decideTarget: Option[String] = annotationMeta.get("Target").map(_.headOption.getOrElse(""))

    override def implementInterfaces: Vector[JavaTypeUse] = Vector.empty

  }

  final case class JavaMember(modifier: JavaModifier, name: String, memberType: JavaTypeUse) extends JavaMembers {
    def setName(n: String): JavaMember = this.copy(name = n)

    def setType(t: JavaTypeUse): JavaMember = this.copy(memberType = t)
  }

  final case class JavaMethod(modifier: JavaModifier, name: String, returnType: JavaTypeUse, args: Vector[JavaArgument], codes: Vector[JavaSToken]) extends JavaMembers {
    lazy val exampleCode: Vector[String] = modifier.commentMacros.filter(_.startsWith("//="))

//    println(JavaCodeFormatter.printCode(name, codes))
  }

  final case class JavaTypeDesignate(name: String, extend: Option[(String, JavaTypeDesignate)], generics: Vector[JavaTypeDesignate], arrayNotation: String) {

    import levsha.text.renderHtml

    def show: String = renderHtml(showNode, TextPrettyPrintingConfig.noPrettyPrinting)

    def genericSymbol[T](word: String): Node[T] = 'span('class /= "generic-symbol", word)

    def describeGenerics[T]: Node[T] = 'span(
      genericSymbol("&lt;"),
      generics.map(x => renderHtml(x.showNode, TextPrettyPrintingConfig.noPrettyPrinting)).mkString(", "),
      genericSymbol("&gt;"))

    def showNode[T]: Node[T] = {
      if (Vector("boolean", "void", "int", "double", "short", "char").contains(name)) Seq('span('class /= "reserved-keyword", name + arrayNotation))
      else Seq('span('class /= "type-keyword", name), if (generics.nonEmpty) describeGenerics else Empty, 'span(arrayNotation))
    }
  }

  final case class JavaTypeUse(typeDesignate: JavaTypeDesignate, arrayNotations: String) {
    def name: String = s"${typeDesignate.name}$arrayNotations"

    def show: String = s"${typeDesignate.show}$arrayNotations"
  }

  final case class JavaArgument(annotations: Vector[JavaAnnotationCall], isFinal: Boolean, name: String, argumentType: JavaTypeUse) {
    def setFinal: JavaArgument = copy(isFinal = true)

    def appendAnnotation(annotation: JavaAnnotationCall): JavaArgument = copy(annotations = annotations :+ annotation)

    def show[T]: Node[T] = 'span(
      if (annotations.nonEmpty) annotations.map(x => 'span('class /= "annotation-def", s"@${x.name}${x.parameters} ")) else Empty,
      if (isFinal) 'span('class /= "reserved-keyword", "final ") else Empty,
      'span(s"${argumentType.show} "), name
    )
  }

  final case class JavaPackageDef(annotations: Vector[String], name: String)

  final case class JavaAnnotationCall(name: String, parameters: String)

  case object JavaDefinitionUnit extends JavaDefinition {
    override def name: String = ""

    override def modifier: JavaModifier = JavaModifier.empty("", Vector.empty)

    override def representName: String = ""
  }

  object JavaTypeUse {
    def empty: JavaTypeUse = JavaTypeUse(JavaTypeDesignate("", None, Vector.empty, ""), "")
  }

  object JavaModifier {
    def empty(path: String, annotationsBuf: Vector[JavaAnnotationCall] = Vector.empty): JavaModifier =
      JavaModifier(Vector.empty, annotationsBuf, PRIVATE, Vector.empty, isStatic = false, isFinal = false, isAbstract = false, path)
  }

  object VectorImplicit {

    implicit class strVectorExt[+A](list: Vector[A]) {
      def addIf[B >: A](p: => Boolean, item: B): Vector[B] = if (p) list :+ item else list

      def add[B >: A](item: B): Vector[B] = list :+ item
    }

  }

  object JavaArgument {
    def empty: JavaArgument = JavaArgument(Vector.empty, isFinal = false, "", JavaTypeUse.empty)
  }

}
