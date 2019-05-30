package com.kakao.bengo.scala

import com.kakao.bengo.javalang.JavaTokenEnum
import levsha.Document.{Empty, Node}
import levsha.text.symbolDsl._

import scala.annotation.tailrec
import scala.util.Random

package object javadoc {

  import com.kakao.bengo.javalang.JavaTokenEnum._

  val ASCIIValues: IndexedSeq[Char] = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')

  def color[T](text: String, color: String): Node[T] = 'span ('style /= s"color: $color;", text)

  def escapeLTGT(str: String): String = { // 문서 상에 <, > 부호가 안보이는 경우...
    @tailrec
    def loop(str: String, acc: String): String = {
      if (str.isEmpty) acc
      else if (str.head == '<') loop(str.tail, acc + "&lt;")
      else if (str.head == '>') loop(str.tail, acc + "&gt;")
      else loop(str.tail, acc + str.head)
    }

    loop(str, "")
  }

  sealed trait JavaDefinition {
    def id: String

    def name: String

    def show[T]: Node[T]

    def modifier: JavaModifier
  }

  sealed trait JavaTypeDef extends JavaDefinition {
    def inheritClass: List[String]

    def implementInterfaces: List[String]
  }

  trait JavaMembers extends JavaDefinition

  case class JavaSToken(tokenType: JavaTokenEnum, value: String)

  case class JavaModifier(commentMacros: List[String],
                          annotations: List[String],
                          access: JavaTokenEnum,
                          generic: String,
                          isStatic: Boolean,
                          isFinal: Boolean,
                          isAbstract: Boolean,
                          fullPath: String) {

    def show[T]: Node[T] = 'span (
      annotations.map(x => color(s"@$x ", "#FFC433")),
      color(s"${access.value} ", "blue"), Empty,
      if (isStatic) color("static ", "blue") else Empty,
      if (isFinal) color("final ", "blue") else Empty,
      if (isAbstract) color("abstract ", "blue") else Empty
    )

    def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

    def appendAnnotation(annotation: String): JavaModifier = this.copy(annotations = annotations :+ annotation)

    def setAccess(access: JavaTokenEnum): JavaModifier = this.copy(access = access)

    def setGeneric(generic: String): JavaModifier = this.copy(generic = generic)

    def setStatic: JavaModifier = this.copy(isStatic = true)

    def setFinal: JavaModifier = this.copy(isFinal = true)

    def setAbstract: JavaModifier = this.copy(isAbstract = true)

    def setPath(path: String): JavaModifier = this.copy(fullPath = fullPath + path)
  }

  case class JavaEnumClass(name: String, modifier: JavaModifier,
                           enumTokens: List[String],
                           definitions: List[JavaDefinition],
                           implements: List[String]) extends JavaTypeDef {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def show[T]: Node[T] =
      'div (
        'a ('id /= id),
        'h3 ('class /= "type-def", s"${color("enum", "blue")} $name"),
        'b ("enum values"),
        'ul (enumTokens.map(x => 'li (x))),
        'div (definitions.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => x.show)))

    override def inheritClass: List[String] = Nil

    override def implementInterfaces: List[String] = Nil

  }

  case class JavaClass(name: String, modifier: JavaModifier,
                       definitions: List[JavaDefinition],
                       inheritClass: List[String],
                       implementInterfaces: List[String]) extends JavaTypeDef {

    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def show[T]: Node[T] = 'div (
      'a ('id /= id),
      'h3 ('class /= "type-def", color("class", "blue"), s" $name", showExtends, showImplements),
      'pre ('class /= "code", modifier.commentMacros.map(_.drop(3)).mkString("\n")),
      'b (modifier.fullPath),
      childDefinitions
    )

    def showExtends[T]: Node[T] = if (inheritClass.isEmpty) "" else 'span (color(" extends ", "blue"), inheritClass.map(escapeLTGT).mkString(", "))

    def showImplements[T]: Node[T] = if (implementInterfaces.isEmpty) "" else 'span (color(" implements ", "blue"), implementInterfaces.map(escapeLTGT).mkString(", "))

    def childDefinitions[T]: Node[T] = 'div (definitions.filter(_.modifier.access != PRIVATE).sortBy(_.name).map(x => x.show))

  }

  case class JavaInterface(name: String, modifier: JavaModifier,
                           definition: List[JavaDefinition],
                           inheritClass: List[String]) extends JavaTypeDef {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def implementInterfaces: List[String] = Nil

    override def show[T]: Node[T] = 'div (
      'a ('id /= id),
      'h3 ('class /= "type-def", color("interface", "blue"), s" $name"),
      'div (
        'b ("inherit classes"),
        'ul (inheritClass.map(x => 'li (escapeLTGT(x))))
      )
    )

  }

  case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                     definitions: List[JavaDefinition],
                                     inheritClass: List[String]) extends JavaTypeDef {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def implementInterfaces: List[String] = Nil

    override def show[T]: Node[T] = 'div (
      'a ('id /= id),
      'h3 ('class /= "type-def", color("@interface", "blue"), s" $name")
    )
  }

  case class JavaMember(modifier: JavaModifier, name: String, memberType: String) extends JavaMembers {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    def setName(n: String): JavaMember = this.copy(name = n)

    def setType(t: String): JavaMember = this.copy(memberType = t)

    override def show[T]: Node[T] = 'div (
      'a ('id /= id),
      'pre ('class /= "code", modifier.commentMacros.map(_.drop(3)).mkString("\n")),
      'p (modifier.show, color(escapeLTGT(memberType), "#769AC8"), name),
      'h4 (modifier.fullPath)
    )

  }

  case class JavaMethod(modifier: JavaModifier, name: String, returnType: String, args: List[JavaArgument]) extends JavaMembers {
    lazy val id: String = (1 to 10).map(_ => ASCIIValues(Random.nextInt(ASCIIValues.length))).mkString("")

    override def show[T]: Node[T] =
      'div (
        'a ('id /= id),
        'p (modifier.show, color(escapeLTGT(returnType), "#769AC8"),
          if (name == "" || name == returnType) 'b (color(" CONSTRUCTOR", "red")) else s" $name",
          args.map(_.show).mkString("(", ", ", ")")),
        'pre (modifier.commentMacros.map(_.drop(3)).mkString("\n")),
        'h4 (modifier.fullPath)
      )
  }

  case class JavaArgument(annotations: List[String], isFinal: Boolean, name: String, argumentType: String) {

    import ListImplicit._

    def setFinal: JavaArgument = copy(isFinal = true)

    def appendAnnotation(annos: String): JavaArgument = copy(annotations = annotations :+ annos)

    def show: String =
      Nil.addIf(annotations.nonEmpty, annotations.map(x => s"@$x").mkString(" "))
        .addIf(isFinal, "final")
        .add(escapeLTGT(argumentType))
        .add(name).mkString(" ")
  }

  case class JavaPackageDef(annotations: List[String], name: String)

  object JavaModifier {
    def empty(path: String): JavaModifier = JavaModifier(Nil, Nil, PRIVATE, "", isStatic = false, isFinal = false, isAbstract = false, path)
  }

  object ListImplicit {

    implicit class strListExt[+A](list: List[A]) {
      def addIf[B >: A](p: => Boolean, item: B): List[B] = if (p) list :+ item else list

      def add[B >: A](item: B): List[B] = list :+ item
    }

  }

  object JavaArgument {
    def empty: JavaArgument = JavaArgument(Nil, isFinal = false, "", "")
  }

}
