package co.gyeongmin.scala.godoc

import co.gyeongmin.scala.godoc.javalang.JavaTokenEnum

import scala.annotation.tailrec

package object javadoc {
  import co.gyeongmin.scala.godoc.javalang.JavaTokenEnum._

  def color(text: String, color: String): String = s"""<span style="color: $color;">$text</span>"""

  case class JavaSToken(tokenType: JavaTokenEnum, value: String)

  sealed trait JavaDefinition {
    def show: String

    def modifier: JavaModifier
  }

  case class JavaModifier(commentMacros: List[String],
                          annotations: List[String],
                          access: JavaTokenEnum,
                          generic: String,
                          isStatic: Boolean,
                          isFinal: Boolean,
                          isAbstract: Boolean) {

    def show: String = commentMacros.mkString("\n")

    def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

    def appendAnnotation(annotation: String): JavaModifier = this.copy(annotations = annotations :+ annotation)

    def setAccess(access: JavaTokenEnum): JavaModifier = this.copy(access = access)

    def setGeneric(generic: String): JavaModifier = this.copy(generic = generic)

    def setStatic: JavaModifier = this.copy(isStatic = true)

    def setFinal: JavaModifier = this.copy(isFinal = true)

    def setAbstract: JavaModifier = this.copy(isAbstract = true)

  }

  object JavaModifier {
    def empty: JavaModifier = JavaModifier(Nil, Nil, PRIVATE, "", isStatic = false, isFinal = false, isAbstract = false)
  }

  sealed trait JavaTypeDef extends JavaDefinition {
    def inheritClass: List[String]

    def implementInterfaces: List[String]
  }

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

  case class JavaEnumClass(name: String, modifier: JavaModifier,
                           enumTokens: List[String],
                           definitions: List[JavaDefinition]) extends JavaTypeDef {

    override def show: String = s"""<h3 class="type-def">${color("enum", "blue")} $name</h3>""" + s"""<b>enum values</b>${enumTokens.map(x => s"<li>$x</li>").mkString("<ul>", "", "</ul>")}""" + definitions.map(x => x.show).mkString("<div>", "", "</div>")
    override def inheritClass: List[String] = Nil
    override def implementInterfaces: List[String] = Nil
  }

  case class JavaClass(name: String, modifier: JavaModifier,
                       definitions: List[JavaDefinition],
                       inheritClass: List[String],
                       implementInterfaces: List[String]) extends JavaTypeDef {

    lazy val showExtends: String = if (inheritClass.isEmpty) "" else s" ${color("extends", "blue")} " + inheritClass.map(escapeLTGT).mkString(", ")
    lazy val showImplements: String = if (implementInterfaces.isEmpty) "" else s" ${color("implements", "blue")} " + implementInterfaces.map(escapeLTGT).mkString(", ")
    override def show: String = s"""<h3 class="type-def">${color("class", "blue")} $name$showExtends$showImplements</h3>""" + modifier.commentMacros.map(_.drop(3)).mkString("<pre class=\"code\">", "\n", "</pre>") + definitions.map(x => x.show).mkString("<div>", "", "</div>")
  }

  case class JavaInterface(name: String, modifier: JavaModifier,
                           definition: List[JavaDefinition],
                           inheritClass: List[String]) extends JavaTypeDef {
    override def implementInterfaces: List[String] = Nil

    override def show: String =
      s"""<h3 class="type-def">${color("interface", "blue")} $name</h3>
         |<div>
         |<b>inherit class</b>
         |${inheritClass.map(x => s"<li>${escapeLTGT(x)}</li>").mkString("<ul>", "", "</ul>")}
         |</div>""".stripMargin
  }

  case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                     definitions: List[JavaDefinition],
                                     inheritClass: List[String]) extends JavaTypeDef {
    override def implementInterfaces: List[String] = Nil
    override def show: String = s"""<h3 class="type-def">${color("@interface", "blue")} $name</h3>"""
  }

  object ListImplicit {
    implicit class strListExt[+A](list: List[A]) {
      def addIf[B >: A](p: => Boolean, item: B): List[B] = if (p) list :+ item else list

      def add[B >: A](item: B): List[B] = list :+ item
    }

  }

  trait JavaMembers extends JavaDefinition

  case class JavaMember(modifier: JavaModifier, name: String, memberType: String) extends JavaMembers {

    import ListImplicit._

    def setName(n: String): JavaMember = this.copy(name = n)

    def setType(t: String): JavaMember = this.copy(memberType = t)

    override def show: String = s"${modifier.commentMacros.mkString("<div>", "\n", "</div>")}" +
      Nil.addIf(modifier.annotations.nonEmpty, modifier.annotations.map(x => color(s"@$x", "#FFC433")).mkString(" "))
        .add(color(modifier.access.value, "blue"))
        .addIf(modifier.isStatic, color("static", "blue"))
        .addIf(modifier.isFinal, color("final", "blue"))
        .addIf(modifier.isAbstract, color("abstract", "blue"))
        .add(color(escapeLTGT(memberType), "#769AC8"))
        .add(name).mkString(" ") + "<br/>"
  }

  case class JavaMethod(modifier: JavaModifier, name: String, returnType: String, args: List[JavaArgument]) extends JavaMembers {
    import ListImplicit._
    override def show: String =
      "<p>" + Nil.addIf(modifier.annotations.nonEmpty, modifier.annotations.map(x => color(s"@$x", "#FFC433")).mkString(" "))
      .add(color(modifier.access.value, "blue"))
      .addIf(modifier.isStatic, color("static", "blue"))
      .addIf(modifier.isFinal, color("final", "blue"))
      .addIf(modifier.isAbstract, color("abstract", "blue"))
      .add(color(escapeLTGT(returnType), "#769AC8") + (if(name == "") "" else s" $name") + args.map(_.show).mkString("(", ", ", ")"))
      .mkString(" ") + "</p>" + s"${modifier.commentMacros.map(_.drop(3)).mkString("<pre>", "\n", "</pre>")}" // drop "//="
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

  object JavaArgument {
    def empty: JavaArgument = JavaArgument(Nil, isFinal = false, "", "")
  }

  case class JavaPackageDef(annotations: List[String], name: String)

}
