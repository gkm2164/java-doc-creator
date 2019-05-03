package com.kakao.search.middle

import com.kakao.search.middle.javalang.JavaTokenEnum

package object javadoc {
  import JavaTokenEnum._

  case class JavaSToken(tokenType: JavaTokenEnum, value: String)

  sealed trait JavaDefininiton {
    def show: String

    def modifier: JavaModifier
  }

  case class JavaModifier(commentMacros: List[String], annotations: List[String], access: JavaTokenEnum, isStatic: Boolean, isFinal: Boolean, isAbstract: Boolean) {
    def show: String = s"${commentMacros.mkString("\n")}"

    def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

    def appendAnnotation(annotation: String): JavaModifier = this.copy(annotations = annotations :+ annotation)

    def setAccess(access: JavaTokenEnum): JavaModifier = this.copy(access = access)

    def setStatic: JavaModifier = this.copy(isStatic = true)

    def setFinal: JavaModifier = this.copy(isFinal = true)

    def setAbstract: JavaModifier = this.copy(isAbstract = true)
  }

  object JavaModifier {
    def empty: JavaModifier = JavaModifier(Nil, Nil, PUBLIC, isStatic = false, isFinal = false, isAbstract = false)
  }

  sealed trait JavaTypeDef extends JavaDefininiton {
    def inheritClass: List[String]

    def implementInterfaces: List[String]
  }

  case class JavaClass(name: String, modifier: JavaModifier,
                       definitions: List[JavaDefininiton],
                       inheritClass: List[String],
                       implementInterfaces: List[String]) extends JavaTypeDef {
    override def show: String = s"""<p><font color="blue">class</font> $name</p>""" + definitions.map(x => x.show).mkString("<div>", "", "</div>")
  }

  case class JavaInterface(name: String, modifier: JavaModifier,
                           definition: List[JavaDefininiton],
                           inheritClass: List[String]) extends JavaTypeDef {
    override def implementInterfaces: List[String] = Nil

    override def show: String =
      s"""<p><font color="blue">interface</font> $name</p><div>
         |<b>inherit class</b>
         |<ul>
         |${inheritClass.map(x => s"<li>$x</li>").mkString("")}
         |</ul>
         |</div>""".stripMargin
  }

  case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                     definitions: List[JavaDefininiton],
                                     inheritClass: List[String]) extends JavaTypeDef {
    override def implementInterfaces: List[String] = Nil

    override def show: String = s"""<p><font color="blue">@interface</font> $name</p>"""
  }

  object ListImplicit {

    implicit class strListExt[+A](list: List[A]) {
      def addIf[B >: A](p: => Boolean, item: B): List[B] = if (p) list :+ item else list

      def add[B >: A](item: B): List[B] = list :+ item
    }

  }

  trait JavaMembers extends JavaDefininiton

  case class JavaMember(modifier: JavaModifier, name: String, memberType: String) extends JavaMembers {

    import ListImplicit._

    def setName(n: String): JavaMember = this.copy(name = n)

    def setType(t: String): JavaMember = this.copy(memberType = t)

    def color(text: String, color: String): String = s"""<font color="$color">$text</font>"""

    override def show: String = s"<p>${modifier.commentMacros}<br />" +
      Nil.addIf(modifier.annotations.nonEmpty, modifier.annotations.map(x => color(s"@$x", "#FFC433")).mkString(" "))
        .add(color(modifier.access.value, "blue"))
        .addIf(modifier.isStatic, color("static", "blue"))
        .addIf(modifier.isFinal, color("final", "blue"))
        .addIf(modifier.isAbstract, color("abstract", "blue"))
        .add(memberType)
        .add(name).mkString(" ") + "</p>"
  }

  case class JavaMethod(modifier: JavaModifier, name: String, returnType: String, args: List[JavaArgument]) extends JavaMembers {
    //  override def show: String = s"== method $name ==\n$this"
    import ListImplicit._

    def color(text: String, color: String): String = s"""<font color="$color">$text</font>"""

    override def show: String = s"<p>${modifier.commentMacros}<br />" +
      Nil.addIf(modifier.annotations.nonEmpty, modifier.annotations.map(x => color(s"@$x", "#FFC433")).mkString(" "))
      .add(modifier.access.value)
      .addIf(modifier.isStatic, "static")
      .addIf(modifier.isFinal, "final")
      .addIf(modifier.isAbstract, "abstract")
      .add(returnType)
      .add(name + (if (args.nonEmpty) args.map(_.show).mkString("(", ", ", ")") else ""))
      .mkString(" ") + "</p>"
  }

  case class JavaArgument(annotations: List[String], isFinal: Boolean, name: String, argumentType: String) {

    import ListImplicit._

    def setFinal: JavaArgument = copy(isFinal = true)

    def appendAnnotation(annos: String): JavaArgument = copy(annotations = annotations :+ annos)

    def show: String =
      Nil.addIf(annotations.nonEmpty, annotations.map(x => s"@$x").mkString(" "))
        .addIf(isFinal, "final")
        .add(argumentType)
        .add(name).mkString(" ")
  }

  object JavaArgument {
    def empty: JavaArgument = JavaArgument(Nil, isFinal = false, "", "")
  }

  case class JavaPackageDef(annotations: List[String], name: String)

}
