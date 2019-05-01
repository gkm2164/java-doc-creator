package com.kakao.search.middle.javadoc

import com.kakao.search.middle.javalang.JavaTokenEnum

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
  override def show: String = s"== class $name (${modifier.show}) ==\n\n" + definitions.map(_.show).mkString("\n")
}

case class JavaInterface(name: String, modifier: JavaModifier,
                         definition: List[JavaDefininiton],
                         inheritClass: List[String]) extends JavaTypeDef {
  override def implementInterfaces: List[String] = Nil

  override def show: String = s"== interface $name ==\n${this}"
}

case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                   definitions: List[JavaDefininiton],
                                   inheritClass: List[String]) extends JavaTypeDef {
  override def implementInterfaces: List[String] = Nil

  override def show: String = s"== annotation interface $name ==\n${this}"
}

trait JavaMembers extends JavaDefininiton

case class JavaMember(modifier: JavaModifier, name: String, memberType: String) extends JavaMembers {
  def setName(n: String): JavaMember = this.copy(name = n)

  def setType(t: String): JavaMember = this.copy(memberType = t)

  override def show: String = s"== member variable $name ==\n${this}"
}

case class JavaMethod(modifier: JavaModifier, name: String, returnType: String, args: List[JavaArgument]) extends JavaMembers {
  override def show: String = s"== method $name ==\n$this"
}

case class JavaArgument(annotations: List[String], isFinal: Boolean, name: String, argumentType: String) {
  def setFinal: JavaArgument = copy(isFinal = true)

  def appendAnnotation(annos: String): JavaArgument = copy(annotations = annotations :+ annos)

}

object JavaArgument {
  def empty: JavaArgument = JavaArgument(Nil, isFinal = false, "", "")
}

case class JavaPackageDef(annotations: List[String], name: String)
