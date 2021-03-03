package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.codeformatter.javaentities.Accessibility.Accessibility

package object javaentities {
  object Accessibility extends Enumeration {
    type Accessibility = Value
    val PUBLIC, PROTECTED, PRIVATE = Value
  }

  case class AnnotationCall()
  case class Modifier(
      access: Accessibility = Accessibility.PRIVATE,
      annotations: List[AnnotationCall] = Nil,
      isStatic: Boolean = false,
      isFinal: Boolean = false
  )
  trait TypeBound

  case class TypeBoundExtends(name: String) extends TypeBound
  case class TypeBoundSuper(name: String) extends TypeBound

  case class TypeParam(name: String, typeBound: TypeBound)

  trait JavaType

  case class JavaClass(
      modifier: Modifier,
      name: String,
      typeParams: List[TypeParam],
      superClass: JavaClass,
      superInterfaces: List[JavaInterface]
  ) extends JavaType

  case class JavaInterface(
      modifier: Modifier,
      name: String,
      typeParams: List[TypeParam],
      superInterfaces: List[JavaInterface]
  ) extends JavaType

  case class JavaAnnotationInterface(modifier: Modifier, name: String)

}
