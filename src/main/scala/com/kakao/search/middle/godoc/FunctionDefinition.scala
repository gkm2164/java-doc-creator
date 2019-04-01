package com.kakao.search.middle.godoc

trait FunctionWriter {
  def show(fd: FunctionDefinition): String
  def showArgs(arg: Argument): String
}

sealed trait FunctionDefinition {
  def typeName: String
  def desc: String
  def ex: Option[String]

  def show(implicit functionWriter: FunctionWriter): String = functionWriter.show(this)
}

case class Constructor(typeName: String, argList: List[Argument], desc: String, ex: Option[String]) extends FunctionDefinition
case class Member(typeName: String, method: String, argList: List[Argument], desc: String, ex: Option[String]) extends FunctionDefinition
case class Setter(typeName: String, memberName: String, memberType: String, desc: String, ex: Option[String]) extends FunctionDefinition

object FunctionDefinition {
  def create(maps: Seq[Map[String, ArrayElem]]): Seq[FunctionDefinition] = {
    maps.flatMap { map =>
      map.get("method_type") flatMap {
        case MacroName("constructor") =>
          val typeName = map.get("type") match {
            case Some(Text(x)) => x
            case _ => ""
          }
          val desc = map.get("desc") match {
            case Some(Text(x)) => x
            case _ => ""
          }
          val argList = map.get("args") match {
            case Some(ArgList(list)) => list
            case _ => Nil
          }
          val ex = map.get("ex") match {
            case Some(Text(x)) => Some(x)
            case _ => None
          }
          Some(Constructor(typeName, argList, desc, ex))
        case MacroName("member") =>
          val typeName = map.get("type") match {
            case Some(Text(x)) => x
            case _ => ""
          }
          val method = map.get("method") match {
            case Some(Text(x)) => x
            case _ => ""
          }
          val desc = map.get("desc") match {
            case Some(Text(x)) => x
            case _ => ""
          }

          val args = map.get("args") match {
            case Some(ArgList(list)) => list
            case _ => Nil
          }
          val ex = map.get("ex") match {
            case Some(Text(x)) => Some(x)
            case _ => None
          }
          Some(Member(typeName, method, args, desc, ex))
        case MacroName("setter") =>
          val typeName = map.get("type") match {
            case Some(Text(x)) => x
            case _ => ""
          }

          val memberName = map.get("member_name") match {
            case Some(Text(x)) => x
            case _ => ""
          }

          val memberType = map.get("member_type") match {
            case Some(Text(x)) => x
            case _ => ""
          }
          val desc = map.get("desc") match {
            case Some(Text(x)) => x
            case _ => ""
          }
          val ex = map.get("ex") match {
            case Some(Text(x)) => Some(x)
            case _ => None
          }
          Some(Setter(typeName, memberName, memberType, desc, ex))
        case _ =>
          None
      }
    }
  }
}
