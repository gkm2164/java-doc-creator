package com.kakao.search.middle.godoc

trait FunctionWriter {
  def show(fd: FunctionDefinition): String
  def showArgs(arg: Argument): String
}

sealed trait FunctionDefinition {
  def show(implicit functionWriter: FunctionWriter): String = functionWriter.show(this)
}

case class Constructor(typeName: String, desc: String, argList: List[Argument]) extends FunctionDefinition
case class Member(typeName: String, method: String, desc: String, argList: List[Argument]) extends FunctionDefinition
case class Setter(typeName: String, memberName: String, memberType: String, desc: String) extends FunctionDefinition

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

          Some(Constructor(typeName, desc, argList))
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
          Some(Member(typeName, method, desc, args))
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
          Some(Setter(typeName, memberName, memberType, desc))
        case _ =>
          None
      }
    }
  }
}
