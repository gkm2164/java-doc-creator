package com.kakao.search.middle.godoc

import com.kakao.search.middle.godoc.doc.elements._

trait BuildGroupWriter {
  def show(fd: BuilderGroupElem): String

  def showArgs(arg: Argument): String
}

sealed trait BuilderGroupElem {
  def typeName: String

  def desc: String

  def ex: Option[String]

  def show(implicit functionWriter: BuildGroupWriter): String = functionWriter.show(this)
}

case class Constructor(typeName: String, argList: List[Argument], desc: String, ex: Option[String]) extends BuilderGroupElem

case class Method(typeName: String, method: String, argList: List[Argument], desc: String, ex: Option[String]) extends BuilderGroupElem

case class Setter(typeName: String, memberName: String, memberType: String, desc: String, ex: Option[String]) extends BuilderGroupElem


//case class DefBlock(name: String, args: Seq[Argument], desc: String, ex: String, builder: Boolean)

trait DefBlock {
  def name: String
}
case class TypeDefBlock(name: String, desc: Option[String], ex: Option[String], builder: Boolean) extends DefBlock
case class FuncDefBlock(name: String, argList: List[Argument], returnType: String, desc: Option[String], ex: Option[String]) extends DefBlock
case class ReceiverFuncDefBlock(recvName: String, recvType: String, func: FuncDefBlock) extends DefBlock {
  def name: String = func.name
}


object DocType {
  // 적당하게 Constructor, Method, Setter 타입으로 리턴
  def create(defBlocks: Seq[DefBlock]): DocType = {
    NoneType
  }
}

trait DocType
case class BuilderGroup(typeName: String, constructor: Constructor, methods: Seq[Method], setters: Seq[Method]) extends DocType
case class GeneralGroup(methods: Seq[Method]) extends DocType
case object NoneType extends DocType