package com.kakao.search.middle.godoc


sealed trait ArrayElem

case class TypeName(name: String) extends ArrayElem

case class MacroName(name: String) extends ArrayElem

case class Text(txt: String) extends ArrayElem

case class ArgList(args: List[Argument]) extends ArrayElem

case class Argument(name: String, t: String, desc: String) extends ArrayElem

