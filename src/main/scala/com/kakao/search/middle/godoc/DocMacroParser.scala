package com.kakao.search.middle.godoc

import com.kakao.search.middle.godoc.doc.elements._
import com.kakao.search.middle.golang.GoParser
import com.kakao.search.middle.golang.syntax.{GoFunctionDef, GoSomeDef, GoTypeDef}

import scala.io.Source

object DocMacroParser {

  def parseComment(str: String): IncompleteBlock = {
    def recur(it: Iterator[String], acc: IncompleteBlock): IncompleteBlock = {
      if (!it.hasNext) acc
      else {
        val kv = it.next
        val (key, value) = if (kv.indexOf(':') == -1) (kv, "") else kv.splitAt(kv.indexOf(':'))
        recur(it, key match {
          case "args" => acc.copy(args = ArgList.parse(value.dropWhile(_ == ':').trim))
          case "desc" => acc.copy(desc = Text.parse(value.dropWhile(_ == ':').trim).txt)
          case "ex" => acc.copy(ex = Text.parse(value.dropWhile(_ == ':').trim).txt)
          case "builder" => acc.copy(builder = true)
          case "implements" => acc.copy(interface = Text.parse(value.dropWhile(_ == ':').trim).txt)
          case k => throw new Exception(s"unknown command $k")
        })
      }
    }

    recur(new Iterator[String] {
      var s: String = str.dropWhile(_ == '\n')

      override def hasNext: Boolean = s.startsWith("//=")

      override def next(): String = {
        val nextStr = s.drop(3)
        val nextStop = nextStr.indexOf("//=")
        val nextTake = if (nextStop == -1) nextStr.length else nextStop
        val ret = nextStr.take(nextTake)
        s = nextStr.drop(nextTake)
        ret.replaceAll("\n// ", "\n").trim
      }
    }, IncompleteBlock(TypeRequired.empty, "", "", "", builder = false))
  }

  def parse(code: String): Seq[DefBlock] = {
    val startOfDoc = "//=doc_start"
    val endOfDoc = "//=doc_end"
    val start = code.indexOf(startOfDoc)
    if (start == -1) Seq.empty
    else {
      val end = code.indexOf(endOfDoc)
      if (end == -1) {
        Seq.empty
      } else {
        val comment = code.slice(start, end)
          .replace(startOfDoc, "")
        val startOfDef = code.drop(end)
        val codeDefUntil = GoParser.findDefStop(startOfDef)
        val fDefLine = startOfDef.slice(endOfDoc.length, codeDefUntil)
        val someDefObj = GoParser.parse(fDefLine)
        val incompleteBlock = parseComment(comment)
        val block: DefBlock = incompleteBlock.resolveType(someDefObj)

        block +: parse(startOfDef.drop(endOfDoc.length))
      }
    }
  }

  def parseGoDoc(path: String): Seq[DefBlock] = {
    val file = Source.fromFile(path)
    val f = file.mkString("")

    parse(f)
  }

  case class IncompleteBlock(args: TypeRequired, interface: String, desc: String, ex: String, builder: Boolean) {

    import scala.collection.JavaConverters._


    implicit class ExtString(x: String) {
      def toOption: Option[String] = Option(x).filter(_ != "")
    }

    implicit class ToFuncDefBlockExt(x: GoFunctionDef) {
      def toFuncDefBlock: FuncDefBlock = {
        FuncDefBlock(x.funcName, args.resolveTypes(x.args.asScala.map(x => x.name -> x.typeName).toMap).toList,
          x.returnType, desc.toOption, ex.toOption)
      }
    }

    def resolveType: PartialFunction[GoSomeDef, DefBlock] = {
      case x: GoFunctionDef =>
        if (x.receiverType != "") ReceiverFuncDefBlock(x.receiverName, x.receiverType, x.toFuncDefBlock)
        else x.toFuncDefBlock
      case x: GoTypeDef => TypeDefBlock(x.typeName, x.isInterface, interface.toOption, desc.toOption, ex.toOption, builder)
    }
  }

}
