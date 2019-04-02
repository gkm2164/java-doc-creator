package com.kakao.search.middle.godoc

import com.kakao.search.middle.{GoParser, MacroArgParser}

import scala.collection.JavaConverters._
import scala.io.Source

object DocMacroParser {


  def parseComment(str: String): Map[String, ArrayElem] = {
    def parseArgValue(str: String): Map[String, String => Argument] =
      MacroArgParser.parseArgList(str).asScala.map { x =>
        println(" => " + x.name + " :: " + x.desc)
        x.name.trim -> ((tn: String) => Argument(x.name.trim, tn, x.desc.trim))
      }.toMap


    def parseComment(it: Iterator[String], acc: Map[String, ArrayElem]): Map[String, ArrayElem] = {
      if (it.hasNext) {
        val kv = it.next
        val (key, value) = kv.splitAt(kv.indexOf(':'))
        val v: (String, ArrayElem) = key match {
          case "args" => "args" -> ArgList.parse(value.dropWhile(_ == ':').trim)
          case "desc" => "desc" -> Text.parse(value.dropWhile(_ == ':').trim)
          case "ex" => "ex" -> Text.parse(value.dropWhile(_ == ':').trim)
          case _ => throw new Exception(s"unknown command $key")
        }
        parseComment(it, acc + v)
//
//        val ArgParser = s"args: \\[([^\\]]+)\\]".r
//        val nameRegex = "([a-zA-Z0-9_]+): *"
//        val MacroParser = s"$nameRegex<([^>]+)>".r
//        val LineParser = s"$nameRegex(.*)".r
//        val BlockParser = s"$nameRegex\n* *\\{([^\\}]+)\\}".r
//        kv match {
//          case ArgParser(value) =>
//            val cleanedValue = value.replaceAll("\n// *", "")
//            val values: Map[String, String => Argument] = parseArgValue(cleanedValue)
//            parseComment(str, acc + ("args" -> TypeRequired(values)))
//          case MacroParser(name, value) =>
//            parseComment(str, acc + (name -> MacroName(value)))
//          case BlockParser(name, body) => parseComment(str, acc + (name -> Text(body.split("\n").map(_.replaceFirst("//", "")).mkString("\n"))))
//          case LineParser(name, value) => parseComment(str, acc + (name -> Text(value)))
//          case _ => parseComment(str, acc)
//        }
      } else {
        acc
      }
    }

    parseComment(new Iterator[String] {
      var s: String = str.dropWhile(_ == '\n')

      override def hasNext: Boolean = s.startsWith("//=")

      override def next(): String = {
        val nextStr = s.drop(3)
        val nextStop = nextStr.indexOf("//=")
        val nextTake = if (nextStop == -1) nextStr.length else nextStop
        val ret = nextStr.take(nextTake)

        s = nextStr.drop(nextTake)
        ret.replaceAll("\n//( +)", "\n").trim
      }
    }, Map.empty)
  }


  def parse(code: String): Seq[Map[String, ArrayElem]] = {
    val startOfDoc = "//=doc_start"
    val endOfDoc = "//=doc_end"
    val start = code.indexOf(startOfDoc)
    if (start == -1) Seq.empty
    else {
      val end = code.indexOf(endOfDoc)
      if (end == -1) {
        Seq.empty
      } else {
        val comment = code.slice(start, start + end - start)
          .replace(startOfDoc, "")
        val codeDefUntil = GoParser.findDefStop(code.drop(end))
        val fDefLine = code.slice(end + endOfDoc.length, end + codeDefUntil)
        val fDefObj = GoParser.parse(fDefLine)
        println(fDefObj.printAsGoStyle())

        val map: Map[String, ArrayElem] = if (comment != "") {
          parseComment(comment)
        } else {
          Map.empty
        }

        map +: parse(code.drop(end + endOfDoc.length))
      }
    }
  }

  def parseGoDoc(path: String): Seq[Map[String, ArrayElem]] = {
    val file = Source.fromFile(path)
    val f = file.mkString("")

    println(s"=== at file: $path === ")

    parse(f)
  }

}
