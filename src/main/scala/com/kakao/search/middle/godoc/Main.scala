package com.kakao.search.middle.godoc

import java.io.File

import com.kakao.search.middle.ArgParser

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source

object Main {
  def parseGoDoc(path: String): Seq[Map[String, ArrayElem]] = {
    val file = Source.fromFile(path)
    val f = file.mkString("")

    def parseComment(str: String): Map[String, ArrayElem] = {
      def parseArgValue(str: String): ArgList = {
        def parseArgValue(str: String, acc: Seq[ArgList]): ArgList = {
          val result: mutable.Seq[ArgParser.Argument] = ArgParser.parseArgList(str).asScala
          ArgList(result map { x => Argument(x.name.trim, x.typeName.trim, x.desc.trim) } toList)
        }

        parseArgValue(str, Seq.empty)
      }

      def parseComment(str: Iterator[String], acc: Map[String, ArrayElem]): Map[String, ArrayElem] = {
        if (str.hasNext) {
          val kv = str.next
          val nameRegex = "([a-zA-Z0-9_]+): "
          val MacroParser = s"$nameRegex*<([^>]+)>".r
          val ArgParser = s"$nameRegex*\\[([^\\]]+)\\]".r
          val LineParser = s"$nameRegex*(.*)".r
          kv match {
            case MacroParser(name, value) =>
              parseComment(str, acc + (name -> MacroName(value)))
            case ArgParser(name, value) =>
              val cleanedValue = value.replaceAll("\n// *", "")
              val values: ArgList = parseArgValue(cleanedValue)
              parseComment(str, acc + (name -> values))


            case LineParser(name, value) => parseComment(str, acc + (name -> Text(value)))
            case _ => parseComment(str, acc)
          }
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
          val nextTake = if (nextStop == -1) s.length else nextStop
          val ret = nextStr.take(nextTake)

          s = nextStr.drop(nextTake)
          ret.replaceAll("^//( +)", "\n").trim
        }
      }, Map.empty)
    }

    def parseCode(code: String): Seq[Map[String, ArrayElem]] = {
      val startOfDoc = "//=dagore_doc"
      val endOfDoc = "//=end"
      val start = code.indexOf(startOfDoc)
      if (start == -1) Seq.empty
      else {
        val end = code.indexOf(endOfDoc)
        if (end == -1) {
          Seq.empty
        } else {
          val comment = code.slice(start, start + end - start)
            .replace(startOfDoc, "")

          val map: Map[String, ArrayElem] = if (comment != "") {
            parseComment(comment)
          } else {
            Map.empty
          }

          map +: parseCode(code.drop(end + endOfDoc.length))
        }
      }
    }

    parseCode(f)
  }

  def main(args: Array[String]): Unit = {
    //Folder 단위로 불러들임.
    val orgDirName = "/Users/kakao/go/src/github.daumkakao.com/search-middle/dagore"

    def showFolder(dirName: String): Seq[(String, Seq[FunctionDefinition])] = {
      val d = new File(dirName)
      if (d.exists() && d.isDirectory) {
        d.listFiles.toList.flatMap {
          case x if x.isDirectory => showFolder(s"$dirName/${x.getName}")
          case x if x.isFile && x.getName.endsWith(".go") => Seq(s"$dirName/${x.getName}" -> FunctionDefinition.create(parseGoDoc(s"$dirName/${x.getName}")))
          case _ => Seq.empty
        }
      } else {
        Seq.empty
      }

    }

    implicit val golangWriter: FunctionWriter = new FunctionWriter {
      override def show(fd: FunctionDefinition): String = fd match {
        case Constructor(typeName, desc, argList) => s"func New$typeName(${argList.map(showArgs).mkString(", ")}) *${typeName}Builder\n$desc"
        case Member(typeName, method, desc, argList) => s"func (*${typeName}Builder) $method(${argList.map(showArgs).mkString(", ")}) *${typeName}Builder\n$desc"
        case Setter(typeName, memberName, memberType, desc) => s"func (*${typeName}Builder) ${memberName.updated(0, memberName(0).toUpper)}($memberName $memberType) *${typeName}Builder\n$desc"
      }

      override def showArgs(x: Argument): String = s"${x.name} ${x.t}"
    }

    println(showFolder(orgDirName).toMap.filter(_._2.nonEmpty).foreach(_._2.foreach(x => println(x.show))))

  }
}