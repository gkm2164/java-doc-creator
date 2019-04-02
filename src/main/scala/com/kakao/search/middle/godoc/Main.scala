package com.kakao.search.middle.godoc

import java.io.File

import scala.language.{higherKinds, postfixOps}

object Main {
  def main(args: Array[String]): Unit = {
    //Folder 단위로 불러들임.
    val orgDirName = "/Users/ben.go/go/src/github.daumkakao.com/search-middle/dagore"

    def showFolder(dirName: String): Seq[(String, Seq[FunctionDefinition])] = {
      val d = new File(dirName)
      if (d.exists() && d.isDirectory) {
        d.listFiles.toList.flatMap {
          case x if x.isDirectory => showFolder(s"$dirName/${x.getName}")
          case x if x.isFile && x.getName.endsWith(".go") => Seq(s"$dirName/${x.getName}" -> FunctionDefinition.create(DocMacroParser.parseGoDoc(s"$dirName/${x.getName}")))
          case _ => Seq.empty
        }
      } else {
        Seq.empty
      }
    }

    implicit val golangWriter: FunctionWriter = new FunctionWriter {
      def repEx(exBlock: Option[String]): String = exBlock match {
        case Some(x) => s"<pre>$x</pre>"
        case None => ""
      }


      override def show(fd: FunctionDefinition): String = fd match {
        case Constructor(typeName, argList, desc, ex) =>
          s"func New$typeName(${argList.map(showArgs).mkString(", ")}) *${typeName}Builder\n$desc\n\n${repEx(ex)}"
        case Member(typeName, method, argList, desc, ex) =>
          s"func (*${typeName}Builder) $method(${argList.map(showArgs).mkString(", ")}) *${typeName}Builder\n$desc\n\n${repEx(ex)}"
        case Setter(typeName, memberName, memberType, desc, ex) =>
          s"func (*${typeName}Builder) ${memberName.updated(0, memberName(0).toUpper)}($memberName $memberType) *${typeName}Builder\n$desc\n\n${repEx(ex)}"
      }

      override def showArgs(x: Argument): String = s"${x.name} __"
    }

    println(showFolder(orgDirName).toMap.filter(_._2.nonEmpty).foreach(_._2.foreach(x => println(x.show))))

  }
}