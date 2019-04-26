package com.kakao.search.middle.godoc

import java.io.{File, PrintWriter}

import com.kakao.search.middle.godoc.doc.elements
import com.kakao.search.middle.godoc.doc.elements.Argument
import com.kakao.search.middle.godoc.doc.writer.FunctionWriter
import com.kakao.search.middle.godoc.doc.writer.GolangWriter._

import scala.language.{higherKinds, postfixOps}

object Main {
  implicit class ShowDefBlock(x: DefBlock) {
    def show(implicit shower: FunctionWriter): String = {
      shower.show(x)
    }
  }

  def showFolder(dirName: String): Seq[(String, Seq[DefBlock])] = {
    val d = new File(dirName)
    if (d.exists() && d.isDirectory) {
      d.listFiles.toList.flatMap {
        case x if x.isDirectory =>
          showFolder(s"$dirName/${x.getName}")
        case x if x.isFile && x.getName.endsWith(".go") =>
          Seq(s"$dirName/${x.getName}" -> DocMacroParser.parseGoDoc(s"$dirName/${x.getName}"))
        case _ => Seq.empty
      }
    } else {
      Seq.empty
    }
  }

  def printWriter(fileName: String)(f: (String => Unit) => Unit): Unit = {
    val pw = new PrintWriter(fileName)
    try {
      f(pw.write)
    } finally {
      pw.close()
    }
  }

  def main(args: Array[String]): Unit = {
    //Folder 단위로 불러들임.
    val ret = showFolder(if (args.length > 0) args(0) else "/Users/ben.go/go/src/github.daumkakao.com/search-middle/dagore")

    printWriter(s"doc.html") { w =>
      w("""<!DOCTYPE html><html><head><title>Hello</title><link type="text/css" rel="stylesheet" href="doc.css"/></head><body>""")
      ret.foreach { case (_, functionDefs) =>
        w(functionDefs.map(_.show).map(x => s"<div>$x</div>").mkString("<hr/>"))
      }
      w("</body></html>")
    }

    val bgs: List[BuilderGroup] = BuilderGroup.constructBuilderGroups(ret)

    implicit class StringExt(s: String) {
      def toLowerFirst: String = s.updated(0, s(0).toLower)
    }

    implicit val golangBuildGroupWriter: BuildGroupWriter = new BuildGroupWriter {
      val typeSet: Set[String] = bgs.groupBy(_.typeName).keySet

      def asLink(typeName: String, link: Boolean = true): String = {
        val TypeRegex = "([^a-zA-Z]*)([a-zA-Z0-9]+\\.)?([a-zA-Z0-9]+)(.*)".r
        typeName match {
          case TypeRegex(prefix, pkgName, typename, postfix) if link && typeSet.contains(typename) => s"""$prefix${Option(pkgName).getOrElse("")}<a href="#$typename" class="type-def">$typename</a>$postfix"""
          case TypeRegex(prefix, pkgName, typename, postfix) => s"""$prefix${Option(pkgName).getOrElse("")}<span class="type-def">$typename</span>$postfix"""
          case _ => typeName
        }
      }

      def reserved(word: String) = s"""<span class="reserved-keyword">$word</span>"""

      def showArg(arg: Argument): String = s"${arg.name} ${asLink(arg.typeName)}"

      override def show(fd: BuilderGroupElem): String = fd match {
        case Constructor(typeName, name, argList, desc, ex) => s"<div><p>${reserved("func")} $name(${argList.map(showArg).mkString(", ")}) *$typeName</p>${desc.map(x => s"<div>$x</div>").getOrElse("")}${ex.map(x => s"""<pre class="code">$x</pre>""").getOrElse("")}</div>"
        case Setter(typeName, memberName, memberType, desc, ex) => s"""<div>${reserved("func")} (r ${asLink(typeName, link = false)}) $memberName(${showArg(Argument(memberName.toLowerFirst, memberType, desc.getOrElse("")))}) ${asLink(typeName, link = false)}${desc.map(x => s"<div>$x</div>").getOrElse("")}${ex.map(x => s"""<pre class="code">$x</pre>""").getOrElse("")}</div>"""
        case Method(typeName, method, argList, desc, ex) => s"<div>${reserved("func")} (r ${asLink(typeName, link = false)}) $method(${argList.map(showArg).mkString(", ")}) ${asLink(typeName, link = false)}${desc.map(x => s"<div>$x</div>").getOrElse("")}${ex.map(x => s"""<pre class="code">$x</pre>""").getOrElse("")}</div>"
      }
    }

    printWriter(s"dependencies.html") { w =>
      w("""<!DOCTYPE html><html><head><title>Hello</title><link type="text/css" rel="stylesheet" href="doc.css"/></head><body>""")
      for (bg <- bgs) {

        w(s"""<a id="${bg.typeName}"><h1>for ${bg.typeName}</h1>""")
        w("<h2>생성자</h2>")
        bg.constructor.foreach(x => w(s"""<div>${x.show}</div>"""))
        //        w("<br/>")
        //        w(s"== setters for ${bg.typeName} ==")
        w("<h2>설정</h2>")
        bg.setters.foreach(x => w(s"""<div>${x.show}</div>"""))
        //        w("<br/>")
        //        w(s"== methods for ${bg.typeName} ==")
        w("<h2>일반 함수</h2>")
        bg.methods.foreach(x => w(s"""<div>${x.show}</div>"""))

        //        w("<br/>")
      }

      w("</body></html>")
    }
  }
}