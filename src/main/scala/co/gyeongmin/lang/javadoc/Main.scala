package co.gyeongmin.lang.javadoc

import java.io._

import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javalang.Tokenizer
import com.typesafe.scalalogging.Logger
import laika.api.Transform
import laika.format._
import levsha.impl.TextPrettyPrintingConfig

import scala.collection.JavaConverters._
import scala.io.Source

object Main {
  val log = Logger("go-doc-creator")
  val onlyJavaAndDirFilter: FilenameFilter = (dir: File, _: String) => {
    dir.isDirectory || (dir.isFile && dir.getName.endsWith(".java"))
  }

  case class DocumentDescription(baseDir: String, outputFile: String, description: String)

  def setField(doc: DocumentDescription, field: String, value: String): DocumentDescription = field match {
    case "baseDir" => doc.copy(baseDir = value)
    case "outputFile" => doc.copy(outputFile = value)
    case "description" => doc.copy(description = value)
    case _ => throw new RuntimeException("unknown field in DocumentDescription")
  }

  def set(desc: DocumentDescription,
          field: String,
          value: String): DocumentDescription = setField(desc, field, value)

  def defaultDoc: DocumentDescription = DocumentDescription(".", "sample.html", "Java doc creator test")

  @scala.annotation.tailrec
  def parseArg(args: List[String], doc: DocumentDescription, option: Option[DebugOption]): (DocumentDescription, Option[DebugOption]) = args match {
    case Nil => (doc, option)
    case ("-i" | "--input") :: filename :: t => parseArg(t, set(doc, "baseDir", filename), option)
    case ("-o" | "--output") :: filename :: t => parseArg(t, set(doc, "outputFile", filename), option)
    case ("-v" | "--verbose") :: t => parseArg(t, doc, Some(DebugOption(stackTrace = true)))
    case h :: _ => throw new RuntimeException(s"unknown option $h")
  }

  def main(args: Array[String]): Unit = {
    val (doc, option) = parseArg(args.toList, defaultDoc, None)
    doc match {
      case DocumentDescription(basedir, outfile, name) => createDoc(basedir, outfile, name, option)
    }
  }

  def createDoc(baseDir: String, outFile: String, name: String, debugOption: Option[DebugOption]): Unit = {
    import levsha.text.renderHtml
    import levsha.text.symbolDsl._

    log.debug("parse introduction markdown")
    val introString = {
      val file = new File(s"$baseDir/INTRODUCTION.md")
      if (file.exists() && file.length() > 0)
        Transform.from(Markdown).to(HTML).fromFile(s"$baseDir/INTRODUCTION.md").toString()
      else ""
    }

    val node = goThroughTree(new File(s"$baseDir/src/main/java"), debugOption)
    val pw = new PrintWriter(outFile)
    pw.write("<!DOCTYPE html>")
    pw.write(renderHtml(
      'html('lang /= "ko",
        'head(
          'meta('charset /= "utf-8"),
          'title("document"),
          'link('type /= "text/css", 'rel /= "stylesheet", 'href /= "css/animation.css"),
          'link('type /= "text/css", 'rel /= "stylesheet", 'href /= "css/style.css"),
          'link('type /= "text/css", 'rel /= "stylesheet", 'href /= "css/bootstrap.css"),
          'link('type /= "text/css", 'rel /= "stylesheet", 'href /= "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/styles/vs2015.min.css"),

          'script('src /= "js/jquery.js"),
          'script('src /= "js/bootstrap.js"),
          'script('src /= "js/main.js"),
          'script('src /= "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/highlight.min.js"),
          'script('src /= "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/java.min.js"),
          'script("hljs.initHighlightingOnLoad();")
        ),
        'body(
          'section(
            'aside('id /= "sidebar", 'class /= "sidebar-dark",
              'nav(
                'h3("Source Code Tree"),
                'ul(runLogging(node.buildNavTree, log.info("build nav tree")))
              )
            ),
            'div('id /= "split-bar", 'br('class /= "clearfix")),
            'article('id /= "main", 'class /= "contents",
              'div(introString),
              node.print
            )
          ),
        )
      ), TextPrettyPrintingConfig.noPrettyPrinting))
    pw.close()
  }

  def runLogging[T](buildNavTree: => T, f: => Unit): T = {
    f
    buildNavTree
  }

  def goThroughTree(currentHandle: File, debugOption: Option[DebugOption]): CodeNonLeaf = {
    def loop(currentHandle: File, pkgNameAcc: Seq[String]): CodeNode = {
      if (currentHandle.isFile) {
        val filename = currentHandle.getName
        val packageName = pkgNameAcc.mkString(".")
        log.info(s"lexing ${pkgNameAcc.mkString("/")}/$filename")
        val src = Source.fromFile(currentHandle.getAbsolutePath)
        val sources = src.mkString("")
        val tokens = Tokenizer.tokenize(sources).asScala
        log.info(s"parse token - # of tokens ${tokens.length}")

        val scalaTokens = tokens.map(x => JavaSToken(x.getE, x.getValue))
        CodeLeaf(filename, packageName, scalaTokens.toList, debugOption)
      } else {
        log.info(s"entered package ${(pkgNameAcc :+ currentHandle.getName).mkString(".")}")
        CodeNonLeaf(currentHandle.getName,
          currentHandle.listFiles(onlyJavaAndDirFilter)
            .map(x => x.getName -> loop(x, pkgNameAcc :+ currentHandle.getName)).toMap)
      }
    }

    CodeNonLeaf("", currentHandle.listFiles(onlyJavaAndDirFilter).map(x => x.getName -> loop(x, Nil)).toMap)
  }

  def getType(currentHandle: File): String = if (currentHandle.isFile) "FILE" else if (currentHandle.isDirectory) "DIRECTORY" else "UNKNOWN"

  case class PrintOption(rawMethodBody: Boolean)

}
