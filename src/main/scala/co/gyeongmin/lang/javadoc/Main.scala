package co.gyeongmin.lang.javadoc

import java.io._

import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javadoc.config.{DebugOption, DebugOptionBuilder, DocumentDescription, DocumentDescriptionBuilder}
import co.gyeongmin.lang.javalang.Tokenizer
import com.typesafe.scalalogging.Logger
import laika.api.Transform
import laika.format._
import levsha.impl.TextPrettyPrintingConfig

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.Try

object Main {
  val log = Logger("go-doc-creator")
  val onlyJavaAndDirFilter: FilenameFilter = (dir: File, _: String) => {
    dir.isDirectory || (dir.isFile && dir.getName.endsWith(".java"))
  }

  @scala.annotation.tailrec
  def parseArg(args: List[String], doc: DocumentDescriptionBuilder, debug: DebugOptionBuilder): (DocumentDescription, DebugOption) = args match {
    case Nil => (doc.realize, debug.realize)
    case ("-i" | "--input") :: pathName :: t =>
      parseArg(t, doc.set(_.baseDir := pathName), debug)
    case ("-o" | "--output") :: pathName :: t =>
      parseArg(t, doc.set(_.outputDir := pathName), debug)
    case ("-d" | "--desc") :: desc :: t =>
      parseArg(t, doc.set(_.description := desc), debug)
    case ("-t" | "--stack-trace") :: t =>
      parseArg(t, doc, debug.set(_.stackTrace := true))
    case ("-m=" | "--max-stack-size=") :: num :: t =>
      parseArg(t, doc, debug.set(_.maxStackSize := Try(num.toInt).getOrElse(1)))
    case ("-a" | "--only-accept") :: t =>
      parseArg(t, doc, debug.set(_.printOnlyAccepted := true))
    case h :: _ => throw new RuntimeException(s"unknown option $h")
  }

  def main(args: Array[String]): Unit = {
    val (doc, debugOption) = parseArg(args.toList, new DocumentDescriptionBuilder, new DebugOptionBuilder)
    println(doc)
    doc match {
      case DocumentDescription(basedir, outfile, name) => createDoc(basedir, outfile, name, debugOption)
    }
  }

  def createDoc(baseDir: String, outFile: String, name: String, debugOption: DebugOption): Unit = {
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

  def goThroughTree(currentHandle: File, debugOption: DebugOption): CodeNonLeaf = {
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
