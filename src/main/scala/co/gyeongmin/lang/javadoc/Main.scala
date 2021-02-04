package co.gyeongmin.lang.javadoc

import java.io._
import co.gyeongmin.lang.javadoc.config._
import co.gyeongmin.lang.javalang.Tokenizer
import com.typesafe.scalalogging.Logger
import laika.api.Transformer
import laika.format._
import levsha.dsl._
import html._
import levsha.impl.TextPrettyPrintingConfig
import org.apache.commons.io.FileUtils

import scala.io.Source
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try

object Main {
  val log: Logger = Logger("go-doc-creator")
  val onlyJavaAndDirFilter: FilenameFilter = (dir: File, _: String) => {
    dir.isDirectory || (dir.isFile && dir.getName.endsWith(".java"))
  }

  @scala.annotation.tailrec
  def parseArg(args: List[String], doc: DocumentDescriptionBuilder, debug: DebugOptionBuilder): Either[ArgParserError, (DocumentDescription, DebugOption)] = args match {
    case Nil => Right((doc.realize, debug.realize))
    case ("-i" | "--input") :: pathName :: t =>
      parseArg(t, doc.set(_.baseDir := pathName), debug)
    case ("-o" | "--output") :: pathName :: t =>
      parseArg(t, doc.set(_.outputDir := pathName), debug)
    case ("-d" | "--desc") :: desc :: t =>
      parseArg(t, doc.set(_.description := desc), debug)
    case ("-t" | "--stack-trace") :: t =>
      parseArg(t, doc, debug.set(_.stackTrace := true))
    case ("-m" | "--max-stack-size") :: num :: t =>
      parseArg(t, doc, debug.set(_.maxStackSize := Try(num.toInt).getOrElse(1)))
    case ("-a" | "--only-accepted") :: t =>
      parseArg(t, doc, debug.set(_.printOnlyAccepted := true))
    case ("-h" | "--help") :: _ => Left(HelpMessage())
    case h :: _ => Left(UnableToIdentifyError(s"unknown parameter: $h"))
  }


  def main(args: Array[String]): Unit =
    parseArg(args.toList, new DocumentDescriptionBuilder, new DebugOptionBuilder) match {
      case Right((doc, debugOption)) =>
        val DocumentDescription(basedir, outputDir, name) = doc
        createDoc(basedir, outputDir, debugOption)
      case Left(h:HelpMessage) => h.printMessage(log)
      case Left(UnableToIdentifyError(msg)) => log.error(msg)
    }

  def removeFolderInside(files: Array[File]): Unit = {
    Option(files).foreach(_.foreach { f =>
      f match {
        case dir if dir.isDirectory =>
          removeFolderInside(dir.listFiles())
        case _ =>
      }

      f.delete()
    })
  }

  def copyResource(resourceName: String, base: String): Try[Unit] = {
    val resourceFolder = new File(s"./$resourceName")
    val targetResourceFolder = new File(s"./$base/$resourceName")
    targetResourceFolder.mkdir()
    Try(FileUtils.copyDirectory(resourceFolder, targetResourceFolder))
  }

  def copyResources(outputDir: String): Try[Unit] = for {
    _ <- copyResource("css", outputDir)
    _ <- copyResource("js", outputDir)
  } yield ()

  def createDoc(baseDir: String, outputDir: String, debugOption: DebugOption): Unit = {
    import levsha.text.renderHtml
    import levsha.text.symbolDsl._

    log.debug("parse introduction markdown")
    val introString = {
      val file = new File(s"$baseDir/INTRODUCTION.md")
      if (file.exists() && file.length() > 0)
        Transformer.from(Markdown).to(HTML).build.transform(s"$baseDir/INTRODUCTION.md").toString
      else ""
    }

    val outputDirHandle = new File(outputDir)
    if (!outputDirHandle.mkdir()) {
      removeFolderInside(outputDirHandle.listFiles())
    }

    val copyResourceResult = copyResources(outputDir)
    if (copyResourceResult.isFailure) {
      log.error("failed to copy resources[css, js]")
    } else {
      log.error("success to copy resources[css, js]")
    }

    val node = goThroughTree(new File(s"$baseDir/src/main/java"), outputDir, debugOption)

    val pw = new PrintWriter(s"$outputDir/index.html")
    pw.write("<!DOCTYPE html>")
    pw.write(renderHtml(
      Html(lang := "ko",
        head(
          meta(charset := "utf-8"),
          title("document"),
          link(`type` := "text/css", rel := "stylesheet", href := "css/animation.css"),
          link(`type` := "text/css", rel := "stylesheet", href := "css/style.css"),
          link(`type` := "text/css", rel := "stylesheet", href := "css/bootstrap.css"),
          link(`type` := "text/css", rel := "stylesheet", href := "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/styles/vs2015.min.css"),

          script(src := "js/jquery.js"),
          script(src := "js/bootstrap.js"),
          script(src := "js/main.js"),
          script(src := "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/highlight.min.js"),
          script(src := "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/java.min.js"),
          script("hljs.initHighlightingOnLoad();")
        ),
        body(
          section(
            aside(id := "sidebar", `class` := "sidebar-dark",
              nav(
                h3("Source Code Tree"),
                ul(runLogging(node.buildNavTree, log.info("build nav tree")))
              )
            ),
            div(id := "split-bar", span(`class` := "clearfix")),
            article(id := "main", `class` := "contents",
              div(introString),
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

  def goThroughTree(currentHandle: File, outputDir: String, debugOption: DebugOption): CodeNonLeaf = {
    def loop(currentHandle: File, pkgNameAcc: Seq[String]): CodeNode = {
      if (currentHandle.isFile) {
        val filename = currentHandle.getName
        val packageName = pkgNameAcc.mkString(".")
        log.info(s"lexing ${pkgNameAcc.mkString("/")}/$filename")
        val src = Source.fromFile(currentHandle.getAbsolutePath)
        val sources = src.mkString("")
        val tokens = Tokenizer.tokenize(sources).asScala
        log.info(s"parse token - # of tokens ${tokens.size}")

        val scalaTokens = tokens.map(x => JavaSToken(x.getE, x.getValue))
        CodeLeaf(filename, packageName, scalaTokens.toList, outputDir, debugOption)
      } else {
        log.info(s"entered package ${(pkgNameAcc :+ currentHandle.getName).mkString(".")}")
        CodeNonLeaf(currentHandle.getName,
          currentHandle.listFiles(onlyJavaAndDirFilter)
            .map(x => x.getName -> loop(x, pkgNameAcc :+ currentHandle.getName)).toMap)
      }
    }

    CodeNonLeaf("", currentHandle.listFiles(onlyJavaAndDirFilter).map(x => x.getName -> loop(x, Nil)).toMap)
  }
}
