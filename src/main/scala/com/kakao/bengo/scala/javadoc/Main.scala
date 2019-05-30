package com.kakao.bengo.scala.javadoc

import java.io.{File, FilenameFilter, PrintWriter}

import com.kakao.bengo.javalang.Tokenizer
import levsha.impl.TextPrettyPrintingConfig

import scala.collection.JavaConverters._
import scala.io.Source

object Main {
  val onlyJavaAndDirFilter: FilenameFilter = (dir: File, name: String) => {
    println(name)
    dir.isDirectory || (dir.isFile && dir.getName.endsWith(".java"))
  }

  def main(args: Array[String]): Unit = {
    List(("/Users/ben.go/go-doc-creator/src/main/java", "javadoc-tokenizer.html"),
      ("/Users/ben.go/java/da-commons/da-intent-handler/src/main/java", "javadoc-da-commons-intent-handler.html"),
      ("/Users/ben.go/java/da-core/src/main/java", "javadoc-dacore.html"))
      .foreach { case (basedir, outfile) => createDoc(basedir, outfile) }
  }

  def createDoc(baseDir: String, outFile: String): Unit = {
    import levsha.text.symbolDsl._
    import levsha.text.renderHtml

    val node = currentPackage(new File(baseDir))
    val pw = new PrintWriter(outFile)
    pw.write("<!DOCTYPE html>")
    pw.write(renderHtml(
      'html ('lang /= "ko",
        'head (
          'meta ('charset /= "UTF-8"),
          'title ("document"),
          'link ('type /= "text/css", 'rel /= "stylesheet", 'href /= "doc.css")
        ),
        'body (
          'nav (node.buildNavTree),
          'div ('class /= "contents",
            'h1 ("Documentation"),
            node.print
          )
        )
      )
      , TextPrettyPrintingConfig.noPrettyPrinting))
    pw.close()
  }

  def currentPackage(currentHandle: File): CodeNonLeaf = {
    def loop(currentHandle: File, pkgNameAcc: Seq[String]): CodeNode = {
      if (currentHandle.isFile) {
        val filename = currentHandle.getName
        val packageName = pkgNameAcc.mkString(".")
        println(s"file name: ${pkgNameAcc.mkString("/")}/$filename")
        val src = Source.fromFile(currentHandle.getAbsolutePath)
        val tokens = Tokenizer.tokenize(src.mkString("")).asScala
        CodeLeaf(filename, packageName,
          tokens.map(x => JavaSToken(x.getE, x.getValue)).toList)
      } else {
        println(s"package name: ${pkgNameAcc.mkString(".")}.${currentHandle.getName}")
        CodeNonLeaf(currentHandle.getName,
          currentHandle.listFiles(onlyJavaAndDirFilter).map(x => x.getName -> loop(x, pkgNameAcc :+ currentHandle.getName)).toMap)
      }
    }

    CodeNonLeaf("", currentHandle.listFiles(onlyJavaAndDirFilter).map(x => x.getName -> loop(x, Nil)).toMap)
  }

  def getType(currentHandle: File): String = if (currentHandle.isFile) "FILE" else if (currentHandle.isDirectory) "DIRECTORY" else "UNKNOWN"
}
