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
    List(("/Users/ben.go/go-doc-creator/src/main/java", "javadoc-tokenizer.html", "Doc code generator"),
      ("/Users/ben.go/java/da-commons/da-intent-handler/src/main/java", "javadoc-da-commons-intent-handler.html", "DA commons 문서"),
      ("/Users/ben.go/java/da-core/src/main/java", "javadoc-dacore.html", "DA core 문서"))
      .foreach { case (basedir, outfile, name) => createDoc(basedir, outfile, name) }
  }

  def createDoc(baseDir: String, outFile: String, name: String): Unit = {
    import levsha.text.symbolDsl._
    import levsha.text.renderHtml

    val node = currentPackage(new File(baseDir))
    val pw = new PrintWriter(outFile)
    pw.write("<!DOCTYPE html>")
    pw.write(renderHtml(
      'html ('lang /= "ko",
        'head (
          'meta ('charset /= "utf-8"),
          'title ("document"),
          'link ('type /= "text/css", 'rel /= "stylesheet", 'href /= "css/animation.css"),
          'link ('type /= "text/css", 'rel /= "stylesheet", 'href /= "css/style.css"),
          'link ('type /= "text/css", 'rel /= "stylesheet", 'href /= "css/bootstrap.css"),
          'link ('type /= "text/css", 'rel /= "stylesheet", 'href /= "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/styles/vs2015.min.css"),
          'script ('src /= "js/jquery.js"),
          'script ('src /= "js/bootstrap.js"),
          'script ('src /= "js/main.js"),
          'script ('src /= "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/highlight.min.js"),
          'script ('src /= "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/java.min.js"),
          'script ("hljs.initHighlightingOnLoad();")
        ),
        'body (
          //          'header ("DA Commons Documentation for Kakao Mini"),
          'section (
            'aside ('id /= "sidebar", 'class /= "sidebar-dark",
              'nav (
                'h3 ("Package List"),
                'ul (node.buildNavTree)
              )
            ),
            'div ('id /= "split-bar", 'br ('class /= "clearfix")),
            'article ('id /= "main", 'class /= "contents",
              'h1 (name),
              node.print
            )
          ),

        )
      )
      , TextPrettyPrintingConfig.noPrettyPrinting))
    pw.close()

    node.createHashMap.keys.toList.sorted.foreach(println)
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
