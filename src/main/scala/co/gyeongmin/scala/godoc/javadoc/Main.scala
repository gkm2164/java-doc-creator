package co.gyeongmin.scala.godoc.javadoc

import java.io.{File, FilenameFilter, PrintWriter}

import co.gyeongmin.scala.godoc.javalang.Tokenizer

import scala.collection.JavaConverters._
import scala.io.Source

object Main {
  val onlyJavaAndDirFilter: FilenameFilter = (dir: File, name: String) => {
    println(name)
    dir.isDirectory || (dir.isFile && dir.getName.endsWith(".java"))
  }

  def main(args: Array[String]): Unit = {
//    val baseDir = "/Users/ben.go/go-doc-creator/src/main/java"
    val baseDir = "/Users/ben.go/java/da-commons/da-intent-handler/src/main/java"
    val node = currentPackage(new File(baseDir))
    val pw = new PrintWriter("javadoc.html")
    pw.write("""<!DOCTYPE html><html><head><meta charset="UTF-8" /><title>document</title><link type="text/css" rel="stylesheet" href="doc.css" /> </head><body>""")
    node.print(pw)
    pw.write("</body></html>")
    pw.close()
  }

  def currentPackage(currentHandle: File): CodeNonLeaf = {
    def loop(currentHandle: File, pkgNameAcc: Seq[String]): CodeNode = {
      if (currentHandle.isFile) {
        val filename = currentHandle.getName
        val packageName = pkgNameAcc.mkString(".")
        println(s"file name: ${pkgNameAcc.mkString(".")}.$filename")
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
