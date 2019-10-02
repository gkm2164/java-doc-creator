package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.monad.{CodeWriterConfig, DebugOption}
import com.kakao.bengo.scala.javadoc.codeformatter.syntax._

object JavaCodeFormatter {
  private val CommentTokens: Seq[JavaTokenEnum] =
    Seq(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME)

  def printCode(codeName: String, tokens: Vector[JavaSToken]): String = {
    val reformatTokens = tokens.filterNot(x => CommentTokens.contains(x.tokenType)).zipWithIndex.toList
    println(s"parse ${reformatTokens.map(_._1.value).mkString(" ")}")

    if (reformatTokens.nonEmpty) {
      JavaParser.blockStmt(false)
        .collect(reformatTokens, CodeWriterConfig(debug = None))
    }
    else {
      println("there's no code here")
      ""
    }
  }
}
