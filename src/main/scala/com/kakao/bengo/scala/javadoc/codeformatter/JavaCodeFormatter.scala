package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.monad.{CodeWriterConfig, DebugOption}
import com.kakao.bengo.scala.javadoc.codeformatter.syntax._

object JavaCodeFormatter {
  private val CommentTokens: Seq[JavaTokenEnum] =
    Seq(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME)

  def printCode(codeName: String, tokens: Vector[JavaSToken]): Unit = {
    val reformatTokens = tokens.filterNot(x => CommentTokens.contains(x.tokenType))
    println(s"parse ${reformatTokens.map(_.value).mkString(" ")}")
    println(JavaParser.blockStmt(false)
                      .collect(reformatTokens.toList,
                        CodeWriterConfig(debug = Some(DebugOption(stackTrace = true)))))
  }
}
