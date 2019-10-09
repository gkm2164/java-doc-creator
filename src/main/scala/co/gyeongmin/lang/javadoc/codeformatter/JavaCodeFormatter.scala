package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javadoc.config.DebugOption
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._
import com.typesafe.scalalogging.Logger

object JavaCodeFormatter {

  import syntax._

  private val CommentTokens: Set[JavaTokenEnum] =
    Set(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME)

  private def printCodeCommon[A](codeName: String,
                                 tokens: Vector[JavaSToken],
                                 startFrom: CodeWriter[A],
                                 debugOption: DebugOption)(implicit log: Logger): String = {
    val reformatTokens = tokens.filterNot(x => CommentTokens.contains(x.tokenType)).zipWithIndex.toList

    if (reformatTokens.nonEmpty) {
      startFrom.enter()
        .collect(reformatTokens,
          CodeWriterConfig(debug = debugOption, logger = Some(new CodeWriterLogger)))
    } else {
      log.info("seems to be an interface/abstract method")
      ""
    }
  }

  def printCodeBlock(codeName: String, tokens: Vector[JavaSToken], debugOption: DebugOption)(implicit log: Logger): String =
    printCodeCommon(codeName, tokens, JavaParser.blockStmt, debugOption)


  def printCode(codeName: String, tokens: Vector[JavaSToken], debugOption: DebugOption)(implicit log: Logger): String =
    printCodeCommon(codeName, tokens, JavaParser.javaCode, debugOption)
}
