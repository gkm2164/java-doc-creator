package co.gyeongmin.lang.tokenizer

object JavaToken extends Enumeration {
  protected case class JavaTokenVal(value: String,
                                    isKeyword: Boolean,
                                    until: Option[String],

                                   ) extends Val
}
