package co.gyeongmin.lang.tokenizer

object JavaToken extends Enumeration {
  protected case class JavaTokenVal(
      value: String,
      isKeyword: Boolean,
      until: Option[String],
      var saveTo: Option[JavaToken],
      var allowEscape: Boolean
  ) extends Val

  object JavaTokenVal {
    def apply(value: String): JavaTokenVal =
      JavaTokenVal(
        value = value,
        isKeyword = false,
        until = None,
        saveTo = null,
        allowEscape = false
      )

    def apply(value: String, isKeyword: Boolean): JavaTokenVal =
      JavaTokenVal(
        value = value,
        isKeyword = isKeyword,
        until = None,
        saveTo = None,
        allowEscape = false
      )

    def apply(value: String, until: String): JavaTokenVal =
      JavaTokenVal(
        value = value,
        isKeyword = false,
        until = Some(until),
        saveTo = None,
        allowEscape = false
      )

    def apply(
      value: String,
      until: String,
      allowEscape: Boolean
    ): JavaTokenVal =
      JavaTokenVal(
        value = value,
        isKeyword = false,
        until = Some(until),
        saveTo = None,
        allowEscape = allowEscape
      )

    def apply(value: String, until: String, saveTo: JavaToken): JavaTokenVal =
      JavaTokenVal(
        value = value,
        isKeyword = false,
        until = Some(until),
        saveTo = Some(saveTo),
        allowEscape = false
      )
  }

  type JavaToken = Value
  val ABSTRACT = JavaTokenVal("abstract", isKeyword = true)
}
