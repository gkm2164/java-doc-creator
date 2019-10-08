package co.gyeongmin.lang.javadoc

import cats.Monad

package object config {

  trait SettingKey[T, Target] {
    self =>
    var thisValue: Option[T] = None

    def defaultValue: T

    def :=(value: T): Unit = {
      thisValue = Option(value)
    }

    def bind(x: Target, v: T): Target

    def getOrDefault: T = thisValue.getOrElse(defaultValue)
  }

  implicit def partialSettingKeyImplicit[Target]: Monad[SettingKey[_, Target]] = new Monad[SettingKey[*, Target]] {
    override def flatMap[A, B](fa: SettingKey[A, Target])(f: A => SettingKey[B, Target]): SettingKey[B, Target] = new SettingKey[B, Target] {
      private val nextSetter = f(fa.getOrDefault)

      override def defaultValue: B = f(fa.getOrDefault).getOrDefault

      override def bind(x: Target, v: B): Target = nextSetter.bind(fa.bind(x, fa.getOrDefault), getOrDefault)
    }

    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => SettingKey[Either[A, B], Target]): SettingKey[B, Target] =
      f(a).getOrDefault match {
      case Left(v) => tailRecM(v)(f)
      case Right(v) => pure(v)
    }

    override def pure[A](v: A): SettingKey[A, Target] = SettingKey((x, _) => x, v)
  }

  implicit class DescriptionBuilderClass[T](self: T) {
    def set(f: T => Unit): T = {
      f(self)
      self
    }
  }

  implicit class SettingKeyMonadSyntax[A, Target](fa: SettingKey[A, Target]) {
    private val F = implicitly[Monad[SettingKey[*, Target]]]
    def flatMap[B](f: A => SettingKey[B, Target]): SettingKey[B, Target] = F.flatMap(fa)(f)
    def map[B](f: A => B): SettingKey[B, Target] = F.map(fa)(f)
  }

  case class DocumentDescription(baseDir: String, outputDir: String, description: String)

  class DocumentDescriptionBuilder {
    val baseDir: SettingKey[String, DocumentDescription] =
      SettingKey((self, value) => self.copy(baseDir = value), ".")
    val outputDir: SettingKey[String, DocumentDescription] =
      SettingKey((self, value) => self.copy(outputDir = value), "./output")
    val description: SettingKey[String, DocumentDescription] =
      SettingKey((self, value) => self.copy(description = value), "default description")
    val empty = DocumentDescription("", "", "")

    def realize: DocumentDescription = combine.bind(empty, empty)

    private def combine: SettingKey[DocumentDescription, DocumentDescription] = for {
      baseDir <- this.baseDir
      outputDir <- this.outputDir
      description <- this.description
    } yield DocumentDescription(baseDir, outputDir, description)
  }

  case class DebugOption(stackTrace: Boolean,
                         maxStackSize: Int,
                         printOnlyAccepted: Boolean)

  class DebugOptionBuilder {
    val stackTrace: SettingKey[Boolean, DebugOption] =
      SettingKey((self, value) => self.copy(stackTrace = value), false)
    val maxStackSize: SettingKey[Int, DebugOption] =
      SettingKey((self, value) => self.copy(maxStackSize = value), 1)
    val printOnlyAccepted: SettingKey[Boolean, DebugOption] =
      SettingKey((self, value) => self.copy(printOnlyAccepted = value), false)
    val empty = DebugOption(stackTrace = false, 0, printOnlyAccepted = false)

    def realize: DebugOption = combine.bind(empty, empty)

    private def combine: SettingKey[DebugOption, DebugOption] = for {
      stackTrace <- this.stackTrace
      maxStackSize <- this.maxStackSize
      printOnlyAccepted <- this.printOnlyAccepted
    } yield DebugOption(stackTrace, maxStackSize, printOnlyAccepted)
  }

  object SettingKey {
    def apply[T, Target](binder: (Target, T) => Target, givenDefaultValue: T): SettingKey[T, Target] = new SettingKey[T, Target] {
      override def defaultValue: T = givenDefaultValue

      override def bind(x: Target, v: T): Target = binder(x, v)
    }
  }

  object DocumentDescriptionBuilder {
    def apply(): DocumentDescriptionBuilder = new DocumentDescriptionBuilder
  }

}
