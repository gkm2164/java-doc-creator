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

    def binder(x: Target, v: T): Target

    def bind(x: Target): Target = binder(x, getOrDefault)

    def getOrDefault: T = thisValue.getOrElse(defaultValue)
  }

  implicit def partialSettingKeyImplicit[Target]: Monad[SettingKey[*, Target]] = new Monad[SettingKey[*, Target]] {
    override def flatMap[A, B](fa: SettingKey[A, Target])(f: A => SettingKey[B, Target]): SettingKey[B, Target] = new SettingKey[B, Target] {
      private val currentValue = fa.getOrDefault
      private val nextSetter = f(currentValue)

      override def defaultValue: B = nextSetter.getOrDefault

      override def binder(x: Target, v: B): Target = nextSetter.binder(fa.binder(x, currentValue), v)
    }

    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => SettingKey[Either[A, B], Target]): SettingKey[B, Target] =
      f(a).getOrDefault match {
        case Left(v) => tailRecM(v)(f)
        case Right(v) => pure(v)
      }

    override def pure[A](v: A): SettingKey[A, Target] = SettingKey((x, _) => x, v)
  }

  implicit class SettingKeyMonadSyntax[A, Target](fa: SettingKey[A, Target]) {
    private val F = implicitly[Monad[SettingKey[*, Target]]]

    def flatMap[B](f: A => SettingKey[B, Target]): SettingKey[B, Target] = F.flatMap(fa)(f)

    def map[B](f: A => B): SettingKey[B, Target] = F.map(fa)(f)
  }

  trait BuilderType[T] {
    final def realize: T = combine.foldRight(empty)(_.bind(_))

    def empty: T

    def combine: List[SettingKey[_, T]]
  }

  sealed trait DescriptorType

  implicit class DescriptionBuilderClass[T <: BuilderType[_]](self: T) {
    def set(f: T => Unit): T = {
      f(self)
      self
    }
  }

  case class DocumentDescription(baseDir: String,
                                 outputDir: String,
                                 description: String) extends DescriptorType

  case class DebugOption(stackTrace: Boolean,
                         maxStackSize: Int,
                         printOnlyAccepted: Boolean) extends DescriptorType

  class DocumentDescriptionBuilder extends BuilderType[DocumentDescription] {
    val baseDir: SettingKey[String, DocumentDescription] =
      SettingKey((self, value) => self.copy(baseDir = value), ".")
    val outputDir: SettingKey[String, DocumentDescription] =
      SettingKey((self, value) => self.copy(outputDir = value), "output.html")
    val description: SettingKey[String, DocumentDescription] =
      SettingKey((self, value) => self.copy(description = value), "default description")
    val empty = DocumentDescription("", "", "")

    val combine: List[SettingKey[_, DocumentDescription]] = baseDir :: outputDir :: description :: Nil
  }

  class DebugOptionBuilder extends BuilderType[DebugOption] {
    val stackTrace: SettingKey[Boolean, DebugOption] =
      SettingKey((self, value) => self.copy(stackTrace = value), false)
    val maxStackSize: SettingKey[Int, DebugOption] =
      SettingKey((self, value) => self.copy(maxStackSize = value), 1)
    val printOnlyAccepted: SettingKey[Boolean, DebugOption] =
      SettingKey((self, value) => self.copy(printOnlyAccepted = value), false)

    val empty = DebugOption(stackTrace = false, 0, printOnlyAccepted = false)
    val combine: List[SettingKey[_, DebugOption]] = stackTrace :: maxStackSize :: printOnlyAccepted :: Nil
  }

  object SettingKey {
    def apply[T, Target](givenBinder: (Target, T) => Target, givenDefaultValue: T): SettingKey[T, Target] = new SettingKey[T, Target] {
      override def defaultValue: T = givenDefaultValue

      override def binder(x: Target, v: T): Target = givenBinder(x, v)
    }
  }
}
