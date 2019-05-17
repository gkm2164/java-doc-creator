package com.kakao.bengo.scala.functional

import scala.language.higherKinds

trait Monad[M[_]] {
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => unit(f(x)))
  def unit[A](x: A): M[A]
}

object MonadSyntax {
  implicit class MonadSyntax[M[_]: Monad, A](ma: M[A]) {
    val monad: Monad[M] = implicitly[Monad[M]]

    def map[B](f: A => B): M[B] = monad.map(ma)(f)
    def flatMap[B](f: A => M[B]): M[B] = monad.flatMap(ma)(f)
  }
}