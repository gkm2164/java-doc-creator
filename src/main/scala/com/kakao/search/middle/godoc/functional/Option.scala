package com.kakao.search.middle.godoc.functional

//
//sealed trait Option[+A] {
//
//  def flatMap[B](f: A => Option[B]): Option[B] = this match {
//    case None => None
//    case Some(x) => f(x)
//  }
//
//  def map[B](f: A => B): Option[B] = this match {
//    case None => None
//    case Some(x) => Some(f(x))
//  }
//}
//
//case object None extends Option[Nothing]
//case class Some[+A](value: A) extends Option[A]
//
//object Option {
//  def map2[A, B, C](x: Option[A], y: Option[B])(f: (A, B) => C): Option[C] = x.flatMap(a => y.map(b => f(a, b)))
//
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
//}