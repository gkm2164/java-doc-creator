package com.kakao.search.middle.functional

import com.kakao.search.middle.javadoc.JavaSToken

case class TokenListState[+A](f: List[JavaSToken] => (A, List[JavaSToken])) {
  def run(tokens: List[JavaSToken]): (A, List[JavaSToken]) = {
    f(tokens)
  }
}

object TokenListState {
  def unit[A](x: A) = TokenListState(tokens => (x, tokens))
}

object TokenListStateBehavior {
  implicit def tokenListStateMonad: Monad[TokenListState] = new Monad[TokenListState] {
    override def flatMap[A, B](ma: TokenListState[A])(f: A => TokenListState[B]): TokenListState[B] = TokenListState(tokens => {
      val (res, next) = ma.run(tokens)
      f(res).run(next)
    })

    override def unit[A](x: A): TokenListState[A] = TokenListState(tokens => (x, tokens))
  }
}