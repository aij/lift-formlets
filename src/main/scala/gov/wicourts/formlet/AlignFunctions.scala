package gov.wicourts.formlet

import scala.language.higherKinds
import scalaz.{Monad, Align}
import scalaz.\&/.{Both, That, This}

object AlignFunctions {
  def combine[F[_],A](
    a1: F[A], a2: F[A]
  )(
    f: (A, A) => F[A]
  )(
    implicit F: Align[F], M: Monad[F]
  ): F[A] = {
    M.join(F.alignWith[A,A,F[A]](_ match {
      case This(a) => M.point(a)
      case That(a) => M.point(a)
      case Both(a1, a2) => f(a1, a2)
    })(a1, a2))
  }
}
