package schrodinger

import scala.concurrent.duration.FiniteDuration
import cats.effect.Ref
import cats.effect.Concurrent
import cats.syntax.all.*
import State.*
import cats.data.EitherT
import cats.effect.Clock

trait CircuitBreaker[F[_]] {

  /** Protect the function f with a circuit breaker
    *
    * In closed state, function f is executed. In open state, the circuit breaker will not allow the function f to be executed. In half-open state,
    * the circuit breaker can retry the function f, returning to open state on failure or closed on success.
    *
    * @param f
    *   function to protect
    * @return
    *   the result of the function f
    */
  def protect[A](f: => F[A]): F[Either[Throwable, A]]

  def getState: F[State]
}

object CircuitBreaker {
  def isElapsed[F[_]: Concurrent](startTime: Long, resetTimeout: FiniteDuration, clock: Clock[F]): F[Boolean] =
    clock.realTime.map(time => (time.toMillis - startTime) >= resetTimeout.toMillis)
}
