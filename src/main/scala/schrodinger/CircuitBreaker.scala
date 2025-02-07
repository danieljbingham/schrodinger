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
}

object CircuitBreaker {

  def apply[F[_]: Concurrent: Clock](maxFailures: Int, resetTimeout: FiniteDuration) =
    for {
      ref <- Ref[F].of(Closed(0))
      clock = Clock[F]
      cb = new CircuitBreaker[F] {
        def protect[A](f: => F[A]): F[Either[Throwable, A]] = ref.get.flatMap {
          case Closed(failures) =>
            f.attempt.flatTap {
              case Left(_) if failures + 1 >= maxFailures => clock.realTime.flatMap(time => ref.set(Open(time.toMillis)))
              case Left(_)                                => ref.set(Closed(failures + 1))
              case Right(_)                               => ().pure
            }
          case Open(startTime) =>
            isElapsed(startTime, resetTimeout, clock).flatMap {
              case true =>
                ref.set(HalfOpen) *> f.attempt.flatTap {
                  case Right(value) => ref.set(Closed(0))
                  case Left(value)  => ref.set(Open(System.currentTimeMillis()))
                }
              case false => Left(CircuitBreakerOpenException).pure
            }
          // TODO: implement half-open state to define when to move back to closed, currently handled by Open state
          case HalfOpen => Left(CircuitBreakerOpenException).pure
        }
      }
    } yield cb

  private def isElapsed[F[_]: Concurrent](startTime: Long, resetTimeout: FiniteDuration, clock: Clock[F]): F[Boolean] =
    clock.realTime.map(time => (time.toMillis - startTime) >= resetTimeout.toMillis)
}
