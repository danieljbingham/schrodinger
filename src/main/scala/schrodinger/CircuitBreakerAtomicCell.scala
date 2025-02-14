package schrodinger

import scala.concurrent.duration.FiniteDuration
import cats.effect.Ref
import cats.effect.Concurrent
import cats.syntax.all.*
import State.*
import cats.data.EitherT
import cats.effect.Clock
import schrodinger.CircuitBreaker.isElapsed
import cats.effect.std.AtomicCell

object CircuitBreakerAtomicCell {

  def apply[F[_]: Concurrent: Clock](maxFailures: Int, resetTimeout: FiniteDuration) =
    for {
      cell <- AtomicCell[F].of(Closed(0))
      clock = Clock[F]
    } yield new CircuitBreaker[F] {
      def protect[A](f: => F[A]): F[Either[Throwable, A]] = cell.evalModify {
        case Closed(failures) =>
          f.attempt.flatMap {
            case Left(err) if failures + 1 >= maxFailures => clock.realTime.map(time => (Open(time.toMillis), Left(err)))
            case Left(err)                                => (Closed(failures + 1), Left(err)).pure
            case Right(value)                             => (Closed(failures), Right(value)).pure
          }
        case Open(startTime) =>
          isElapsed(startTime, resetTimeout, clock).flatMap {
            case true =>
              f.attempt.flatMap {
                case Right(value) => (Closed(0), Right(value)).pure
                case Left(err)    => clock.realTime.map(time => (Open(time.toMillis), Left(err)))
              }
            case false => (Open(startTime), Left(CircuitBreakerOpenException)).pure
          }
        /*
         * TODO: implement half-open state to define when to move back to closed based on proportion of successes or something,
         * currently bypassed by Open state
         */
        case HalfOpen => (HalfOpen, Left(CircuitBreakerOpenException)).pure
      }

      def getState: F[State] = cell.get
    }
}
