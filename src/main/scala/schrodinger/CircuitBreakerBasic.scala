package schrodinger

import scala.concurrent.duration.FiniteDuration
import cats.effect.Ref
import cats.effect.Concurrent
import cats.syntax.all.*
import State.*
import cats.data.EitherT
import cats.effect.Clock
import schrodinger.CircuitBreaker.isElapsed
import cats.effect.std.Mutex

object CircuitBreakerBasic {

  def apply[F[_]: Concurrent: Clock](maxFailures: Int, resetTimeout: FiniteDuration) =
    for {
      ref <- Ref[F].of(Closed(0))
      mutex <- Mutex[F]
      clock = Clock[F]
    } yield new CircuitBreaker[F] {
      private def simpleProtect[A](f: => F[A]): F[Either[Throwable, A]] = ref.get.flatMap {
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
                case Left(value)  => clock.realTime.flatMap(time => ref.set(Open(time.toMillis)))
              }
            case false => Left(CircuitBreakerOpenException).pure
          }
        /*
         * TODO: implement half-open state to define when to move back to closed based on proportion of successes or something,
         * currently bypassed by Open state
         */
        case HalfOpen => Left(CircuitBreakerOpenException).pure
      }

      def protect[A](f: => F[A]): F[Either[Throwable, A]] = mutex.lock.surround(simpleProtect(f))

      def getState: F[State] = ref.get
    }
}
