package schrodinger

import cats.effect.IO
import cats.implicits.*
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.*
import org.scalatest.compatible.Assertion

final class CircuitBreakerSpec extends AsyncWordIOSpecBase {

  private val failureMessage = "uh oh"
  private val failure = new RuntimeException(failureMessage)
  private val successMessage = "yippee"
  private val success = IO(successMessage)

  "CircuitBreaker" should {
    "execute function in closed state within maxFailures" in {
      for {
        cb <- CircuitBreakerBasic[IO](maxFailures = 1, resetTimeout = 1.second)
        result <- cb.protect(success)
      } yield result.value shouldBe successMessage

    }

    "transition to open state on maxFailures" in {
      for {
        cb <- CircuitBreakerBasic[IO](maxFailures = 1, resetTimeout = 1.second)
        closedResult <- cb.protect(IO.raiseError(failure))
        openResult <- cb.protect(IO.raiseError(failure))
      } yield {
        closedResult.left.value shouldBe a[RuntimeException]
        closedResult.left.value.getMessage shouldBe failureMessage
        openResult.left.value shouldBe CircuitBreakerOpenException
      }
    }

    // TODO: test by simulating cats clock
    "remain in open state when below resetTimeout" in {
      for {
        cb <- CircuitBreakerBasic[IO](maxFailures = 1, resetTimeout = 10.seconds)
        closedResult <- cb.protect(IO.raiseError(failure))
        openResult <- cb.protect(IO.raiseError(failure))
        openResult1 <- cb.protect(IO(1))
      } yield {
        closedResult.left.value shouldBe a[RuntimeException]
        closedResult.left.value.getMessage shouldBe failureMessage
        openResult.left.value shouldBe CircuitBreakerOpenException
        openResult1.left.value shouldBe CircuitBreakerOpenException
      }
    }

    // TODO: test by simulating cats clock instead of sleeping
    "reset open state when resetTimeout has elapsed and function fails" in {
      for {
        cb <- CircuitBreakerBasic[IO](maxFailures = 1, resetTimeout = 1.seconds)
        closedResult <- cb.protect(IO.raiseError(failure))
        openResult <- cb.protect(IO.raiseError(failure))
        _ <- IO.sleep(2.seconds)
        openResult1 <- cb.protect(IO.raiseError(failure))
      } yield {
        closedResult.left.value shouldBe a[RuntimeException]
        closedResult.left.value.getMessage shouldBe failureMessage
        openResult.left.value shouldBe CircuitBreakerOpenException
        openResult1.left.value shouldBe a[RuntimeException]
        openResult1.left.value.getMessage shouldBe failureMessage
      }
    }

    // TODO: test by simulating cats clock instead of sleeping
    "transition to closed state when resetTimeout has elapsed and function succeeds" in {
      for {
        cb <- CircuitBreakerBasic[IO](maxFailures = 1, resetTimeout = 1.seconds)
        closedResult <- cb.protect(IO.raiseError(failure))
        openResult <- cb.protect(IO.raiseError(failure))
        _ <- IO.sleep(2.seconds)
        closedResult1 <- cb.protect(success)
      } yield {
        closedResult.left.value shouldBe a[RuntimeException]
        closedResult.left.value.getMessage shouldBe failureMessage
        openResult.left.value shouldBe CircuitBreakerOpenException
        closedResult1.value shouldBe successMessage
      }
    }

    // TODO: refactor tests so I'm testing all cases for all implementations of CircuitBreaker
    "should atomically update state with atomiccell" in {
      for {
        cb <- CircuitBreakerAtomicCell[IO](maxFailures = 501, resetTimeout = 1.seconds)
        _ <- List.fill(500)(cb.protect(IO.raiseError(failure))).parSequence
        state <- cb.getState
      } yield state shouldBe State.Closed(500)
    }

    "should atomically update state with mutex" in {
      for {
        cb <- CircuitBreakerBasic[IO](maxFailures = 501, resetTimeout = 1.seconds)
        _ <- List.fill(500)(cb.protect(IO.raiseError(failure))).parSequence
        state <- cb.getState
      } yield state shouldBe State.Closed(500)
    }
  }
}
