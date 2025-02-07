package schrodinger

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.EitherValues

trait AsyncWordIOSpecBase extends AsyncWordSpec, AsyncIOSpec, Matchers, EitherValues
