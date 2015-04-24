package gov.wicourts.formlet

import org.specs2.mutable.Specification

import xml._

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers.{^ => _, _}

class FormSpec extends FormletSpec {
  val F = Form.F

  import FormValidation._

  def testEnv = new Env {
    def param(s: String) =
      if (s == "test") List("test value")
      else Nil
    def file(s: String) = None
  }

  "A failing form" >> {
    "should fail with the provided message" >> {
      val r = Form.failing("no way!").evalEmpty
      r.errorsNs must_== List(Text("no way!"))
    }
  }

  "A form" >> {
    "can be described" >> {
      val a = F.point[String]("ok").describe(a => Vector("a: " + a))
      val b = F.point[String]("really ok").describe(b => Vector("b: " + b))
      val c = Form.failing[String]("nope!")

      val d = F.tuple3(a, b, c)

      val (w, _, _) = d.runEmpty

      w must_== Vector("b: really ok", "a: ok")
    }
  }

  "Validations that depend on another field" >> {
    "should not execute if the dependent fields are not valid" >> {
      val alwaysFails = FormValidation[String, String](a => liftStringV("not this time".failure))
      val failingForm = Form.failing[String]("no way!")
      val a = F.point[String]("ok")

      "with two parameters" >> {
        val b = a.val2(failingForm)(alwaysFails.lift1To2V)
        b.evalEmpty.result must_== "ok".success
      }

      "with three parameters" >> {
        val b = a.val3(failingForm, failingForm)(alwaysFails.lift1To3V)
        b.evalEmpty.result must_== "ok".success
      }

      "with four parameters" >> {
        val b = a.val4(failingForm, failingForm, failingForm)(alwaysFails.lift1To4V)
        b.evalEmpty.result must_== "ok".success
      }
    }

    "should always apply client-side validation transform" >> {
      val alwaysFails = FormValidation[String, String](
        a => liftStringV("not this time".failure),
        Some(s => s"$s [data-foo]" #> "foo")
      )

      val other = Form.failing[String]("no way!")
      val failing = other.baseSelector("input")

      val in = <input />
      val out = <input data-foo="foo" />

      "with two parameters" >> {
        check(failing.val2(other)(alwaysFails.lift1To2V), in, out)
      }

      "with three parameters" >> {
        check(failing.val3(other, other)(alwaysFails.lift1To3V), in, out)
      }

      "with four parameters" >> {
        check(failing.val4(other, other, other)(alwaysFails.lift1To4V), in, out)
      }
    }
  }

  "A validated form" >> {
    "should process all its validations" >> {
      val r =
        (F.point("hi".some)
          ?? (StringValidation(_ => "nope".failure), StringValidation(_ => "definitely not".failure)))
          .evalEmpty
      r.errorsNs must_== List(Text("nope"), Text("definitely not"))
    }
    "should process validations in groups" >> {
      val r =
        (F.point("hi".some) ?? StringValidation(_ => "nope".failure) ?? StringValidation(_ => "definitely not".failure)).evalEmpty
      r.errorsNs must_== List(Text("nope"))
    }
  }

  "Memoization" >> {
    def randomResultForm: Form[String] =
      Form.liftResult(BoundForm(randomString(10).success, FormMetadata.empty, cssSelZero))

    "should not work without calling .memoize" >> {
      val form = F.tuple2(randomResultForm, randomResultForm)
      val result = form.evalEmpty.result.toOption

      result.map(r => r._1 must_!= r._2).getOrElse(1 must_== 2)
    }

    "should work when calling .memoize" >> {
      val memoized = randomResultForm.memoize
      val form = F.tuple2(memoized, memoized)
      val result = form.evalEmpty.result.toOption

      result.map(r => r._1 must_== r._2).getOrElse(1 must_== 2)
    }
  }
}
