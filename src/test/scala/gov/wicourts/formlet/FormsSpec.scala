package gov.wicourts.formlet

import org.specs2.mutable.Specification

import xml._

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers.{^ => _, _}

class FormsSpec extends FormletSpec {
  import FormHelpers._

  val F = Form.F

  def testEnv = new Env {
    def param(s: String) =
      if (s == "test") List("test value")
      else Nil
    def file(s: String) = None
  }

  "A failing form" >> {
    "should fail with the provided message" >> {
      val (_, r) = Form.failing("no way!").runEmpty
      r.errors must_== List(Text("no way!"))
    }
  }

  "Validations that depend on another field" >> {
    "should not execute if the dependent field is not valid" >> {
      val alwaysFails = lift2V[String,String]((bn, a) => liftStringV("not this time".failure))
      val failingForm = Form.failing[String]("no way!")
      val a = F.point[String]("ok").val2(failingForm)(alwaysFails)

      a.evalEmpty.result must_== "ok".success
    }

    "should always apply client-side validation transform" >> {
      val alwaysFails = lift2V[String,String](
        (bn, a) => liftStringV("not this time".failure))
          .setTransform(s => ((s + " [data-foo]") #> "foo"))

      val failingOtherForm = Form.failing[String]("no way!")
      val failingForm = Form
        .failing[String]("no way!")
        .baseSelector("input")
        .val2(failingOtherForm)(alwaysFails)

      val in = <input />
      val out = <input data-foo="foo" />

      check(failingForm, in, out)
    }
  }

  "A validated form" >> {
    "should process all its validations" >> {
      val (_, r) =
        (F.point("hi".some)
          ?? (StringValidation(_ => "nope".failure), StringValidation(_ => "definitely not".failure)))
          .runEmpty
      r.errors must_== List(Text("nope"), Text("definitely not"))
    }
    "should process validations in groups" >> {
      val (_, r) =
        (F.point("hi".some) ?? StringValidation(_ => "nope".failure) ?? StringValidation(_ => "definitely not".failure)).runEmpty
      r.errors must_== List(Text("nope"))
    }
  }

  "Memoization" >> {
    def randomResultForm: Form[String] =
      liftResult(BoundForm(randomString(10).success, FormMetadata.empty, cssSelZero))

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
