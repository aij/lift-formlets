package gov.wicourts.formlet

import org.specs2.mutable.Specification

import xml._

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers.{^ => _, _}

class FormsSpec extends Specification {
  import Forms._

  val F = Form.F

  def applyNs[A](form: Form[A], ns: NodeSeq): NodeSeq =
    form.runEmpty._2.transform.apply(ns).head

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

  "A validated form" >> {
    "should process all its validations" >> {
      val (_, r) =
        (F.point(Some("hi")) ?? (_ => "nope".failure, _ => "definitely not".failure)).runEmpty
      r.errors must_== List(Text("nope"), Text("definitely not"))
    }
    "should process validations in groups" >> {
      val (_, r) =
        (F.point(Some("hi")) ?? (_ => "nope".failure) ?? (_ => "definitely not".failure)).runEmpty
      r.errors must_== List(Text("nope"))
    }
  }

  "Memoization" >> {
    val randomResultForm: Form[String] =
      Form(env =>
        for {
          _ <- get[FormState]
        } yield BoundForm(
          randomString(10).success,
          None,
          Forms.cssSelZero))

    "should not work without calling .memoize" >> {
      val form = F.tuple2(randomResultForm, randomResultForm)
      val result = form.runEmpty._2.result.toOption

      result.map(r => r._1 must_!= r._2).getOrElse(1 must_== 2)
    }

    "should work when calling .memoize" >> {
      val memoized = randomResultForm.memoize
      val form = F.tuple2(memoized, memoized)
      val result = form.runEmpty._2.result.toOption

      result.map(r => r._1 must_== r._2).getOrElse(1 must_== 2)
    }
  }
}
