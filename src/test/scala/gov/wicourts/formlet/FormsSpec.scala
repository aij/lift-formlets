package gov.wicourts.formlet

import org.specs2.mutable.Specification

import xml._

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers.{^ => _, _}

class FormsSpec extends Specification {
  import Forms._
  import HtmlForms._
  import HtmlForms.DefaultFieldHelpers._

  val F = Form.F

  def applyNs[A](form: Form[A], ns: NodeSeq): NodeSeq =
    form.runEmpty._2.transform.apply(ns).head

  def testEnv(s: String): List[String] =
    if (s == "test") List("test value")
    else Nil

  "A label form" >> {
    "should bind the label to the argument" >> {
      applyNs(label("test label"), <label></label>) must_== <label>test label</label>
    }
  }

  "An input form" >> {
    "should return its default with empty environment" >> {
      val (_, r) = input("test", "test default".some).runEmpty
      r.result must_== "test default".some.success
    }

    "should return environment value if provided" >> {
      val (_, r) = input("test", "test default".some).run(testEnv _)
      r.result must_== "test value".some.success
    }

    "should bind its value" >> {
      val v = applyNs(input("test", "a".some), <input/>)
      v must_== <input value="a"></input>
    }
  }

  "A failing form" >> {
    "should fail with the provided message" >> {
      val (_, r) = Form.failing("no way!").runEmpty
      r.errors must_== List(Text("no way!"))
    }
  }

  "A field form" >> {
    "should be named by its label" >> {
      val (_, r) = field(".test", label("test label")).runEmpty
      r.name must_== Some("test label")
    }

    "should bind its errors" >> {
      val xml = <div class="test"><div class="errors"></div></div>
      val result = <div class="test"><div class="errors"><div>no way!</div></div></div>
      applyNs(field(".test", Form.failing("no way!")), xml) must_== result
    }
  }

  "A required form" >> {
    "should fail if form is empty" >> {
      val (_, r) = F.point(None).mapStringV(required).runEmpty
      r.errors must_== List(Text(HtmlForms.requiredMessage))
    }

    "should succeed if form has a value" >> {
      val (_, r) = F.point(Some("hi")).mapStringV(required).runEmpty
      r.result must_== "hi".success
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
          ".akskeidkopdk" #> NodeSeq.Empty))
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

  // Example code
  "Composite forms" >> {
    case class FullName(firstName: String, lastName: String)

    // Turn firstName (a Form[Option[String]]) and lastName (a
    // Form[Option[String]]) into a Form[Option[FullName]]. If either
    // firstName or lastName is None, the result will be None
    def optionalFullName(
      firstName: Form[Option[String]],
      lastName: Form[Option[String]]
    ): Form[Option[FullName]] = {
      val H = F.compose[Option]
      H.apply2(firstName, lastName)(FullName.apply _)
    }

    "using Frank example #1" >> {
      val requiredFirstName = input("firstName", Some("Frank")).required

      def mkFirstName(input: Form[String]): Form[String] =
        field(".firstName", input <++ label("First name"))

      val firstName = mkFirstName(requiredFirstName)

      val lastName = field(
        ".lastName",
        input("lastName", Some("Johnson")).required <++ label("Last name")
      )


      "can combine using applyN (^/^^/etc.)" >> {
        // Turn firstName (a Form[String]) and a lastName (a Form[String]) into
        // a Form[FullName]
        val fullName = ^(firstName, lastName)(FullName.apply _)
        val (_, r) = fullName.runEmpty
        r.result must_== FullName("Frank", "Johnson").success
      }

      "can combine using an applicative builder" >> {
        // Same as above with different syntax
        val fullName = (firstName |@| lastName)(FullName.apply _)
        val (_, r) = fullName.runEmpty
        r.result must_== FullName("Frank", "Johnson").success
      }

      "if any parts of the whole fail validation, so does the whole" >> {
        // Simulate a user entry of Joe for firstName
        def env(s: String): List[String] =
          if (s.equals("firstName")) List("Joe")
          else Nil
        val frankFirstName = mkFirstName(requiredFirstName ?? (
          s => if (s.startsWith("Frank")) s.success else "Only Frank is allowed".failure))

        val fullName = ^(frankFirstName, lastName)(FullName.apply _)
        val (_, r) = fullName.run(env _)

        r.errors must_== List(Text("Only Frank is allowed"))
      }
    }

    "using Frank example #2" >> {
      val firstName = field(
        ".firstName",
        input[String]("firstName", None) <++ label("Last name")
      )

      val lastName = field(
        ".lastName",
        input[String]("lastName", "Johnson".some) <++ label("Last name")
      )

      "can combine multiple optional fields" >> {
        val (_, r) = optionalFullName(firstName, lastName).runEmpty
        r.result must_== None.success
      }
    }

    "using Frank example #3" >> {
      val lastName = field(
        ".lastName",
        input[String]("lastName", None) <++ label("Last name"))

      def requireIfOtherSet[B,A](bn: FieldValue[Option[B]], a: Option[A]): Validation[String,Option[A]] =
        if (bn.value.isDefined && a.isEmpty)
          ("This field is required if the " + bn.name.getOrElse("N/A") + " field is set").failure
        else
          a.success

      val firstNameInput = input[String]("firstName", None)
        .val2(lastName)(requireIfOtherSet)
      val firstName = field(".firstName", firstNameInput <++ label("First name"))

      val fullName = optionalFullName(firstName, lastName)

      "first name won't be required if last name isn't set" >> {
        val (_, r) = fullName.runEmpty
        r.result must_== None.success
      }

      "will return a FullName if both are set" >> {
        def env(s: String): List[String] =
          s match {
            case "firstName" => List("Frank")
            case "lastName" => List("Johnson")
            case _ => Nil
          }
        val (_, r) = fullName.run(env _)
        r.result must_== FullName("Frank", "Johnson").some.success
      }

      "will return an error if only last name is set" >> {
        def env(s: String): List[String] =
          s match {
            case "lastName" => List("Johnson")
            case _ => Nil
          }
        val (_, r) = fullName.run(env _)
        r.errors must_== List(Text("This field is required if the Last name field is set"))
      }
    }
  }
}
