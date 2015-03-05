package gov.wicourts.formlet

import org.specs2.mutable.Specification
import org.specs2.matcher.XmlMatchers

import xml._

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers.{^ => _, _}
import net.liftweb.http.SHtml.SelectableOption

class HtmlFormsSpec extends Specification with XmlMatchers {
  import Forms._
  import HtmlForms._
  import HtmlForms.DefaultFieldHelpers._

  val F = Form.F

  def applyNs[A](form: Form[A], ns: NodeSeq): NodeSeq =
    form.runEmpty._2.transform.apply(ns).head

  def testEnv = new Env {
    def param(s: String) =
      if (s == "test") List("test value")
      else Nil
    def file(s: String) = None
  }

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
      val (_, r) = input("test", "test default".some).run(testEnv)
      r.result must_== "test value".some.success
    }

    "should bind its name and value" >> {
      val v = applyNs(input("test", "a".some), <input/>)
      v must_== <input name="test" value="a"></input>
    }
  }

  "A field form" >> {
    "should be named by its label" >> {
      val (_, r) = field(".test", label("test label")).runEmpty
      r.name must_== Some("test label")
    }

    "should set error label to its label" >> {
      val errorMessage = "something went wrong"
      val l = "test label"
      val (_, r) = field(
        ".test",
        Form.failing(errorMessage) <++ label(l)).runEmpty

      r.result must_== FormError(Text(errorMessage), l.some).failure.toValidationNel
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

  "A select form" >> {
    val options = List(
      SelectableOption(1, "one"),
      SelectableOption(2, "two"),
      SelectableOption(3, "three"))

    "should be able to select a value from its saved state" >> {
      def transform[A](
        name: String,
        defaults: List[String],
        options: List[SelectableOptionWithNonce[A]]
      ) = "div" #> options.map(_.nonce).mkString(",")

      val sel = select("test", None, options)(transform _).required

      val (s, r) = sel.runEmpty
      val nonces = r.transform.apply(<div></div>).toString.split(",")
      val (_, r2) = sel.run(singleEnv(Map("test" -> nonces(1))), s)

      r2.result must_== 2.success
    }

    "should be able to render to a <select>" >> {
      val sel = selectSelect("test", None, options).required

      val ns = applyNs(sel, <select></select>)

      (ns \\ "option").length must_== 3
    }
  }

  "A checkbox form" >> {
    "should be able render itself" >> {
      val c = checkbox("test", true)

      val expected =
        <div>
          <input type="checkbox" name="test" value="true"/>
          <input type="hidden" name="test" value="false"/>
        </div>


      val ns = applyNs(c, <div><input></input></div>)

      ns must ==/ (expected)
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
        val env = singleEnv(Map("firstName" -> "Joe"))
        val frankFirstName = mkFirstName(requiredFirstName ?? (
          s => if (s.startsWith("Frank")) s.success else "Only Frank is allowed".failure))

        val fullName = ^(frankFirstName, lastName)(FullName.apply _)
        val (_, r) = fullName.run(env)

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

      def requireIfOtherSet[B,A](bn: FormValue[Option[B]], a: Option[A]): Validation[String,Option[A]] =
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
        val env = singleEnv(Map("firstName" -> "Frank", "lastName" -> "Johnson"))
        val (_, r) = fullName.run(env)
        r.result must_== FullName("Frank", "Johnson").some.success
      }

      "will return an error if only last name is set" >> {
        val env = singleEnv(Map("lastName" -> "Johnson"))
        val (_, r) = fullName.run(env)
        r.errors must_== List(Text("This field is required if the Last name field is set"))
      }
    }
  }
}
