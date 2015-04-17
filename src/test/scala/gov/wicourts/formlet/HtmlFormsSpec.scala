package gov.wicourts.formlet

import org.specs2.mutable.Specification
import org.specs2.matcher.XmlMatchers

import xml._

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers.{^ => _, _}
import net.liftweb.http.SHtml.SelectableOption

class HtmlFormsSpec extends FormletSpec {
  import FormHelpers._

  import HtmlForms._
  import HtmlForms.DefaultFieldHelpers._

  val F = Form.F

  implicit val errorBinder = ErrorBinder((selector, baseSelector, errors) =>
    s"$selector [class+]" #> "error" &
      s"$selector *+" #> (errors.map(n => <small class="error">{n.error}</small>): NodeSeq) &
      s"$selector" #> baseSelector.map(b => s"$b [aria-invalid]" #> "true").getOrElse(cssSelZero)
  )

  def testEnv = new Env {
    def param(s: String) =
      if (s == "test") List("test value")
      else Nil
    def file(s: String) = None
  }

  "A label form" >> {
    "should bind the label to the argument" >> {
      val in = <label></label>
      val out = <label>test label</label>
      check(label("test label"), in, out)
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
      val in = <input />
      val out = <input name="test" value="a" />
      check(input("test", "a".some), in, out)
    }

    "should support client-side validation" >> {
      val in = <input />
      val out = <input name="test" value="" required="required"></input>
      check(input[String]("test", None).html5Required, in, out)
    }

    "should support form context" >> {
      val in = <input />
      val out = <input name="tc_test" value=""></input>
      check(input[String]("test", None).context("tc"), in, out)
    }
  }

  "A field form" >> {
    "should be named by its label" >> {
      val (_, r) = field(".test", label("test label")).runEmpty
      r.metadata.label must_== "test label".some
    }

    "should set error label to its label" >> {
      val errorMessage = "something went wrong"
      val l = "test label"
      val (_, r) = field(
        ".test",
        Form.failing(errorMessage) <* label(l)).runEmpty

      r.result must_== FormError(Text(errorMessage), l.some).failure.toValidationNel
    }

    "should bind nested errors" >> {
      val in = <div class="test"><input type="text"/></div>
      val out = <div class="test error"><input type="text"/><small class="error">no way!</small></div>
      check(field(".test", Form.failing("no way!")), in, out)
    }

    "should be able to bind directly to an <input>" >> {
      val in = <input class="testClass"></input>
      val out = <input name="testName" class="testClass" value="v"></input>
      check(field(".testClass", input("testName", "v".some)), in, out)
    }

    "should be able to bind errors from validations applied to itself" >> {
      val in =
        <div class="test">
          <input type="text" />
        </div>
      val out =
        <div class="test error">
          <input type="text" name="test" value="" foo="foo" aria-invalid="true" />
          <small class="error">no way!</small>
        </div>

      val other = F.point("hi!")

      val f = field(".test", input[String]("test", None))
      val v = FormValidation[Option[String], Option[String]](
        a => liftStringV("no way!".failure),
        Some(s => s"$s [foo]" #> "foo")
      )

      "with one parameter" >> {
        check(f.mapV(v), in, out)
      }

      "with two parameters" >> {
        check(f.val2(other)(v.lift1To2V), in, out)
      }

      "with three parameters" >> {
        check(f.val3(other, other)(v.lift1To3V), in, out)
      }

      "with four parameters" >> {
        check(f.val4(other, other, other)(v.lift1To4V), in, out)
      }
    }
  }

  "A group form" >> {
    "should apply CSS selector context and form name context" >> {
      val in =
        <div>
          <div class="name1"><input type="text"/></div>
          <div class="name2"><input type="text"/></div>
        </div>
      val out =
        <div>
          <div class="name1"><input value="" type="text" name="name1_fullName"/></div>
          <div class="name2"><input value="" type="text" name="name2_fullName"/></div>
        </div>

      val fullName = input[String]("fullName", None)
      val form = F.tuple2(group("name1", fullName), group("name2", fullName))

      check(form, in, out)
    }
  }

  "A required form" >> {
    "should fail if form is empty" >> {
      val (_, r) = F.point(None).mapStringV(required).runEmpty
      r.errorsNs must_== List(Text(HtmlForms.requiredMessage))
    }

    "should succeed if form has a value" >> {
      val (_, r) = F.point("hi".some).mapStringV(required).runEmpty
      r.result must_== "hi".success
    }
  }

  "A select form" >> {
    val options = List(
      SelectableOption(1, "one", "id" -> "oneoneone"),
      SelectableOption(2, "two", "id" -> "twotwotwo"),
      SelectableOption(3, "three", "id" -> "threethreethree"))

    "should be able to select a value from its saved state" >> {
      def transform[A](
        name: String,
        defaults: List[String],
        options: List[SelectableOptionWithNonce[A]]
      ) = ("div", "div" #> options.map(_.nonce).mkString(","))

      val sel = choice("test", None, options)(transform _).required

      val (s, r) = sel.runEmpty
      val nonces = r.binder.apply(<div></div>).toString.split(",")
      val (_, r2) = sel.run(Env.singleEnv(Map("test" -> nonces(1))), s)

      r2.result must_== 2.success
    }

    "should support form context" >> {
      val sel = select("test", None, options).required.context("tc")

      val ns = applyNs(sel, <select></select>)

      (ns \\ "@name").toString must_== "tc_test"
    }

    // A few basic smoke tests

    "should be able to render to a <select>" >> {
      val sel = select("test", None, options).required

      val ns = applyNs(sel, <select></select>)

      (ns \\ "option").length must_== 3
    }

    "should be able to render to a <select multiple>" >> {
      val sel = multiSelect("test", Nil, options)

      val ns = applyNs(sel, <select></select>)

      (ns \\ "@multiple").toString must_== "multiple"
      (ns \\ "option").length must_== 3
    }

    "should be able to render to radio buttons" >> {
      val input =
        <div>
          <label><input type="radio" /></label>
        </div>

      val form = select("test", None, options, asRadioButtons = true).required
      val ns = applyNs(form, input)

      (ns \\ "label").length must_== 3
      (ns \\ "input" \\ "@type").distinct.toString must_== "radio"
    }

    "should be able to render to checkbox controls" >> {
      val input =
        <div>
          <label><input type="checkbox" /></label>
        </div>

      val form = multiSelect("test", Nil, options, asCheckboxes = true)
      val ns = applyNs(form, input)

      (ns \\ "label").length must_== 3
      (ns \\ "input" \\ "@type").distinct.toString must_== "checkbox"
    }
  }

  "A checkbox form" >> {
    "should be able render itself" >> {
      val in = <div><input></input></div>
      val out =
        <div>
          <input type="checkbox" name="test" value="true" checked="checked"/>
          <input type="hidden" name="test" value="false"/>
        </div>

      check(checkbox("test", true), in, out)
    }

    "should support form context" >> {
      val in = <div><input></input></div>
      val out =
        <div>
          <input type="checkbox" name="tc_test" value="true"/>
          <input type="hidden" name="tc_test" value="false"/>
        </div>

      check(checkbox("test", false).context("tc"), in, out)
    }
  }

  "A file form" >> {
    "should be able to render itself and support form context" >> {
      val in = <input />
      val out = <input type="file" name="tc_test" />

      check(file("test").context("tc"), in, out)
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
      val requiredFirstName = input("firstName", "Frank".some).required

      def mkFirstName(input: Form[String]): Form[String] =
        field(".firstName", input <* label("First name"))

      val firstName = mkFirstName(requiredFirstName)

      val lastName = field(
        ".lastName",
        input("lastName", "Johnson".some).required <* label("Last name")
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
        val env = Env.singleEnv(Map("firstName" -> "Joe"))
        val frankFirstName = mkFirstName(requiredFirstName ?? (
          StringValidation(s => if (s.startsWith("Frank")) s.success else "Only Frank is allowed".failure)))

        val fullName = ^(frankFirstName, lastName)(FullName.apply _)
        val (_, r) = fullName.run(env)

        r.errorsNs must_== List(Text("Only Frank is allowed"))
      }
    }

    "using Frank example #2" >> {
      val firstName = field(
        ".firstName",
        input[String]("firstName", None) <* label("Last name")
      )

      val lastName = field(
        ".lastName",
        input[String]("lastName", "Johnson".some) <* label("Last name")
      )

      "can combine multiple optional fields" >> {
        val (_, r) = optionalFullName(firstName, lastName).runEmpty
        r.result must_== None.success
      }
    }

    "using Frank example #3" >> {
      val lastName = field(
        ".lastName",
        input[String]("lastName", None) <* label("Last name"))

      def requireIfOtherSet[B, A](bn: FormValue[Option[B]], a: Option[A]): ValidationNelE[Option[A]] = {
        val result =
          if (bn.value.isDefined && a.isEmpty)
            ("This field is required if the " + bn.label.getOrElse("N/A") + " field is set").failure
          else
            a.success
        liftStringV(result)
      }

      val firstNameInput = input[String]("firstName", None)
        .val2(lastName)(lift2V(requireIfOtherSet))
      val firstName = field(".firstName", firstNameInput <* label("First name"))

      val fullName = optionalFullName(firstName, lastName)

      "first name won't be required if last name isn't set" >> {
        val (_, r) = fullName.runEmpty
        r.result must_== None.success
      }

      "will return a FullName if both are set" >> {
        val env = Env.singleEnv(Map("firstName" -> "Frank", "lastName" -> "Johnson"))
        val (_, r) = fullName.run(env)
        r.result must_== FullName("Frank", "Johnson").some.success
      }

      "will return an error if only last name is set" >> {
        val env = Env.singleEnv(Map("lastName" -> "Johnson"))
        val (_, r) = fullName.run(env)
        r.errorsNs must_== List(Text("This field is required if the Last name field is set"))
      }
    }
  }
}
