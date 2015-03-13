package code.snippet

import gov.wicourts.formlet._
import gov.wicourts.formlet.Form._
import gov.wicourts.formlet.HtmlForms._
import gov.wicourts.formlet.HtmlForms.DefaultFieldHelpers._
import gov.wicourts.formlet.HtmlForms.FoundationErrorBinder._
import gov.wicourts.formlet.snippet.RequestBoundForm

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers._
import net.liftweb.http.S

// Template: src/main/webapp/form1.html
object Form1 {
  // About the simplest form there is. Binds to a single <input>.
  private def form: Form[Option[String]] =
    field(".fullName", input("fullName", none[String]))

  private val binder = RequestBoundForm.newBinder(form) { a =>
    S.notice(s"Hi there, ${a getOrElse "N/A"}. Nice to meet you!")
  }

  def render = ".form1" #> binder
}

// Template: src/main/webapp/form2.html
object Form2 {
  import Extras._

  case class FullName(firstName: String, middleName: Option[String], lastName: String) {
    def fullName: String =
      List(firstName.some, middleName, lastName.some).flatten.mkString(" ")
  }

  // See Extras for an explanation of the inputField combinator
  //
  // There are now three input fields. First name and last name are required.
  // Middle initial is optional.
  //
  // There is a bit of implicit magic to add a .required enrichment to any
  // Option[A] form, but behind the scenes it uses the .mapStringV method just
  // like the second validation does. One important note about .mapString or
  // its more general cousin .mapV is that it can change the type of the form.
  // First name came in as Form[Option[String]], but left as a Form[String].
  //
  // The key detail is that any refinement of a form or composition with
  // another form simply returns another Form. This means that the second
  // validation sees a String and not an Option[String].
  //
  // Form values can be transformed in other ways too. As an example, a period
  // is appended to the middle name field if it's defined.
  //
  // Lastly, the three fields are combined into a FullName.
  //
  // A few more words about validations:
  //
  // In order to really be useful, validation errors must be displayed somehow
  // when the form is replayed. The field combinator takes an implicit
  // ErrorBinder parameter which will be called to bind validation errors for
  // that field into the display. The examples use FoundationErrorBinder.
  //
  // Any errors in the form provided to the field as a parameter will be
  // displayed as well as any errors arising from validations applied to the
  // field form itself.
  private def form: Form[FullName] =
    ^^(
      inputField("firstName", "First name", none[String])
        .required
        .mapStringV(s =>
          if(s.equalsIgnoreCase("Mike")) s.success
          else "Only Mike is allowed".failure),
      inputField("middleInitial", "Middle initial", none[String])
        .map(_.map(_ + ".")),
      inputField("lastName", "Last name", none[String]).required
    )(FullName.apply _)

  private val binder = RequestBoundForm.newBinder(form) { a =>
    S.notice(s"Hi there, ${a.fullName}. Nice to meet you!")
  }

  def render = ".form2" #> binder
}

object Extras {
  // An example combinator that composes several forms (four to be exact).
  // labelText is bound to a <label>. An <input> is bound with the provided
  // name and then the for attribute on the <label> and the id attribute on the
  // <input> is also set to this name. Finally, .<name> is used as the selector
  // to a field form to provide the necessary context.
  //
  // It's important to note that none of the forms listed here is in any way
  // "magical". None of the them are particularly involved and all could have
  // been defined as part of the application just like inputField.
  def inputField[A](
    name: String, labelText: String, default: Option[A]
  )(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    val attrs = "label [for]" #> name & "input [id]" #> name
    val labeledField = label(labelText) *> input(name, default) <* sel(attrs)

    field("." + name, labeledField)
  }
}
