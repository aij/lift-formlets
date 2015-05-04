package code.snippet

import gov.wicourts.formlet._
import gov.wicourts.formlet.Form._
import gov.wicourts.formlet.HtmlForms._
import gov.wicourts.formlet.HtmlForms.DefaultFieldHelpers._
import gov.wicourts.formlet.HtmlForms.FoundationErrorBinder._
import gov.wicourts.formlet.snippet.RequestBoundForm

import scalaz.std.option._

import scalaz.syntax.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.validation._

import net.liftweb.util.Helpers.{^ => _, _}
import net.liftweb.http.S
import net.liftweb.http.SHtml.SelectableOption

// Template: src/main/webapp/form1.html
object Form1 {
  // About the simplest form there is. Binds to a single <input>.
  private def form: Form[Option[String]] =
    field(".fullName", input("fullName", none[String]))

  private val binder = RequestBoundForm.newBinder(form) { (_, a) =>
    S.notice(s"Hi there, ${a getOrElse "N/A"}. Nice to meet you!")
  }

  def render = ".form1" #> binder
}

// Template: src/main/webapp/form2.html
object Form2 {
  case class FullName(firstName: String, middleName: Option[String], lastName: String) {
    def fullName: String =
      List(firstName.some, middleName, lastName.some).flatten.mkString(" ")
  }

  // See below for an explanation of the inputField combinator
  //
  // There are now three input fields. First name and last name are required.
  // Middle initial is optional.
  //
  // There is a bit of implicit magic to add a .required enrichment to any
  // Option[A] form, but behind the scenes it uses the .mapStringV method just
  // like the second validation does. One important note about .mapStringV or
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

  private val binder = RequestBoundForm.newBinder(form) { (_, a) =>
    S.notice(s"Hi there, ${a.fullName}. Nice to meet you!")
  }

  def render = ".form2" #> binder

  // An example combinator that composes several forms (four to be exact).
  // labelText is bound to a <label>. An <input> is bound with the provided
  // name and then the for attribute on the <label> and the id attribute on the
  // <input> is also set to this name. Finally, .<name> is used as the selector
  // to a field form to provide the necessary context.
  //
  // See http://simply.liftweb.net/index-7.10.html for more information on the
  // #> operator. Running a form produces a binding function (a CssSel) that is
  // applied to an existing HTML template. Binding functions are composed when
  // forms are composed.
  def inputField[A](
    name: String, labelText: String, default: Option[A]
  )(
    implicit converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    val attrs = "label [for]" #> name & "input [id]" #> name
    val labeledField = label(labelText) *> input(name, default) <* sel(attrs)

    field(s".$name", labeledField)
  }
}

// Template: src/main/webapp/form3.html
object Form3 {
  // As above
  case class FullName(firstName: String, middleName: Option[String], lastName: String) {
    def fullName: String =
      List(firstName.some, middleName, lastName.some).flatten.mkString(" ")
  }

  case class Registration(
    guest: FullName,
    plusone: FullName,
    favoriteColors: List[String],
    comments: Option[String],
    mailingList: Boolean
  )

  private val fullNameForm =
    ^^(
      inputField("firstName", "First name", none[String]).required,
      inputField("middleInitial", "Middle initial", none[String]),
      inputField("lastName", "Last name", none[String]).required
    )(FullName.apply _)

  val choices = List(
    "Red",
    "Blue",
    "Yellow",
    "Purple",
    "Orange",
    "Green").map(s => SelectableOption(s, s))

  // Various ways of rendering a list of choices. Behind the scenes these all
  // use the choices form with different UI binders, which you could also
  // supply yourself.
  private val colors =
    ^^^(
      selectField("selectSelect", none[String], choices),
      selectField("radioSelect", none[String], choices, asRadioButtons = true),
      multiSelectField("selectMultiSelect", Nil, choices),
      multiSelectField("checkboxMultiSelect", Nil, choices, asCheckboxes = true)
    )((a, b, c, d) => List(a.toList, b.toList, c, d).flatten.distinct.sorted)

  private def form: Form[Registration] =
    ^^^^(
      group("guest", fullNameForm),
      group("plusone", fullNameForm),
      colors,
      field(".comments", textarea("comments", none[String])),
      field(".mailingList", checkbox("mailingList", true))
    )(Registration.apply _)

  private val binder = RequestBoundForm.newBinder(form) { (_, a) =>
    // Just for demo purposes!
    S.notice(
      <div>
        Hi there {a.guest.fullName}! Thanks for registering.
        <ul>
          <li>Name: {a.guest.fullName}</li>
          <li>Plus one name: {a.plusone.fullName}</li>
          <li>Favorite colors: {a.favoriteColors.mkString(", ")}</li>
          <li>Comments: {a.comments.getOrElse("")}</li>
          <li>Mailing list: {a.mailingList}</li>
        </ul>
      </div>
    )
  }

  def render = ".form3" #> binder

  // A few combinators for input forms identified by CSS class name.

  // Very similar to the inputField combinator from Form2, but it goes one step
  // further and generates the <label> element directly vs. binding to an
  // existing one.
  def inputField[A](
    name: String, labelText: String, default: Option[A]
  )(
    implicit converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    val labeledField = input(name, default) <* sel("input [id]" #> name)

    field(s".$name", labeledField).mapBinder(
      _ & s".$name -*" #> <label for={name}>{labelText}</label>)
  }

  def selectField[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]],
    asRadioButtons: Boolean = false
  )(
    implicit converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    field(s".$name", select(name, default, options, asRadioButtons))
  }

  def multiSelectField[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]],
    asCheckboxes: Boolean = false
  )(
    implicit converter: Converter[Option[A]]
  ): Form[List[A]] = {
    field(s".$name", multiSelect(name, default, options, asCheckboxes))
  }
}
