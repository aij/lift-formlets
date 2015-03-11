package code.snippet

import gov.wicourts.formlet._
import gov.wicourts.formlet.Form._
import gov.wicourts.formlet.HtmlForms._
import gov.wicourts.formlet.HtmlForms.DefaultFieldHelpers._
import gov.wicourts.formlet.snippet.RequestBoundForm

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers._
import net.liftweb.http.S

// src/main/webapp/form1.html
object Form1 {
  // About the simplest form there is. Binds to a single <input>.
  private def form: Form[Option[String]] =
    field(".fullName", input("fullName", none[String]))

  private val binder = RequestBoundForm.newBinder(form) { a =>
    S.notice(s"Hi there, ${a getOrElse "N/A"}. Nice to meet you!")
  }

  def render = ".form1" #> binder
}

// src/main/webapp/form2.html
object Form2 {
  // An example combinator that composes several forms (four to be exact).
  // labelText is bound to a <label>. An <input> is bound with the provided
  // name and then the for attribute on the <label> and the id on the <input>
  // is also set to this name. Finally, .<name> is used as the selector to a
  // field form to provide the necessary context.
  //
  // The utility here might be a little questionable, but it does serve as an
  // example of how primitive forms can be easily composed to bind to
  // application-specific HTML.
  def inputField[A](
    name: String, labelText: String, default: Option[A]
  )(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    val attrs = "label [for]" #> name & "input [id]" #> name
    val labeledField = label(labelText) *> input(name, default) <* sel(attrs)

    field("." + name, labeledField)
  }

  private def form: Form[Option[String]] =
    inputField("fullName", "Full name", none[String])

  private val binder = RequestBoundForm.newBinder(form) { a =>
    S.notice(s"Hi there, ${a getOrElse "N/A"}. Nice to meet you!")
  }

  def render = ".form2" #> binder
}
