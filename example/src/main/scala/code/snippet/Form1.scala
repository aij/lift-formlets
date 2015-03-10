package code.snippet

import gov.wicourts.formlet._
import gov.wicourts.formlet.HtmlForms._
import gov.wicourts.formlet.HtmlForms.DefaultFieldHelpers._
import gov.wicourts.formlet.snippet.RequestBoundForm

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers._

object Form1 {
  val formState = RequestBoundForm.newFormState

  def form: Form[Option[String]] = 
    field(".fullName", 
      label("Full name") *> input("fullName", none[String]))

  def render = ".form1" #> RequestBoundForm.create(formState, form)(a => println("Got a " + a))
}
