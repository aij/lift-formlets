package code.snippet

import gov.wicourts.formlet._
import gov.wicourts.formlet.HtmlForms._
import gov.wicourts.formlet.HtmlForms.DefaultFieldHelpers._
import gov.wicourts.formlet.snippet.RequestBoundForm

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers._

object Form1 extends RequestBoundForm[Option[String]] {
  protected def form: Form[Option[String]] =
    field(".fullName",
      label("Full name") *> input("fullName", none[String]))

  private def process(a: Option[String]): Unit = {
    println("Got a " + a)
  }

  def render = ".form1" #> binder(process)
}
