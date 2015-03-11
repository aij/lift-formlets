package code.snippet

import gov.wicourts.formlet._
import gov.wicourts.formlet.HtmlForms._
import gov.wicourts.formlet.HtmlForms.DefaultFieldHelpers._
import gov.wicourts.formlet.snippet.RequestBoundForm

import scalaz._
import Scalaz._

import net.liftweb.util.Helpers._
import net.liftweb.http.S

object Form1 {
  private def form: Form[Option[String]] =
    field(".fullName",
      label("Full name") *> input("fullName", none[String]))

  private val binder = RequestBoundForm.newBinder(form) { a =>
    S.notice(s"Hi there, ${a getOrElse "N/A"}. Nice to meet you!")
  }

  def render = ".form1" #> binder
}
