package gov.wicourts.formlet

import org.specs2.mutable.Specification
import org.specs2.matcher.XmlMatchers

import xml._

trait FormletSpec extends Specification with XmlMatchers {
  def applyNs[A](form: Form[A], ns: NodeSeq): NodeSeq =
    form.evalEmpty.transform.apply(ns).head

  def check[A](form: Form[A], ns: NodeSeq, expected: NodeSeq) = {
    applyNs(form, ns) must ==/ (expected)
  }
}
