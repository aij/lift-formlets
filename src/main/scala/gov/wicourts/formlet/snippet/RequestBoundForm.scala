package gov.wicourts.formlet.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.http._

import gov.wicourts.formlet._

import xml._

trait RequestBoundForm[A] {
  protected val formState = RequestBoundForm.newFormState

  protected def form: Form[A]

  def binder(process: A => Unit): NodeSeq => NodeSeq =
    RequestBoundForm.create(formState, form)(process)
}

object RequestBoundForm {
  def newFormState: RequestVar[FormState] = {
    new RequestVar[FormState](FormState(false)) {
      override lazy val __nameSalt = Helpers.nextFuncName
    }
  }

  def create[A](formState: RequestVar[FormState], form: => Form[A])(process: A => Unit): NodeSeq => NodeSeq = {
    def processResult_? = formState.get.renderErrors_?

    val env =
      if (processResult_?)
        Env.paramsEnv
      else
        Env.emptyEnv

    val (s, a) = form.run(env, formState.get)

    if (processResult_?) {
      a.result.foreach(process)
    }

    val provideFormState: Elem = SHtml.hidden(() => formState.set(s.copy(renderErrors_? = true)))

    {
      ns: NodeSeq => ns match {
        case e: Elem => e.copy(child = provideFormState ++ a.transform.apply(e.child))
        case n => n
      }
    }
  }
}
