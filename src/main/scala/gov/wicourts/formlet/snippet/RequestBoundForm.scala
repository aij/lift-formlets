package gov.wicourts.formlet.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.http._

import gov.wicourts.formlet._

import xml._

object RequestBoundForm {
  def create[A](form: => Form[A])(process: A => Unit): NodeSeq => NodeSeq = {
    object formState extends RequestVar[FormState](FormState(false)) {
      override lazy val __nameSalt = Helpers.nextFuncName
    }

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
