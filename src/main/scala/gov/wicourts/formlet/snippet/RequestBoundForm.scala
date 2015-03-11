package gov.wicourts.formlet.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.http._

import gov.wicourts.formlet._

import xml._

object RequestBoundForm {
  /**
   * Creates a new function for binding a form to a template. N.B. The
   * result be assigned to a `val` that persists across requests (for example,
   * a val in a snippet singleton object).
   *
   * @param form The form to bind. It will be recreated when processing a request.
   * @param process The function to call when the form is successfully validated.
   */
  def newBinder[A](form: => Form[A])(process: A => Unit): NodeSeq => NodeSeq = {
    create(newFormState, form)(process)
  }

  /** Creates a new `RequestVar[FormState]`, configured for initial display */
  private def newFormState: RequestVar[FormState] = {
    new RequestVar[FormState](FormState(false)) {
      override lazy val __nameSalt = Helpers.nextFuncName
    }
  }

  private def create[A](formState: RequestVar[FormState], form: => Form[A])(process: A => Unit): NodeSeq => NodeSeq = {
    // This is kind of awkward. On submission, we want to run the form right
    // away. On the other hand, we don't want to run it again when it's time
    // to display it.
    object currentResult extends TransientRequestVar[Option[BoundForm[A]]](None) {
      override lazy val __nameSalt = Helpers.nextFuncName
    }

    {
      ns: NodeSeq =>
        val (s, a) = currentResult.get
          .map(a => (formState.get, a))
          .getOrElse(form.run(Env.emptyEnv, formState.get))

        val processSubmission: Elem = SHtml.hidden(() => {
          formState.set(s.copy(renderErrors_? = true))

          val (s2, a2) = form.run(Env.paramsEnv, formState.get)

          a2.result.foreach(process)

          currentResult.set(Some(a2))
          formState.set(s2)
        })

        ns match {
          case e: Elem => e.copy(child = processSubmission ++ a.transform.apply(e.child))
          case n => n
        }
    }
  }
}
