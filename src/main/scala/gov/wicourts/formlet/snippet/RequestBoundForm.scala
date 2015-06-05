package gov.wicourts.formlet.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.http._

import gov.wicourts.formlet._

import xml._

object RequestBoundForm {
  trait RequestProcessor[A] {
    /** Processes a form submission */
    def process(descr: Vector[String], result: ValidationNelE[A]): Unit

    /** Renders a form */
    def render(state: MutableFormState[A], form: BoundForm[A], contents: NodeSeq): NodeSeq
  }

  def newBinder[A](f: (Vector[String], A) => Unit): Form[A] => NodeSeq => NodeSeq =
    newFormHandler[A](new MutableFormState[A](), new RequestProcessor[A] {
      def process(descr: Vector[String], result: ValidationNelE[A]): Unit =
        result.foreach(f(descr, _))
      def render(state: MutableFormState[A], form: BoundForm[A], contents: NodeSeq): NodeSeq =
        form.binder.apply(contents)
    })

  def newFormHandler[A](
    state: MutableFormState[A],
    processor: RequestProcessor[A]
  ): Form[A] => NodeSeq => NodeSeq = {
    form => ns => {
      val (s, a) = state.currentResult.get
        .map(a => (state.formState.get, a))
        .getOrElse(form.run(Env.emptyEnv, state.formState.get))

      val processSubmission: Elem = SHtml.hidden(() => {
        state.formState.set(s.copy(renderErrors_? = true))

        val (s2, a2) = form.run(Env.paramsEnv, state.formState.get)

        processor.process(a2.metadata.description, a2.result)

        state.currentResult.set(Some(a2))
        state.formState.set(s2)
      })

      ns match {
        case e: Elem => e.copy(child = processSubmission ++ processor.render(state, a, e.child))
        case n => n
      }
    }
  }
}

/** An class whose instances contain the Lift-specific state necessary
  * to run a form. The same instance should generally be used across all
  * requests for a particular form.
  */
class MutableFormState[A] {
  // This is kind of awkward. On submission, we want to run the form right
  // away. On the other hand, we don't want to run it again when it's time to
  // display it.
  object currentResult extends TransientRequestVar[Option[BoundForm[A]]](None) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  object formState extends RequestVar[FormState](FormState(false)) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  def capture: () => Unit = {
    val r = currentResult.get
    val s = formState.get

    () => {
      currentResult.set(r)
      formState.set(s)
    }
  }
}
