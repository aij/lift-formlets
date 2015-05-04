package gov.wicourts

import scalaz.{ValidationNel, Validation}

import net.liftweb.util.Helpers.{^ => _, _}
import net.liftweb.util.{A => _, _}

/**
 * A implementation of Formlets using the Lift framework.
 *
 * References:
 *  - [[http://groups.inf.ed.ac.uk/links/formlets/]]
 */
package object formlet {
  private [formlet] val cssSelZero = "nothing" #> PassThru

  /** A form's value is provided as an instance of this type */
  type ValidationNelE[A] = ValidationNel[FormError,A]

  /**
   * A string validation is an instance of this type. It may be lifted to a
   * [[ValidationNelE]].
   */
  type ValidationNelS[A] = ValidationNel[String,A]

  /** Lifts a function returning a String Validation to a [[FormValidation]]. */
  def StringValidation[A](f: A => Validation[String,A]): FormValidation[A,A] =
    FormValidation(a => FormValidation.liftStringV(f(a)), None)
}
