package gov.wicourts.formlet

import net.liftweb.http.{S, FileParamHolder}
import net.liftweb.http.SHtml.{BasicElemAttr, SelectableOption}
import net.liftweb.util._
import net.liftweb.util.Helpers.{^ => _, _}

import xml._

import scalaz._
import Scalaz._

import gov.wicourts.formlet.Form._

/** Provides combinators for working with HTML forms */
trait HtmlForms {
  import FormHelpers._

  /** Serializes a values of type `A` to a `String`. */
  type Serializer[A] = A => String

  /** Converts an input `String` to a value of type `A` */
  type Converter[A] = String => Validation[String,A]

  val requiredMessage = "This field is required"

  /** Lifts an optional value of type `A` to a validation of type `A` */
  def required[A](a: Option[A]): Validation[String,A] = a.toSuccess(requiredMessage)

  /**
   * Returns a [[FormValidation]] that checks that the input exists and also
   * applies a HTML5 required attribute to the form.
   */
  def html5Required[A]: FormValidation[Option[A],A] =
    FormValidation(
      a => liftStringV(required(a)),
      Some(s => (s + " [required]") #> "required"))

  implicit val defaultErrorBinder = ErrorBinder((show, errors) =>
    if (show)
      // XXX This needs to be more flexible
      ".errors *" #> (errors.map(n => <div>{n.error}</div>): NodeSeq)
    else
      cssSelZero
  )

  /**
   * Binds the provided form to the provided selector and collects and
   * binds nested errors.
   */
  def field[A](
    selector: String, contents: Form[A]
  )(
    implicit errorBinder: ErrorBinder
  ): Form[A] =
    Form(env =>
      for {
        s <- get[FormState]
        aa <- contents.runForm(env)
      } yield {
        val errContext = ErrorContext(selector, aa.binder, errorBinder)
        BoundForm(
          setLabel(aa.result, aa.metadata.label),
          aa.metadata.setErrorContext(errContext),
          selector #> (aa.binder & errorBinder.run(s.renderErrors_?, aa.errors)))
      }
    )

  /** An `&lt;input&gt;` form */
  def input[A](name: String, default: Option[A])(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]): Form[Option[A]] = {
    baseInput("input", "input [name]", "input [value]", name, default)
  }

  /** An `&lt;textarea&gt;` form */
  def textarea[A](name: String, default: Option[A])(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]): Form[Option[A]] = {
    baseInput("textarea", "textarea [name]", "textarea *", name, default)
  }

  /** A `SelectableOption` that can be referenced by the provided opaque value. */
  case class SelectableOptionWithNonce[+T](nonce: String, option: SelectableOption[T])

  /** Provides the UI binder for a [[multiSelect]]. */
  type SelectBinder[A] = (String, List[String], List[SelectableOptionWithNonce[A]]) => (String, CssSel)

  private def asLabeledControl[A](
    typeValue: String,
    selectedAttr: String
  )(
    name: String, selectedNonces: List[String], noncedOptions: List[SelectableOptionWithNonce[A]]
  ): (String, CssSel) = {
    val binder =
      "label" #> noncedOptions.map { noncedOption =>
        val nonce = noncedOption.nonce
        val option = noncedOption.option
        val controlBinder =
          s"type=$typeValue [name]" #> name &
          s"type=$typeValue [value]" #> nonce &
          s"type=$typeValue [$selectedAttr]" #> Some(selectedAttr).filter(_ => selectedNonces.contains(nonce))

        val (idAttribute, controlBinderWithAttributes) =
          option.attrs.foldLeft((None: Option[String], controlBinder)) { (binderSoFar, attribute) =>
            attribute match {
              case BasicElemAttr(name, value) =>
                val updatedBinder =
                  binderSoFar._2 &
                  (s"type=$typeValue [$name]") #> value

                if (name == "id")
                  (value.some, updatedBinder)
                else
                  (binderSoFar._1, updatedBinder)
              case _ =>
                binderSoFar
            }
          }

        "label" #> { ns: NodeSeq => ns match {
          case label: Elem if label.label == "label" =>
            val nonTextChildren = label.child.filterNot(_.isInstanceOf[Text])

            val updatedLabel =
              idAttribute.foldLeft(label.copy(child = nonTextChildren ++ Text(" " + option.label)))(_ % ("for", _))

            controlBinderWithAttributes apply updatedLabel
        } } & controlBinderWithAttributes
      }
    ("label", binder)
  }

  private def asSelect[A](
    multiple: Boolean
  )(
    name: String, selectedNonces: List[String], options: List[SelectableOptionWithNonce[A]]
  ): (String, CssSel) = {
     def selected(in: Boolean) = {
       if (in)
         new UnprefixedAttribute("selected", "selected", Null)
       else
         Null
     }

    val multipleSel =
      if (multiple)
        "select [multiple]" #> "multiple"
      else
        cssSelZero
    val nameSel =
      "select [name]" #> name

    val optionsNs =
      options.map { optionWithNonce =>
        val nonce = optionWithNonce.nonce
        val option = optionWithNonce.option
        option.attrs.foldLeft(<option value={nonce}>{option.label}</option>)(_ % _) %
          selected(selectedNonces.contains(nonce))
      }

    ("select", multipleSel & nameSel & "select *" #> <xml:group>{optionsNs}</xml:group>)
  }

  /** A [[multiSelect]] rendered as a `&lt;select multiple&gt;` */
  def selectMultiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  ): Form[List[A]] =
    multiSelect(name, default, options)(asSelect(true))

  /** A [[multiSelect]] rendered using `&lt;input type="checkbox"&gt;` */
  def checkboxMultiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  ): Form[List[A]] =
    multiSelect(name, default, options)(asLabeledControl("checkbox", "checked"))

  /**
   * Creates a form that selects from a list of values, bound using the provided
   * [[SelectBinder]]. In order to select a value, the form must be run
   * using the the same [[FormState]] as that used to create the form initially.
   */
  def multiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  )(
    binder: SelectBinder[A]
  ): Form[List[A]] = {

    def noncedOptions = {
      val secure = options.map { selectableOption =>
        SelectableOptionWithNonce(randomString(20), selectableOption)
      }

      val defaultNonces = default.flatMap { default =>
        secure.find(_.option.value == default).map(_.nonce)
      }

      (defaultNonces, secure)
    }

    type SelectVar = (List[String], List[SelectableOptionWithNonce[A]])
    val __var = FormState.newFormStateVar[SelectVar]

    Form(env =>
      for {
        formName <- gets[FormState, String](_.contextName(name))
        v <- __var.st
        selectOptions <- v.map(state[FormState,SelectVar]).getOrElse {
          val r = noncedOptions
          (__var := r.some).map(_ => r)
        }
      } yield {
        val (defaultNonces, options) = selectOptions
        val values = env.param(formName).flatMap(v => options.find(_.nonce == v))

        val formValue =
          if (values.nonEmpty) values.map(_.option.value)
          else default

        val nonces =
          if (values.nonEmpty) values.map(_.nonce)
          else defaultNonces

        val (selector, sel) = binder(formName, nonces, options)

        BoundForm(
          liftStringV(formValue.success),
          FormMetadata(None, selector.some, None),
          sel
        )
      }
    )
  }

  /** A [[select]] rendered as a `&lt;select&gt;` */
  def selectSelect[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  ): Form[Option[A]] =
    select(name, default, options)(asSelect(false))

  /** A [[select]] rendered using `&lt;input type="radio"&gt;` */
  def radioSelect[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  ): Form[Option[A]] =
    select(name, default, options)(asLabeledControl("radio", "selected"))

  /** A [[multiSelect]] that selects a single value */
  def select[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  )(
    binder: SelectBinder[A]
  ): Form[Option[A]] =
    multiSelect(name, default.toList, options)(binder).map(_.headOption)

  /** A `&lt;input type="checkbox"&gt;` form */
  def checkbox(
    name: String, default: Boolean
  )(
    implicit serializer: Serializer[Boolean], converter: Converter[Boolean]
  ): Form[Boolean] = {
    fresult { (env, state) =>
      val formName = state.contextName(name)
      val userInput = Env.single(env, formName)
      val formValue = userInput map converter getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        FormMetadata(None, "input".some, None),
        "input" #> { ns: NodeSeq => ns match {
          case element: Elem => {
            val checkboxNs = <input type="checkbox" name={formName} value={serializer(formValue | default)} />

            element.attributes.foldLeft(checkboxNs)(_ % _) ++
            <input type="hidden" name={formName} value="false" />
          }
        }})
      }
    }

  /** A `&lt;input type="file"&gt;` form */
  def file(name: String): Form[Option[FileParamHolder]] = {
    fresult { (env, state) =>
      val formName = state.contextName(name)
      val result = env.file(formName)

      BoundForm(
        result.success,
        FormMetadata(None, "input".some, None),
        "input [name]" #> formName & "input [type]" #> "file"
      )
    }
  }

  private def baseInput[A](
    baseSelector: String,
    nameSelector: String,
    valueSelector: String,
    name: String,
    default: Option[A]
  )(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    fresult { (env, state) =>
      val formName = state.contextName(name)
      val userInput = Env.single(env, formName)
      val formValue = userInput map converter getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        FormMetadata(None, baseSelector.some, None),
        nameSelector #> formName & valueSelector #> (userInput | serializer(default)))
    }
  }

  /** A `&lt;label&gt;` form */
  def label(label: String): Form[Unit] = sel("label *" #> label, label.some)

  /** [[Converter]] and [[Serializer]] instances for standard types */
  object DefaultFieldHelpers {
    implicit val stringConverter: Converter[String] = s => s.success

    implicit val booleanSerializer: Serializer[Boolean] = _.toString

    implicit val booleanConverter: Converter[Boolean] = s =>
      asBoolean(s).openOr(false).success

    implicit def optionSerializer[A](implicit s: Serializer[A]): Serializer[Option[A]] =
      _.map(s(_)).getOrElse("")

    implicit def optionConverter[A](implicit c: Converter[A]): Converter[Option[A]] =
      c(_).map(Some(_))
  }

}

trait LowPriorityHtmlFormsFunctions extends HtmlForms {
  H =>

  import scala.language.implicitConversions

  class FormOps[A](form: Form[A]) {}

  implicit def formToFormOps[A](form: Form[A]): FormOps[A] =
    new FormOps(form)
}

trait HtmlFormsFunctions extends LowPriorityHtmlFormsFunctions {
  H =>

  import scala.language.implicitConversions

  class OptFormOps[A](form: Form[Option[A]]) {
    def required: Form[A] = form.mapStringV(H.required _)
    def html5Required: Form[A] = form.mapV(H.html5Required)
  }

  implicit def optFormToOptFormOps[A](form: Form[Option[A]]): OptFormOps[A] =
    new OptFormOps(form)
}

object HtmlForms extends HtmlForms with HtmlFormsFunctions

