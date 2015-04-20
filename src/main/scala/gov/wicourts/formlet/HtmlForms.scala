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

  trait Converter[A] {
    def serialize(a: A): String
    def convert(s: String): Validation[String, A]
  }

  val requiredMessage = "Please enter a value"

  /** Lifts an optional value of type `A` to a validation of type `A` */
  def required[A](a: Option[A]): Validation[String, A] = a.toSuccess(requiredMessage)

  /** Returns a [[FormValidation]] that checks that the input exists and also
    * applies a HTML5 required attribute to the form.
    */
  def html5Required[A]: FormValidation[Option[A], A] =
    FormValidation(
      a => liftStringV(required(a)),
      Some(s => s"$s [required]" #> "required"))

  /** Groups a form under a class selector of `className` with a context of the same. */
  def group[A](className: String, contents: Form[A]): Form[A] =
    contents.mapResult(aa =>
      BoundForm(
        aa.result,
        aa.metadata.clearErrorContext,
        s".$className" #> aa.binder)
    ).context(className)

  /** Binds the provided form to the provided selector and binds nested errors. */
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
        val errContext = ErrorContext(selector, errorBinder)
        BoundForm(
          setLabel(aa.result, aa.metadata.label),
          aa.metadata.setErrorContext(errContext),
          selector #> aa.binder &
            errorBinder.apply(s, selector, aa.metadata.baseSelector, aa.errors))
      }
    )

  /** An `&lt;input&gt;` form */
  def input[A](
    name: String, default: Option[A]
  )(
    implicit converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    baseInput("input", "input [name]", "input [value]", name, default)
  }

  /** An `&lt;textarea&gt;` form */
  def textarea[A](name: String, default: Option[A]
  )(
    implicit converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    baseInput("textarea", "textarea [name]", "textarea *", name, default)
  }

  /** A `SelectableOption` that can be referenced by the provided opaque value. */
  case class SelectableOptionWithNonce[+T](nonce: String, option: SelectableOption[T])

  /** Provides the UI binder for a [[choices]] form. */
  type ChoiceBinder[A] = (String, List[String], List[SelectableOptionWithNonce[A]]) => (String, CssSel)

  private def asLabeledControl[A](
    typeValue: String
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
          s"type=$typeValue [checked]" #> Some("checked").filter(_ => selectedNonces.contains(nonce))

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

  /** A list of choices rendered either as a `&lt;select multiple&gt;` or check boxes */
  def multiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]],
    asCheckboxes: Boolean = false
  ): Form[List[A]] =
    choices(
      name, default, options
    )(
      if (asCheckboxes)
        asLabeledControl("checkbox")
      else
        asSelect(true)
    )

  /** Creates a form that selects from a list of values, bound using the provided
    * [[ChoiceBinder]]. In order to select a value, the form must be run
    * using the the same [[FormState]] as that used to create the form initially.
    */
  def choices[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  )(
    binder: ChoiceBinder[A]
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

    Form(env =>
      for {
        formName <- gets[FormState, String](_.contextName(name))
        __var = FormState.newFormStateVar[SelectVar](s"__select__$formName")
        v <- __var.st
        selectOptions <- v.map(state[FormState, SelectVar]).getOrElse {
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

  /** A list of choices rendered either as a `&lt;select&gt;` or radio buttons */
  def select[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]],
    asRadioButtons: Boolean = false
  ): Form[Option[A]] =
    choice(
      name, default, options
    )(
      if (asRadioButtons)
        asLabeledControl("radio")
      else
        asSelect(false)
    )

  /** A [[choices]] form that selects a single value */
  def choice[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  )(
    binder: ChoiceBinder[A]
  ): Form[Option[A]] =
    choices(name, default.toList, options)(binder).map(_.headOption)

  /** A `&lt;input type="checkbox"&gt;` form */
  def checkbox(
    name: String, default: Boolean
  )(
    implicit converter: Converter[Boolean]
  ): Form[Boolean] = {
    def checked(in: Boolean) = {
      if (in)
        new UnprefixedAttribute("checked", "checked", Null)
      else
        Null
    }

    fresult { (env, state) =>
      val formName = state.contextName(name)
      val userInput = Env.single(env, formName)
      val formValue = userInput map converter.convert getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        FormMetadata(None, "input".some, None),
        "input" #> { ns: NodeSeq => ns match {
          case element: Elem => {
            val checkboxNs = <input type="checkbox" name={formName} value="true" />

            element.attributes.foldLeft(checkboxNs)(_ % _) % checked(formValue | default) ++
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
    implicit converter: Converter[Option[A]]
  ): Form[Option[A]] = {
    fresult { (env, state) =>
      val formName = state.contextName(name)
      val userInput = Env.single(env, formName)
      val formValue = userInput map converter.convert getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        FormMetadata(None, baseSelector.some, None),
        nameSelector #> formName & valueSelector #> (userInput | converter.serialize(default)))
    }
  }

  /** A `&lt;label&gt;` form */
  def label(label: String): Form[Unit] = sel("label *" #> label, label.some)

  /** [[Converter]] instances for standard types */
  object DefaultFieldHelpers {
    implicit val stringConverter: Converter[String] = new Converter[String] {
      override def serialize(s: String): String = s
      override def convert(s: String): Validation[String, String] = s.success
    }

    implicit val booleanConverter: Converter[Boolean] = new Converter[Boolean] {
      override def serialize(b: Boolean) = b.toString
      override def convert(s: String) = asBoolean(s).openOr(false).success
    }

    implicit val intConverter: Converter[Int] = new Converter[Int] {
      override def serialize(i: Int) = i.toString
      override def convert(s: String) = s.parseInt.leftMap(_ => "Please enter a whole number")
    }

    implicit val longConverter: Converter[Long] = new Converter[Long] {
      override def serialize(i: Long) = i.toString
      override def convert(s: String) = s.parseLong.leftMap(_ => "Please enter a whole number")
    }

    implicit def optionConverter[A](implicit c: Converter[A]): Converter[Option[A]] =
      new Converter[Option[A]] {
        override def serialize(a: Option[A]) = a.map(c.serialize).getOrElse("")
        override def convert(s: String) = c.convert(s).map(Some(_))
      }
  }

  object EmptyErrorBinder {
    implicit val errorBinder = ErrorBinder((selector, baseSelector, errors) => cssSelZero)
  }

  object FoundationErrorBinder {
    implicit val errorBinder = ErrorBinder((selector, baseSelector, errors) =>
      s"$selector [class+]" #> "error" &
        s"$selector *+" #> (errors.map(n => <small class="error">{n.error}</small>): NodeSeq)
    )
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

