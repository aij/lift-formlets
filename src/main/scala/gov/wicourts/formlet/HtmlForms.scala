package gov.wicourts.formlet

import net.liftweb.http.{S, FileParamHolder}
import net.liftweb.http.SHtml.SelectableOption
import net.liftweb.util._
import net.liftweb.util.Helpers.{^ => _, _}

import xml._

import scalaz._
import Scalaz._

import scala.language.implicitConversions

import gov.wicourts.formlet.Forms._
import gov.wicourts.formlet.Forms.Form._

trait HtmlForms {
  import FormHelpers._

  type Serializer[A] = A => String
  type Converter[A] = String => Validation[String,A]

  val requiredMessage = "This field is required"

  def required[A](a: Option[A]): Validation[String,A] = a.toSuccess(requiredMessage)

  def html5Required[A]: FormValidation[Option[A],A] =
    FormValidation(
      a => liftStringV(required(a)),
      Some(s => (s + " [required]") #> "required"))

  def field[A](selector: String, contents: Form[A]): Form[A] =
    Form(env =>
      for {
        aa <- contents.runForm(env)
      } yield {
        // XXX This needs to be more flexible
        val errorsCssSel = ".errors *" #> (aa.errors.map(n => <div>{n}</div>): NodeSeq)
        BoundForm(
          setLabel(aa.result, aa.name),
          aa.name,
          aa.baseSelector,
          selector #> (aa.transform & errorsCssSel))
      }
    )

  def input[A](name: String, default: Option[A])(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]): Form[Option[A]] = {
    baseInput("input", "input [name]", "input [value]", name, default)
  }

  def textarea[A](name: String, default: Option[A])(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]): Form[Option[A]] = {
    baseInput("textarea", "textarea [name]", "textarea *", name, default)
  }

  case class SelectableOptionWithNonce[+T](nonce: String, option: SelectableOption[T])

  type SelectTransformer[A] = (String, List[String], List[SelectableOptionWithNonce[A]]) => (String, CssSel)

  def asSelect[A](
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

  def selectMultiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  ): Form[List[A]] =
    multiSelect(name, default, options)(asSelect(true))

  def multiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  )(
    transform: SelectTransformer[A]
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
    val __var = new FormStateVar[SelectVar]

    Form(env =>
      for {
        v <- gets[FormState, Option[SelectVar]](__var.get)
        formName <- gets[FormState, String](_.contextName(name))
        selectOptions <- v.map(state[FormState,SelectVar]).getOrElse {
          val selectState = noncedOptions
          for {
            _ <- modify[FormState](s => __var.set(s, selectState))
          } yield selectState
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

        val (selector, sel) = transform(formName, nonces, options)

        BoundForm(liftStringV(formValue.success), None, Some(selector), sel)
      }
    )
  }

  def selectSelect[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  ): Form[Option[A]] =
    select(name, default, options)(asSelect(false))

  def select[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  )(
    transform: SelectTransformer[A]
  ): Form[Option[A]] =
    multiSelect(name, default.toList, options)(transform).map(_.headOption)

  def checkbox(
    name: String, default: Boolean
  )(
    implicit serializer: Serializer[Boolean], converter: Converter[Boolean]
  ): Form[Boolean] = {
    fresult { (env, state) =>
      val formName = state.contextName(name)
      val userInput = single(env, formName)
      val formValue = userInput map converter getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        None,
        Some("input"),
        "input" #> { ns: NodeSeq => ns match {
          case element: Elem => {
            val checkboxNs = <input type="checkbox" name={formName} value={serializer(formValue | default)} />

            element.attributes.foldLeft(checkboxNs)(_ % _) ++
            <input type="hidden" name={formName} value="false" />
          }
        }})
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
      val userInput = single(env, formName)
      val formValue = userInput map converter getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        None,
        Some(baseSelector),
        nameSelector #> formName & valueSelector #> (userInput | serializer(default)))
    }
  }

  def label(label: String): Form[Unit] = sel("label *" #> label, Some(label))

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

  class FormOps[A](form: Form[A]) {}

  implicit def formToFormOps[A](form: Form[A]): FormOps[A] =
    new FormOps(form)
}

trait HtmlFormsFunctions extends LowPriorityHtmlFormsFunctions {
  H =>

  class OptFormOps[A](form: Form[Option[A]]) {
    def required: Form[A] = form.mapStringV(H.required _)
    def html5Required: Form[A] = form.mapV(H.html5Required)
  }

  implicit def optFormToOptFormOps[A](form: Form[Option[A]]): OptFormOps[A] =
    new OptFormOps(form)
}

object HtmlForms extends HtmlForms with HtmlFormsFunctions

