package gov.wicourts.formlet

import net.liftweb.http.{S, FileParamHolder}
import net.liftweb.http.SHtml.{BasicElemAttr, SelectableOption}
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

  def asLabeledControl[A](
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

  def checkboxMultiSelect[A](
    name: String,
    default: List[A],
    options: List[SelectableOption[A]]
  ): Form[List[A]] =
    multiSelect(name, default, options)(asLabeledControl("checkbox", "checked"))

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

        BoundForm(liftStringV(formValue.success), None, selector.some, sel)
      }
    )
  }

  def selectSelect[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  ): Form[Option[A]] =
    select(name, default, options)(asSelect(false))

  def radioSelect[A](
    name: String,
    default: Option[A],
    options: List[SelectableOption[A]]
  ): Form[Option[A]] =
    select(name, default, options)(asLabeledControl("radio", "selected"))

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
        "input".some,
        "input" #> { ns: NodeSeq => ns match {
          case element: Elem => {
            val checkboxNs = <input type="checkbox" name={formName} value={serializer(formValue | default)} />

            element.attributes.foldLeft(checkboxNs)(_ % _) ++
            <input type="hidden" name={formName} value="false" />
          }
        }})
      }
    }

  def file(name: String): Form[Option[FileParamHolder]] = {
    fresult { (env, state) =>
      val formName = state.contextName(name)
      val result = env.file(formName)

      BoundForm(
        result.success,
        None,
        "input".some,
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
      val userInput = single(env, formName)
      val formValue = userInput map converter getOrElse default.success
      BoundForm(
        liftStringV(formValue),
        None,
        baseSelector.some,
        nameSelector #> formName & valueSelector #> (userInput | serializer(default)))
    }
  }

  def label(label: String): Form[Unit] = sel("label *" #> label, label.some)

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

