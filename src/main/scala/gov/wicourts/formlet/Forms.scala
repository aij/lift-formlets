package gov.wicourts.formlet

import net.liftweb.http.S
import net.liftweb.util._
import net.liftweb.util.Helpers.{^ => _, _}

import xml._

import scalaz._
import Scalaz._

import scala.language.implicitConversions

object Forms {
  type Env = String => List[String]

  type ValidationNelE[A] = ValidationNel[FieldError,A]
  type ValidationNelS[A] = ValidationNel[String,A]

  trait FieldError {
    def render: NodeSeq
  }

  case class FieldErrorN(render: NodeSeq) extends FieldError
  case class FieldErrorS(s: String) extends FieldError {
    def render = Text(s)
  }

  case class FieldValue[A](name: Option[String], value: A)

  case class FormState private [formlet] (values: Map[String,Any], lastId: Int) {
    def incLastId(): FormState = copy(lastId = lastId+1)
  }

  object FormState {
    def apply(): FormState = FormState(Map[String,Any](), 1)
  }

  /**
    * Represents the result of running a form
    */
  case class BoundForm[A](result: ValidationNelE[A], name: Option[String], transform: CssSel) {
    def errors: List[NodeSeq] = result.fold(_.map(_.render).toList, _ => Nil)
  }

  case class Form[A](runForm: Env => State[FormState,BoundForm[A]]) {
    import Form._

    def run(env: Env, initialState: FormState = FormState()): (FormState, BoundForm[A]) =
      runForm(env).run(initialState)

    def runEmpty(): (FormState, BoundForm[A]) = run(emptyEnv)

    def memoize: Form[A] = {
      val __var = new FormStateVar[BoundForm[A]]
      Form(env =>
        for {
          v <- gets[FormState, Option[BoundForm[A]]](__var.get)
          w <- v.map(b => state[FormState,BoundForm[A]](b)).getOrElse(for {
              aa <- this.runForm(env)
              _ <- modify[FormState](s => __var.set(s, aa))
            } yield aa)
        } yield w)
    }

    // Handy combinator from Formaggio
    def <++(fu: Form[Unit]): Form[A] =
      Form(env =>
        for {
          aa <- this.runForm(env)
          uu <- fu.runForm(env)
        } yield BoundForm(
          aa.result,
          combineNames(aa, uu),
          aa.transform & uu.transform))

    def appendU(fu: Form[Unit]): Form[A] = <++(fu)

    def mapV[B](f: A => ValidationNelE[B]): Form[B] = this.map(f).liftV

    def mapStringV[B](f: A => Validation[String,B]): Form[B] =
      this.map(f(_).toValidationNel).liftStringV

    def ??(fs: (A => Validation[String,A])*): Form[A] =
      this.map(a => traverseVals(a, fs.toList)).liftStringV

    private def traverseVals(a: A, l: List[A => Validation[String,A]]): ValidationNelS[A] =
      l.traverseU(_(a).toValidationNel).map(_ => a)

    private def foldValResult(a: ValidationNelE[ValidationNelS[A]]): ValidationNelE[A] =
      a.fold(_.failure[A], Form.liftNelStringV(_))

    def val2[B](b: Form[B])(fs: ((FieldValue[B], A) => Validation[String,A])*): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          aa <- this.runForm(env)
        } yield {
          val result =
            foldValResult(
              ^(bb.result, aa.result)((b, a) =>
                traverseVals(a, fs.toList.map(_.curried(FieldValue(bb.name, b))))))
          BoundForm(result, aa.name, aa.transform)
        })

    def val3[B,C](
      b: Form[B], c: Form[C]
    )(
      fs: ((FieldValue[B], FieldValue[C], A
    ) => Validation[String,A])*): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          cc <- c.runForm(env)
          aa <- this.runForm(env)
        } yield {
          val result =
            foldValResult(
              ^^(bb.result, cc.result, aa.result)((b, c, a) =>
                traverseVals(a, fs.toList.map(f =>
                  f(FieldValue(bb.name, b), FieldValue(cc.name, c), _: A)))))
          BoundForm(result, aa.name, aa.transform)
        })

    def val4[B,C,D](
      b: Form[B], c: Form[C], d: Form[D]
    )(
      fs: ((FieldValue[B], FieldValue[C], FieldValue[D], A) => Validation[String,A])*
    ): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          cc <- c.runForm(env)
          dd <- d.runForm(env)
          aa <- this.runForm(env)
        } yield {
          val result =
            foldValResult(
              ^^^(bb.result, cc.result, dd.result, aa.result)((b, c, d, a) =>
                traverseVals(a, fs.toList.map(f =>
                  f(FieldValue(bb.name, b), FieldValue(cc.name, c), FieldValue(dd.name, d), _: A)))))
          BoundForm(result, aa.name, aa.transform)
        })

    def liftV[C](implicit ev: A <:< ValidationNelE[C]): Form[C] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield BoundForm(
          aa.result.fold(_.failure[C], a => a: ValidationNelE[C]),
          aa.name,
          aa.transform))

    def liftStringV[C](implicit ev: A <:< ValidationNelS[C]): Form[C] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield BoundForm(
          aa.result.fold(_.failure[C], a => Form.liftNelStringV(a)),
          aa.name,
          aa.transform))
  }

  trait FormInstances {
    implicit val formApplicative: Applicative[Form] = new Applicative[Form] {
      override def map[A, B](a: Form[A])(f: A => B): Form[B] =
        Form(env =>
          for (aa <- a.runForm(env))
          yield BoundForm(aa.result map f, aa.name, aa.transform))

      override def point[A](a: => A) =
        Form(env =>
          for (_ <- get[FormState])
          yield BoundForm(a.success, None, ".reiwejklasjkfdssfdasfkjl" #> NodeSeq.Empty))

      override def ap[A,B](fa: => Form[A])(f: => Form[A=>B]): Form[B] =
        Form(env =>
          for {
            aa <- fa.runForm(env)
            ff <- f.runForm(env)
          } yield BoundForm(
            aa.result <*> ff.result,
            Form.combineNames(aa, ff),
            aa.transform & ff.transform))
    }
  }

  object Form extends FormInstances {
    val F = Applicative[Form]

    def combineNames[A,B](a: BoundForm[A], b: BoundForm[B]): Option[String] = {
      Some(List(a.name, b.name).flatten.mkString(" and ")).filterNot(_.isEmpty)
    }

    def failing[A](message: String): Form[A] = {
      F.point(message.failure.toValidationNel).liftStringV
    }

    def sel(sel: CssSel, name: Option[String] = None): Form[Unit] =
      fresult { env => BoundForm(().success, name, sel) }

    def fresult[A](f: Env => BoundForm[A]): Form[A] =
      Form(env => for (_ <- get[FormState]) yield f(env))

    def liftStringV[A](in: Validation[String,A]): ValidationNelE[A] =
      in.leftMap(FieldErrorS(_)).toValidationNel

    def liftNelStringV[A](in: ValidationNelS[A]): ValidationNelE[A] =
      in.leftMap(_.map(FieldErrorS(_)))
  }

  class FormStateVar[T] {
    private val key = nextFuncName

    def get(formState: FormState): Option[T] =
      formState.values.get(key).map(_.asInstanceOf[T])

    def set(formState: FormState, value: T): FormState =
      formState.copy(values = formState.values + (key -> value))
  }

  def paramsEnv: Env = s =>
    S.params(s).map(_.trim).filter(_.isEmpty)

  def emptyEnv: Env = _ => Nil

  def single(env: Env, name: String): Option[String] = env(name).headOption

}

trait HtmlForms {
  import Forms._
  import Form._

  type Serializer[A] = A => String
  type Converter[A] = String => Validation[String,A]

  val requiredMessage = "This field is required"

  def required[A](a: Option[A]): Validation[String,A] = a.toSuccess(requiredMessage)

  def field[A](selector: String, contents: Form[A]): Form[A] =
    Form(env =>
      for {
        aa <- contents.runForm(env)
      } yield {
        // XXX This needs to be more flexible
        val errorsCssSel = ".errors *" #> (aa.errors.map(n => <div>{n}</div>): NodeSeq)
        BoundForm(aa.result, aa.name, selector #> (aa.transform & errorsCssSel))
      }
    )

  def input[A](name: String, default: => Option[A])(
    implicit serializer: Serializer[Option[A]], converter: Converter[Option[A]]): Form[Option[A]] = {
    fresult { env =>
      val userInput = single(env, name)
      val formValue = userInput map converter getOrElse default.success
      BoundForm(liftStringV(formValue), None, "input [value]" #> (userInput | serializer(default)))
    }
  }

  def label(label: String): Form[Unit] = sel("label *" #> label, Some(label))

  private def htmlId(s: FormState) = "f" + s.lastId

  object DefaultFieldHelpers {
    // implicit val stringSerializer: Serializer[String] = _.toString
    implicit val stringConverter: Converter[String] = s => s.success

    implicit def optionSerializer[A](implicit s: Serializer[A]): Serializer[Option[A]] =
      _.map(s(_)).getOrElse("")

    implicit def optionConverter[A](implicit c: Converter[A]): Converter[Option[A]] =
      c(_).map(Some(_))
  }

}

trait LowPriorityHtmlFormsFunctions extends HtmlForms {
  H =>

  import Forms._
  import Form._

  class FormOps[A](form: Form[A]) {}

  implicit def formToFormOps[A](form: Form[A]): FormOps[A] =
    new FormOps(form)
}

trait HtmlFormsFunctions extends LowPriorityHtmlFormsFunctions {
  H =>

  import Forms._
  import Form._

  class OptFormOps[A](form: Form[Option[A]]) {
    def required: Form[A] = form.mapStringV(H.required _)
  }

  implicit def optFormToOptFormOps[A](form: Form[Option[A]]): OptFormOps[A] =
    new OptFormOps(form)
}

object HtmlForms extends HtmlForms with HtmlFormsFunctions

