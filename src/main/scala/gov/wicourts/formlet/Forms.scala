package gov.wicourts.formlet

import net.liftweb.http.{S, FileParamHolder}
import net.liftweb.util.{A => _, _}
import net.liftweb.util.Helpers.{^ => _, _}

import xml._

import scalaz._
import Scalaz._

import scala.language.implicitConversions

object Forms {
  val cssSelZero = "nothing" #> PassThru

  trait Env {
    def param(name: String): List[String]
    def file(name: String): Option[FileParamHolder]
  }

  type ValidationNelE[A] = ValidationNel[FormError,A]
  type ValidationNelS[A] = ValidationNel[String,A]

  case class FormError(
    render: NodeSeq,
    label: Option[String] = None,
    transform: Option[String => CssSel] = None
  )

  case class FormValue[A](name: Option[String], value: A)

  case class FormState (private [formlet] values: Map[String,Any])

  object FormState {
    def apply(): FormState = FormState(Map[String,Any]())
  }

  case class BoundForm[A](
    result: ValidationNelE[A],
    name: Option[String],
    baseSelector: Option[String],
    transform: CssSel
  ) {
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
          w <- v.map(state[FormState,BoundForm[A]]).getOrElse(for {
              aa <- this.runForm(env)
              _ <- modify[FormState](s => __var.set(s, aa))
            } yield aa)
        } yield w)
    }

    def map[B](f: A => B): Form[B] =
      Form(env =>
        for (aa <- this.runForm(env))
        yield aa.copy(result = aa.result map f))

    def ap[B](f: => Form[A=>B]): Form[B] =
      Form(env =>
        for {
          aa <- this.runForm(env)
          ff <- f.runForm(env)
        } yield BoundForm(
          aa.result <*> ff.result,
          Form.combineNames(aa, ff),
          Form.combineBaseSelectors(aa, ff),
          aa.transform & ff.transform))

    private def extractErrorTransforms[B](
      result: ValidationNelE[B],
      baseSelector: Option[String]
    ): List[CssSel] = {
      for {
        sel <- baseSelector.toList
        errs <- result.swap.toList
        err <- errs.toList
        f <- err.transform
      } yield f(sel)
    }

    def mapV[B](f: A => ValidationNelE[B]): Form[B] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield {
          if (aa.result.isFailure)
            aa.copy(result = aa.result map f) // Just to get the types right
          else {
            val m = aa.result map f
            val errTransforms = extractErrorTransforms(m, aa.baseSelector)

            BoundForm(m, aa.name, aa.baseSelector, composeTransform(aa, errTransforms))
          }
        }).liftV

    def mapStringV[B](f: A => Validation[String,B]): Form[B] =
      this.mapV(a => Form.liftStringV(f(a)))

    def ??(fs: (A => Validation[FormError,A])*): Form[A] =
      this.mapV(a => traverseVals(a, fs.toList))

    private def traverseVals(a: A, l: List[A => Validation[FormError,A]]): ValidationNelE[A] =
      l.traverseU(_(a).toValidationNel).map(_ => a)

    private def foldValResult(a: ValidationNelE[ValidationNelE[A]]): ValidationNelE[A] =
      a.fold(_.failure[A], identity)

    private def composeTransform(form: BoundForm[A], l: List[CssSel]): CssSel =
      l.foldLeft(form.transform)((b, a) => b & a)

    def val2[B](b: Form[B])(fs: ((FormValue[B], A) => Validation[FormError,A])*): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          aa <- this.runForm(env)
        } yield {
          if (List(bb, aa).exists(_.result.isFailure))
            aa
          else {
            val result =
              foldValResult(
                ^(bb.result, aa.result)((b, a) =>
                  traverseVals(a, fs.toList.map(_.curried(FormValue(bb.name, b))))))
            val errTransforms = extractErrorTransforms(result, aa.baseSelector)
            BoundForm(result, aa.name, aa.baseSelector, composeTransform(aa, errTransforms))
          }
        })

    def val3[B,C](
      b: Form[B], c: Form[C]
    )(
      fs: ((FormValue[B], FormValue[C], A
    ) => Validation[FormError,A])*): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          cc <- c.runForm(env)
          aa <- this.runForm(env)
        } yield {
          if (List(bb, cc, aa).exists(_.result.isFailure))
            aa
          else {
            val result =
              foldValResult(
                ^^(bb.result, cc.result, aa.result)((b, c, a) =>
                  traverseVals(a, fs.toList.map(f =>
                    f(FormValue(bb.name, b), FormValue(cc.name, c), _: A)))))
            val errTransforms = extractErrorTransforms(result, aa.baseSelector)
            BoundForm(result, aa.name, aa.baseSelector, composeTransform(aa, errTransforms))
          }
        })

    def val4[B,C,D](
      b: Form[B], c: Form[C], d: Form[D]
    )(
      fs: ((FormValue[B], FormValue[C], FormValue[D], A) => Validation[FormError,A])*
    ): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          cc <- c.runForm(env)
          dd <- d.runForm(env)
          aa <- this.runForm(env)
        } yield {
          if (List(bb, cc, dd, aa).exists(_.result.isFailure))
            aa
          else {
            val result =
              foldValResult(
                ^^^(bb.result, cc.result, dd.result, aa.result)((b, c, d, a) =>
                  traverseVals(a, fs.toList.map(f =>
                    f(FormValue(bb.name, b), FormValue(cc.name, c), FormValue(dd.name, d), _: A)))))
            val errTransforms = extractErrorTransforms(result, aa.baseSelector)
            BoundForm(result, aa.name, aa.baseSelector, composeTransform(aa, errTransforms))
          }
        })

    def liftV[C](implicit ev: A <:< ValidationNelE[C]): Form[C] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield BoundForm(
          aa.result.fold(_.failure[C], a => a: ValidationNelE[C]),
          aa.name,
          aa.baseSelector,
          aa.transform))

    def liftStringV[C](implicit ev: A <:< ValidationNelS[C]): Form[C] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield BoundForm(
          aa.result.fold(_.failure[C], a => Form.liftNelStringV(a)),
          aa.name,
          aa.baseSelector,
          aa.transform))
  }

  trait FormInstances {
    implicit val formApplicative: Applicative[Form] = new Applicative[Form] {
      override def map[A, B](a: Form[A])(f: A => B): Form[B] = a map f

      override def point[A](a: => A) =
        Form(env =>
          for (_ <- get[FormState])
          yield BoundForm(a.success, None, None, cssSelZero))

      override def ap[A,B](fa: => Form[A])(f: => Form[A=>B]): Form[B] =
        fa ap f
    }
  }

  object Form extends FormInstances {
    val F = Applicative[Form]

    def combineNames[A,B](a: BoundForm[A], b: BoundForm[B]): Option[String] = {
      Some(List(a.name, b.name).flatten.mkString(" and ")).filterNot(_.isEmpty)
    }

    def combineBaseSelectors[A,B](a: BoundForm[A], b: BoundForm[B]): Option[String] = {
      (a.baseSelector, b.baseSelector) match {
        case (None, None) | (Some(_), Some(_)) => None
        case (None, s@Some(_)) => s
        case (s@Some(_), None) => s
      }
    }

    def failing[A](message: String): Form[A] = {
      F.point(message.failure.toValidationNel).liftStringV
    }

    def sel(sel: CssSel, name: Option[String] = None): Form[Unit] =
      fresult { env => BoundForm(().success, name, None, sel) }

    def fresult[A](f: Env => BoundForm[A]): Form[A] =
      Form(env => for (_ <- get[FormState]) yield f(env))

    def liftStringV[A](in: Validation[String,A]): ValidationNelE[A] =
      in.leftMap(s => FormError(Text(s))).toValidationNel

    def liftNelStringV[A](in: ValidationNelS[A]): ValidationNelE[A] =
      in.leftMap(_.map(s => FormError(Text(s))))

    def setLabel[A](in: ValidationNelE[A], label: Option[String]): ValidationNelE[A] =
      in.leftMap(_.map(_.copy(label = label)))
  }

  class FormStateVar[T] {
    private val key = nextFuncName

    def get(formState: FormState): Option[T] =
      formState.values.get(key).map(_.asInstanceOf[T])

    def set(formState: FormState, value: T): FormState =
      formState.copy(values = formState.values + (key -> value))
  }

  def paramsEnv: Env = new Env {
    def param(s: String) = S.params(s).map(_.trim).filter(_.isEmpty)
    def file(s: String) = S.request.flatMap(_.uploadedFiles.find(_.name == s))
  }

  def emptyEnv: Env = new Env {
    def param(s: String) = Nil
    def file(s: String) = None
  }

  def singleEnv(m: Map[String, String]): Env = new Env {
    def param(s: String) = m.get(s).toList
    def file(s: String) = None
  }

  def multiEnv(m: Map[String, List[String]]): Env = new Env {
    def param(s: String) = m.getOrElse(s, Nil)
    def file(s: String) = None
  }

  def single(env: Env, name: String): Option[String] = env.param(name).headOption

  object FormsHelpers extends LowPriorityFormsHelpersImplicits {
    // For the benefit of Failure
    implicit def stringNothingToFormErrorV(in: Validation[String,Nothing]): Validation[FormError,Nothing] =
      in.leftMap(s => FormError(Text(s)))
  }

  trait LowPriorityFormsHelpersImplicits {
    implicit def stringToFormErrorV[A](in: Validation[String,A]): Validation[FormError,A] =
      in.leftMap(s => FormError(Text(s)))
  }
}

