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

  def StringValidation[A](f: A => Validation[String,A]): FormValidation[A,A] =
    FormValidation(a => FormHelpers.liftStringV(f(a)), None)

  case class FormValidation[A,B](
    validation: A => ValidationNelE[B],
    transform: Option[String => CssSel] = None
  ) {
    def map[C](f: B => C): FormValidation[A,C] =
      FormValidation(a => validation(a) map f, transform)

    def ap[C](f: => FormValidation[A,B => C]): FormValidation[A,C] =
      FormValidation(
        a => this.validation(a) <*> f.validation(a),
        (this.transform, f.transform) match {
          case (Some(t1), Some(t2)) => Some((s: String) => t1(s) & t2(s))
          case (f1@Some(_), None) => f1
          case (None, f2@Some(_)) => f2
          case (None, None) => None
        }
      )

    def flatten[C](f: A => C)(implicit ev: B <:< List[C]): FormValidation[A,C] =
      new FormValidation(a => this.validation(a).map(_ => f(a)), this.transform)

    def setTransform(f: String => CssSel): FormValidation[A,B] = this.copy(transform = Some(f))
  }

  trait FormValidationInstances {
    implicit def formValidationApplicative[X]: Applicative[({type f[a] = FormValidation[X, a]})#f] =
      new Applicative[({type f[a] = FormValidation[X, a]})#f] {
        override def map[A, B](a: FormValidation[X,A])(f: A => B): FormValidation[X,B] = a map f

        override def point[A](a: => A) =
          FormValidation[X,A](_ => a.success, None)

        override def ap[A,B](fa: => FormValidation[X,A])(f: => FormValidation[X,A=>B]): FormValidation[X,B] =
          fa ap f
      }
  }

  object FormValidation extends FormValidationInstances

  case class FormError(
    render: NodeSeq,
    label: Option[String] = None
  )

  case class FormValue[A](name: Option[String], value: A)

  case class FormState (private [formlet] values: Map[String,Any], private [formlet] context: List[String]) {
    def pushContext(s: String): FormState = this.copy(context = s :: context)
    def popContext: FormState = this.copy(context = context.tail)

    def contextName(name: String): String = (name :: context).reverse.mkString("_")
  }

  object FormState {
    def apply(): FormState = FormState(Map[String,Any](), Nil)
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

    def runEmpty: (FormState, BoundForm[A]) = run(emptyEnv)

    def evalEmpty: BoundForm[A] = runEmpty._2

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

    def context(c: String): Form[A] = {
      Form(env =>
        for {
          _ <- modify[FormState](_.pushContext(c))
          aa <- this.runForm(env)
          _ <- modify[FormState](_.popContext)
        } yield aa)
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

    def mapV[B](f: FormValidation[A,B]): Form[B] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield {
          BoundForm(
            aa.result map f.validation,
            aa.name,
            aa.baseSelector,
            errTransform(aa, f))
        }).liftV

    private def errTransform[A,B,C](aa: BoundForm[A], f: FormValidation[B,C]): CssSel = {
      val e =
        for {
          sel <- aa.baseSelector
          t <- f.transform
        } yield aa.transform & t(sel)
      e getOrElse aa.transform
    }

    def mapStringV[B](f: A => Validation[String,B]): Form[B] =
      this.mapV(FormValidation(a => FormHelpers.liftStringV(f(a)), None))

    def ??(fs: FormValidation[A,A]*): Form[A] =
      this.mapV(fs.toList.sequenceU.flatten(identity))

    private def mapResult[B](f: BoundForm[A] => BoundForm[B]): Form[B] =
      Form(env =>
        for {
          aa <- this.runForm(env)
        } yield f(aa))

    def name(s: String): Form[A] = mapResult(_.copy(name = s.some))
    def baseSelector(s: String): Form[A] = mapResult(_.copy(baseSelector = s.some))

    private def foldV(a: ValidationNelE[ValidationNelE[A]]): ValidationNelE[A] =
      a.fold(_.failure[A], identity)

    def val2[B](b: Form[B])(fs: FormValidation[(FormValue[B], A), A]*): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          aa <- this.runForm(env)
        } yield {
          val all = fs.toList.sequenceU.flatten(_._2)
          if (List(bb, aa).exists(_.result.isFailure))
            aa.copy(transform = errTransform(aa, all))
          else {
            val result = foldV(^(bb.result, aa.result)((b, a) =>
              all.validation(FormValue(bb.name, b), a)))
            BoundForm(result, aa.name, aa.baseSelector, errTransform(aa, all))
          }
        })

    def val3[B,C](
      b: Form[B], c: Form[C]
    )(
      fs: FormValidation[(FormValue[B], FormValue[C], A), A]*
    ): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          cc <- c.runForm(env)
          aa <- this.runForm(env)
        } yield {
          val all = fs.toList.sequenceU.flatten(_._3)
          if (List(bb, cc, aa).exists(_.result.isFailure))
            aa.copy(transform = errTransform(aa, all))
          else {
            val result = foldV(^^(bb.result, cc.result, aa.result)((b, c, a) =>
              all.validation(FormValue(bb.name, b), FormValue(cc.name, c), a)))
            BoundForm(result, aa.name, aa.baseSelector, errTransform(aa, all))
          }
        })

    def val4[B,C,D](
      b: Form[B], c: Form[C], d: Form[D]
    )(
      fs: FormValidation[(FormValue[B], FormValue[C], FormValue[D], A), A]*
    ): Form[A] =
      Form(env =>
        for {
          bb <- b.runForm(env)
          cc <- c.runForm(env)
          dd <- d.runForm(env)
          aa <- this.runForm(env)
        } yield {
          val all = fs.toList.sequenceU.flatten(_._4)
          if (List(bb, cc, dd, aa).exists(_.result.isFailure))
            aa.copy(transform = errTransform(aa, all))
          else {
            val result = foldV(^^^(bb.result, cc.result, dd.result, aa.result)((b, c, d, a) =>
              all.validation(FormValue(bb.name, b), FormValue(cc.name, c), FormValue(dd.name, d), a)))
            BoundForm(result, aa.name, aa.baseSelector, errTransform(aa, all))
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
          aa.result.fold(_.failure[C], a => FormHelpers.liftNelStringV(a)),
          aa.name,
          aa.baseSelector,
          aa.transform))
  }

  trait FormInstances {
    implicit val formApplicative: Applicative[Form] = new Applicative[Form] {
      override def map[A, B](a: Form[A])(f: A => B): Form[B] = a map f

      override def point[A](a: => A): Form[A] =
        FormHelpers.liftResult(BoundForm(a.success, None, None, cssSelZero))

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
      fresult { (_, _) => BoundForm(().success, name, None, sel) }

    def fresult[A](f: (Env, FormState) => BoundForm[A]): Form[A] =
      Form(env => for (s <- get[FormState]) yield f(env, s))

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

  object FormHelpers {
    def lift2V[A,B](f: (FormValue[B], A) => ValidationNelE[A]): FormValidation[(FormValue[B],A),A] =
      FormValidation({ case (bn, a) => f(bn, a) }, None)

    def lift3V[A,B,C](
      f: (FormValue[B], FormValue[C], A) => ValidationNelE[A]
    ): FormValidation[(FormValue[B],FormValue[C],A),A] =
      FormValidation({ case (bn, cn, a) => f(bn, cn, a) }, None)

    def lift4V[A,B,C,D](
      f: (FormValue[B], FormValue[C], FormValue[D], A) => ValidationNelE[A]
    ): FormValidation[(FormValue[B],FormValue[C],FormValue[D],A),A] =
      FormValidation({ case (bn, cn, dn, a) => f(bn, cn, dn, a) }, None)

    def liftStringV[A](in: Validation[String,A]): ValidationNelE[A] =
      in.leftMap(s => FormError(Text(s))).toValidationNel

    def liftNelStringV[A](in: ValidationNelS[A]): ValidationNelE[A] =
      in.leftMap(_.map(s => FormError(Text(s))))

    def liftResult[A](form: BoundForm[A]): Form[A] =
      Form(env =>
        for (_ <- get[FormState])
        yield form)
  }
}

