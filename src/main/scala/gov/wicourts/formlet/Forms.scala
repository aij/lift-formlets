package gov.wicourts.formlet

import net.liftweb.http.{S, FileParamHolder}
import net.liftweb.util.{A => _, _}
import net.liftweb.util.Helpers.{^ => _, _}

import xml._

import scalaz._
import Scalaz._

/** Represents a form's input */
trait Env {
  /** Returns the value of a given parameter */
  def param(name: String): List[String]

  /** Returns an uploaded file with the name of the provided parameter */
  def file(name: String): Option[FileParamHolder]
}

object Env {
  /** Returns an [[Env]] implementation that uses Lift's request handling framework */
  def paramsEnv: Env = new Env {
    def param(s: String) = S.params(s).map(_.trim).filterNot(_.isEmpty)
    def file(s: String) = S.request.flatMap(_.uploadedFiles.find(_.name == s))
  }

  /** Returns an [[Env]] that always returns nothing */
  def emptyEnv: Env = new Env {
    def param(s: String) = Nil
    def file(s: String) = None
  }

  /** Creates a testing environment using the provided map */
  def singleEnv(m: Map[String, String]): Env = new Env {
    def param(s: String) = m.get(s).toList
    def file(s: String) = None
  }

  /** Creates a testing environment using the provided map */
  def multiEnv(m: Map[String, List[String]]): Env = new Env {
    def param(s: String) = m.getOrElse(s, Nil)
    def file(s: String) = None
  }

  private [formlet] def single(env: Env, name: String): Option[String] = env.param(name).headOption
}

/**
  * @param validation The function that performs the validation
  * @param transform An optional transform to apply to the form
  *
  * @tparam A The input type
  * @tparam B The output type
  */
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

  /**
    * Flattens a validation returning a `List[C]` to a validation returning
    * a `C`.
    */
  def flatten[C](f: A => C)(implicit ev: B <:< List[C]): FormValidation[A,C] =
    new FormValidation(a => this.validation(a).map(_ => f(a)), this.transform)

  /** Returns a copy of this [[FormValidation]] with the transform set to the provided function */
  def setTransform(f: String => CssSel): FormValidation[A,B] = this.copy(transform = Some(f))

  /** Lifts this `FormValidation` to one that takes two parameters */
  def lift1To2V[C]: FormValidation[(FormValue[C], A),B] =
    FormValidation({ case (_, a) => this.validation(a) }, this.transform)

  /** Lifts this `FormValidation` to one that takes three parameters */
  def lift1To3V[C,D]: FormValidation[(FormValue[C], FormValue[D], A),B] =
    FormValidation({ case (_, _, a) => this.validation(a) }, this.transform)

  /** Lifts this `FormValidation` to one that takes four parameters */
  def lift1To4V[C,D,E]: FormValidation[(FormValue[C], FormValue[D], FormValue[E], A),B] =
    FormValidation({ case (_, _, _, a) => this.validation(a) }, this.transform)
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

/**
  * @param error The error to display
  * @param label An optional label (for example, the label of a form field)
  */
case class FormError(
  error: NodeSeq,
  label: Option[String] = None
)

/**
  * A named wrapper around a value of type `A` which is used to provide
  * the current value of dependent form value when validating forms that
  * depend on other forms.
  */
case class FormValue[A](label: Option[String], value: A)

case class FormState (
  private [formlet] values: Map[String,Any],
  private [formlet] context: List[String],
  renderErrors_? : Boolean) {
  def pushContext(s: String): FormState = this.copy(context = s :: context)
  def popContext: FormState = this.copy(context = context.tail)

  def contextName(name: String): String = (name :: context).reverse.mkString("_")
}

object FormState {
  def apply(renderErrors: Boolean): FormState =
    FormState(Map[String,Any](), Nil, renderErrors)
}

/**
  * The result of running a form
  *
  * @param result The form's value
  * @param label An optional label (for example, a form field's label)
  * @param baseSelector An optional CSS selector that can be used
  * to make further modifications to the form
  * @param transform The CSS selector transform to apply to the input template
  */
case class BoundForm[A](
  result: ValidationNelE[A],
  label: Option[String],
  baseSelector: Option[String],
  transform: CssSel
) {
  /** Returns a list of error values or `Nil` if there are no error */
  def errors: List[NodeSeq] = result.fold(_.map(_.error).toList, _ => Nil)
}

/**
  * All forms are instance of this class
  *
  * @tparam A The type of the form's value
  */
case class Form[A](runForm: Env => State[FormState,BoundForm[A]]) {
  import Form._

  /**
    * Runs this form using the provided environment and initial state (defaulting to
    * an empty state).
    */
  def run(env: Env, initialState: FormState = FormState(true)): (FormState, BoundForm[A]) =
    runForm(env).run(initialState)

  /** Runs this form with an empty environment and state */
  def runEmpty: (FormState, BoundForm[A]) = run(Env.emptyEnv)

  /**
    * Runs this form with an empty environment and returns the result,
    * discarding any state.
    */
  def evalEmpty: BoundForm[A] = runEmpty._2

  /**
    * Returns a form that runs this form and saves the result to the current
    * `FormState`. Running the resulting form again will return the same result
    * as long as the same `FormState` is used.
    */
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

  /**
    * Returns a form that runs this form in the provided context. This is used
    * to scope form field input names.
    */
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
        Form.combineLabels(aa, ff),
        Form.combineBaseSelectors(aa, ff),
        aa.transform & ff.transform))

  /**
    * Returns a form that maps the form's result through the provided
    * form validation. The returned form will apply the form validation's
    * transform to this form's result.
    */
  def mapV[B](f: FormValidation[A,B]): Form[B] =
    Form(env =>
      for {
        aa <- this.runForm(env)
      } yield {
        BoundForm(
          aa.result map f.validation,
          aa.label,
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

  /**
    * A convenience method that maps this form through a string-based
    * validation.
    */
  def mapStringV[B](f: A => Validation[String,B]): Form[B] =
    this.mapV(FormValidation(a => FormHelpers.liftStringV(f(a)), None))

  /** Validates this form using the provided validations */
  def ??(fs: FormValidation[A,A]*): Form[A] =
    this.mapV(fs.toList.sequenceU.flatten(identity))

  private def mapResult[B](f: BoundForm[A] => BoundForm[B]): Form[B] =
    Form(env =>
      for {
        aa <- this.runForm(env)
      } yield f(aa))

  /** Modifies the result of this form to have the label provided */
  def label(s: String): Form[A] = mapResult(_.copy(label = s.some))

  /** Modifies the result of this form to have the base selector provided */
  def baseSelector(s: String): Form[A] = mapResult(_.copy(baseSelector = s.some))

  private def foldV(a: ValidationNelE[ValidationNelE[A]]): ValidationNelE[A] =
    a.fold(_.failure[A], identity)

  /** Validates this form using the result of another form as input. */
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
            all.validation(FormValue(bb.label, b), a)))
          BoundForm(result, aa.label, aa.baseSelector, errTransform(aa, all))
        }
      })

  /** Validates this form using the result of two other forms as input. */
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
            all.validation(FormValue(bb.label, b), FormValue(cc.label, c), a)))
          BoundForm(result, aa.label, aa.baseSelector, errTransform(aa, all))
        }
      })

  /** Validates this form using the result of three other forms as input. */
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
            all.validation(FormValue(bb.label, b), FormValue(cc.label, c), FormValue(dd.label, d), a)))
            BoundForm(result, aa.label, aa.baseSelector, errTransform(aa, all))
          }
        })

  /**
    * Lifts a form returning a validation into a form that returns a value of
    * the type of validation
    */
  def liftV[C](implicit ev: A <:< ValidationNelE[C]): Form[C] =
    Form(env =>
      for {
        aa <- this.runForm(env)
      } yield BoundForm(
        aa.result.fold(_.failure[C], a => a: ValidationNelE[C]),
        aa.label,
        aa.baseSelector,
        aa.transform))

  /**
    * Lifts a form returning a string-based validation into a form that
    * returns a value of the type of validation
    */
  def liftStringV[C](implicit ev: A <:< ValidationNelS[C]): Form[C] =
    Form(env =>
      for {
        aa <- this.runForm(env)
      } yield BoundForm(
        aa.result.fold(_.failure[C], a => FormHelpers.liftNelStringV(a)),
        aa.label,
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

  def combineLabels[A,B](a: BoundForm[A], b: BoundForm[B]): Option[String] = {
    Some(List(a.label, b.label).flatten.mkString(" and ")).filterNot(_.isEmpty)
  }

  def combineBaseSelectors[A,B](a: BoundForm[A], b: BoundForm[B]): Option[String] = {
    (a.baseSelector, b.baseSelector) match {
      case (None, None) | (Some(_), Some(_)) => None
      case (None, s@Some(_)) => s
      case (s@Some(_), None) => s
    }
  }

  /** Returns a form that always fails with the provided message */
  def failing[A](message: String): Form[A] = {
    F.point(message.failure.toValidationNel).liftStringV
  }

  /** Returns a form that applies the provided selector and label */
  def sel(sel: CssSel, label: Option[String] = None): Form[Unit] =
    fresult { (_, _) => BoundForm(().success, label, None, sel) }

  /** A convenience method for defining a form using a function */
  def fresult[A](f: (Env, FormState) => BoundForm[A]): Form[A] =
    Form(env => for (s <- get[FormState]) yield f(env, s))

  /** Returns a copy of the provided validation with the label set to a new value */
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

object FormHelpers {
  /** Lifts a function of two parameters into the equivalent [[FormValidation]] */
  def lift2V[A,B](f: (FormValue[B], A) => ValidationNelE[A]): FormValidation[(FormValue[B],A),A] =
    FormValidation({ case (bn, a) => f(bn, a) }, None)

  /** Lifts a function of three parameters into the equivalent [[FormValidation]] */
  def lift3V[A,B,C](
    f: (FormValue[B], FormValue[C], A) => ValidationNelE[A]
  ): FormValidation[(FormValue[B],FormValue[C],A),A] =
    FormValidation({ case (bn, cn, a) => f(bn, cn, a) }, None)

  /** Lifts a function of four parameters into the equivalent [[FormValidation]] */
  def lift4V[A,B,C,D](
    f: (FormValue[B], FormValue[C], FormValue[D], A) => ValidationNelE[A]
  ): FormValidation[(FormValue[B],FormValue[C],FormValue[D],A),A] =
    FormValidation({ case (bn, cn, dn, a) => f(bn, cn, dn, a) }, None)

  /** Lifts a string-based validation into the validation used by a form's result */
  def liftStringV[A](in: Validation[String,A]): ValidationNelE[A] =
    in.leftMap(s => FormError(Text(s))).toValidationNel

  /** Lifts a list of string-based validations into the validation used by a form's result */
  def liftNelStringV[A](in: ValidationNelS[A]): ValidationNelE[A] =
    in.leftMap(_.map(s => FormError(Text(s))))

  /** Lifts a form's result into a form that always returns that result when run */
  def liftResult[A](form: BoundForm[A]): Form[A] =
    Form(env =>
      for (_ <- get[FormState])
      yield form)

  /** Lifts a `FormValidation` with two parameters to one of three */
  def lift2To3V[A,B,C](in: FormValidation[(FormValue[B],A),A]): FormValidation[(FormValue[B],FormValue[C],A),A] =
    FormValidation({ case (b, _, a) => in.validation((b, a)) }, in.transform)

  /** Lifts a `FormValidation` with two parameters to one of four */
  def lift2To4V[A,B,C,D](in: FormValidation[(FormValue[B],A),A]): FormValidation[(FormValue[B],FormValue[C],FormValue[D],A),A] =
    FormValidation({ case (b, _, _, a) => in.validation((b, a)) }, in.transform)

  /** Lifts a `FormValidation` with three parameters to one of four */
  def lift3To4V[A,B,C,D](in: FormValidation[(FormValue[B],FormValue[C],A),A]): FormValidation[(FormValue[B],FormValue[C],FormValue[D],A),A] =
    FormValidation({ case (b, c, _, a) => in.validation((b, c, a)) }, in.transform)
}
