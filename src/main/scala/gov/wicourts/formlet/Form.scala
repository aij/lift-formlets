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
  * @param binder An optional binder to apply to the form
  *
  * @tparam A The input type
  * @tparam B The output type
  */
case class FormValidation[A,B](
  validation: A => ValidationNelE[B],
  binder: Option[String => CssSel] = None
) {
  def map[C](f: B => C): FormValidation[A,C] =
    FormValidation(a => validation(a) map f, binder)

  def ap[C](f: => FormValidation[A,B => C]): FormValidation[A,C] =
    FormValidation(
      a => this.validation(a) <*> f.validation(a),
      (this.binder, f.binder) match {
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
    new FormValidation(a => this.validation(a).map(_ => f(a)), this.binder)

  /** Returns a copy of this [[FormValidation]] with the binder set to the provided function */
  def setBinder(f: String => CssSel): FormValidation[A,B] = this.copy(binder = Some(f))

  /** Lifts this `FormValidation` to one that takes two parameters */
  def lift1To2V[C]: FormValidation[(FormValue[C], A),B] =
    FormValidation({ case (_, a) => this.validation(a) }, this.binder)

  /** Lifts this `FormValidation` to one that takes three parameters */
  def lift1To3V[C,D]: FormValidation[(FormValue[C], FormValue[D], A),B] =
    FormValidation({ case (_, _, a) => this.validation(a) }, this.binder)

  /** Lifts this `FormValidation` to one that takes four parameters */
  def lift1To4V[C,D,E]: FormValidation[(FormValue[C], FormValue[D], FormValue[E], A),B] =
    FormValidation({ case (_, _, _, a) => this.validation(a) }, this.binder)
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

  private val formStateValues: FormState @> Map[String,Any] =
    Lens.lensu[FormState,Map[String,Any]](
      (a, v) => a.copy(values = v),
      _.values
    )

  private def mapCastLens[K,V](k: K): Map[K,Any] @> Option[V] =
    Lens.lensu[Map[K,Any],Option[V]](
      (a, o) => o map (a.updated(k, _)) getOrElse (a - k),
      _ get k map (_.asInstanceOf[V])
    )

  def newFormStateVar[T](key: String = nextFuncName): FormState @> Option[T] =
    formStateValues >=> mapCastLens[String,T](key)
}

case class ErrorBinder(run: (String, List[FormError]) => CssSel) {
  def apply(state: FormState, selector: String, errors: List[FormError]): CssSel = {
    if (state.renderErrors_? && errors.nonEmpty)
      run(selector, errors)
    else
      cssSelZero
  }
}

case class ErrorContext(
  selector: String,
  errorBinder: ErrorBinder
)

/**
 * @param label An optional label (for example, a form field's label)
 * @param baseSelector An optional CSS selector that can be used
 * to make further modifications to the form
 */
case class FormMetadata(
  label: Option[String],
  baseSelector: Option[String],
  errorContext: Option[ErrorContext]
) {
  private def eitherOrNone[A](a: Option[A], b: Option[A]): Option[A] =
    (a, b) match {
      case (None, None) | (Some(_), Some(_)) => None
      case (None, s@Some(_)) => s
      case (s@Some(_), None) => s
    }

  def merge(other: FormMetadata): FormMetadata = {
    FormMetadata(
      Some(List(this.label, other.label).flatten.mkString(" and ")).filterNot(_.isEmpty),
      eitherOrNone(this.baseSelector, other.baseSelector),
      eitherOrNone(this.errorContext, other.errorContext)
    )
  }

  def setErrorContext(e: ErrorContext): FormMetadata =
    this.copy(errorContext = e.some)

  def clearErrorContext: FormMetadata =
    this.copy(errorContext = none)
}

object FormMetadata {
  def empty: FormMetadata = FormMetadata(None, None, None)

  private val metadataLabel = Lens.lensu[FormMetadata,Option[String]](
    (a, value) => a.copy(label = value),
    _.label
  )

  private val metadataBaseSelector = Lens.lensu[FormMetadata,Option[String]](
    (a, value) => a.copy(baseSelector = value),
    _.baseSelector
  )

  def formLabel[A]: BoundForm[A] @> Option[String] =
    BoundForm.formMetadata[A] >=> metadataLabel

  def formBaseSelector[A]: BoundForm[A] @> Option[String] =
    BoundForm.formMetadata[A] >=> metadataBaseSelector
}

/**
  * The result of running a form
  *
  * @param result The form's value
  * @param metadata Used by other combinators to modify their behavior
  * @param binder The CSS selector binder to apply to the input template
  */
case class BoundForm[A](
  result: ValidationNelE[A],
  metadata: FormMetadata,
  binder: CssSel
) {
  /** Returns a list of error values or `Nil` if there are no error */
  def errorsNs: List[NodeSeq] = errors.map(_.error)

  def errors: List[FormError] = result.fold(_.toList, _ => Nil)
}

object BoundForm {
  def formMetadata[A]: BoundForm[A] @> FormMetadata =
    Lens.lensu[BoundForm[A],FormMetadata](
      (a, value) => a.copy(metadata = value),
      _.metadata
    )
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
    val __var = FormState.newFormStateVar[BoundForm[A]]()
    Form(env =>
      for {
        v <- __var.st
        w <- v.map(state[FormState,BoundForm[A]]).getOrElse(for {
            aa <- this.runForm(env)
            _ <- __var := aa.some
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
        aa.metadata merge ff.metadata,
        aa.binder & ff.binder))

  /**
    * Returns a form that maps the form's result through the provided
    * form validation. The returned form will apply the form validation's
    * binder to this form's result.
    */
  def mapV[B](f: FormValidation[A,B]): Form[B] =
    Form(env =>
      for {
        s <- get[FormState]
        aa <- this.runForm(env)
      } yield {
        val bb = foldV(aa.result map f.validation)
        BoundForm(
          bb,
          aa.metadata,
          mappedErrors(s, f, aa, bb))
      })

  private def mappedErrors[B,C,D](
    state: FormState,
    f: FormValidation[C,D],
    aa: BoundForm[A],
    out: ValidationNelE[B]
  ): CssSel = {
    val l = List(clientSideErrBinder(aa, f), rebindErrors(state, aa, out)).flatten
    l.fold(aa.binder)(_ & _)
  }

  private def clientSideErrBinder[B,C](
    aa: BoundForm[A],
    f: FormValidation[B,C]
  ): Option[CssSel] = {
    val r =
      for {
        sel <- aa.metadata.baseSelector
        t <- f.binder
      } yield {
        t(sel)
      }
    ^(aa.metadata.errorContext, r)(_.selector #> _) orElse r
  }

  private def rebindErrors[B](
    state: FormState,
    in: BoundForm[A],
    out: ValidationNelE[B]
  ): Option[CssSel] = {
    val errors = out.fold(_.toList, _ => Nil)
    for {
      c <- in.metadata.errorContext
        if in.result.isSuccess && state.renderErrors_? && errors.nonEmpty
    } yield c.errorBinder.apply(state, c.selector, errors)
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

  /** Transforms the result of this form */
  def mapResult[B](f: BoundForm[A] => BoundForm[B]): Form[B] =
    Form(env =>
      for {
        aa <- this.runForm(env)
      } yield f(aa))

  /** Transforms the binder of this form */
  def mapBinder(f: CssSel => CssSel): Form[A] =
    mapResult(r => r.copy(binder = f(r.binder)))

  /** Modifies the result of this form to have the label provided */
  def label(s: String): Form[A] =
    mapResult(FormMetadata.formLabel.set(_, Some(s)))

  /** Modifies the result of this form to have the base selector provided */
  def baseSelector(s: String): Form[A] =
    mapResult(FormMetadata.formBaseSelector.set(_, Some(s)))

  private def foldV[B](a: ValidationNelE[ValidationNelE[B]]): ValidationNelE[B] =
    a.fold(_.failure[B], identity)

  /** Validates this form using the result of another form as input. */
  def val2[B](b: Form[B])(fs: FormValidation[(FormValue[B], A), A]*): Form[A] =
    Form(env =>
      for {
        s <- get[FormState]
        bb <- b.runForm(env)
        aa <- this.runForm(env)
      } yield {
        val all = fs.toList.sequenceU.flatten(_._2)
        val result =
          if (List(bb, aa).exists(_.result.isFailure))
            aa.result
          else {
            foldV(^(bb.result, aa.result)((b, a) =>
              all.validation(FormValue(bb.metadata.label, b), a)))
          }
        BoundForm(result, aa.metadata, mappedErrors(s, all, aa, result))
      })

  /** Validates this form using the result of two other forms as input. */
  def val3[B,C](
    b: Form[B], c: Form[C]
  )(
    fs: FormValidation[(FormValue[B], FormValue[C], A), A]*
  ): Form[A] =
    Form(env =>
      for {
        s <- get[FormState]
        bb <- b.runForm(env)
        cc <- c.runForm(env)
        aa <- this.runForm(env)
      } yield {
        val all = fs.toList.sequenceU.flatten(_._3)
        val result =
          if (List(bb, cc, aa).exists(_.result.isFailure))
            aa.result
          else {
            foldV(^^(bb.result, cc.result, aa.result)((b, c, a) =>
              all.validation(
                FormValue(bb.metadata.label, b),
                FormValue(cc.metadata.label, c),
                a)))
          }
        BoundForm(result, aa.metadata, mappedErrors(s, all, aa, result))
      })

  /** Validates this form using the result of three other forms as input. */
  def val4[B,C,D](
    b: Form[B], c: Form[C], d: Form[D]
  )(
    fs: FormValidation[(FormValue[B], FormValue[C], FormValue[D], A), A]*
  ): Form[A] =
    Form(env =>
      for {
        s <- get[FormState]
        bb <- b.runForm(env)
        cc <- c.runForm(env)
        dd <- d.runForm(env)
        aa <- this.runForm(env)
      } yield {
        val all = fs.toList.sequenceU.flatten(_._4)
        val result =
          if (List(bb, cc, dd, aa).exists(_.result.isFailure))
            aa.result
          else {
            foldV(^^^(bb.result, cc.result, dd.result, aa.result)((b, c, d, a) =>
              all.validation(
                FormValue(bb.metadata.label, b),
                FormValue(cc.metadata.label, c),
                FormValue(dd.metadata.label, d),
                a)))
          }
        BoundForm(result, aa.metadata, mappedErrors(s, all, aa, result))
      })

  /**
    * Lifts a form returning a validation into a form that returns a value of
    * the type of validation
    */
  def liftV[C](implicit ev: A <:< ValidationNelE[C]): Form[C] =
    mapResult(x => x.copy(result = x.result.fold(_.failure[C], a => a: ValidationNelE[C])))

  /**
    * Lifts a form returning a string-based validation into a form that
    * returns a value of the type of validation
    */
  def liftStringV[C](implicit ev: A <:< ValidationNelS[C]): Form[C] =
    mapResult(x => x.copy(result = x.result.fold(_.failure[C], a => FormHelpers.liftNelStringV(a))))
}

trait FormInstances {
  implicit val formApplicative: Applicative[Form] = new Applicative[Form] {
    override def map[A, B](a: Form[A])(f: A => B): Form[B] = a map f

    override def point[A](a: => A): Form[A] =
      FormHelpers.liftResult(BoundForm(a.success, FormMetadata.empty, cssSelZero))

    override def ap[A,B](fa: => Form[A])(f: => Form[A=>B]): Form[B] =
      fa ap f
  }
}

object Form extends FormInstances {
  val F = Applicative[Form]

  /** Returns a form that always fails with the provided message */
  def failing[A](message: String): Form[A] = {
    F.point(message.failure.toValidationNel).liftStringV
  }

  /** Returns a form that applies the provided selector and label */
  def sel(sel: CssSel, label: Option[String] = None): Form[Unit] =
    fresult { (_, _) => BoundForm(().success, FormMetadata(label, None, None), sel) }

  /** A convenience method for defining a form using a function */
  def fresult[A](f: (Env, FormState) => BoundForm[A]): Form[A] =
    Form(env => for (s <- get[FormState]) yield f(env, s))

  /** Returns a copy of the provided validation with the label set to a new value */
  def setLabel[A](in: ValidationNelE[A], label: Option[String]): ValidationNelE[A] =
    in.leftMap(_.map(_.copy(label = label)))
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
    FormValidation({ case (b, _, a) => in.validation((b, a)) }, in.binder)

  /** Lifts a `FormValidation` with two parameters to one of four */
  def lift2To4V[A,B,C,D](in: FormValidation[(FormValue[B],A),A]): FormValidation[(FormValue[B],FormValue[C],FormValue[D],A),A] =
    FormValidation({ case (b, _, _, a) => in.validation((b, a)) }, in.binder)

  /** Lifts a `FormValidation` with three parameters to one of four */
  def lift3To4V[A,B,C,D](in: FormValidation[(FormValue[B],FormValue[C],A),A]): FormValidation[(FormValue[B],FormValue[C],FormValue[D],A),A] =
    FormValidation({ case (b, c, _, a) => in.validation((b, c, a)) }, in.binder)
}
