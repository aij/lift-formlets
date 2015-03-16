# lift-formlets

lift-formlets is a functionally-pure, form-handling library for the Lift Web
application framework. It is based around the idea, as described in this
[paper](http://groups.inf.ed.ac.uk/links/formlets/), that with the right choice
of abstractions, Web forms can and should be easily and infinitely composable.

lift-formlets makes heavy use of [Scalaz](https://github.com/scalaz/scalaz)'s
`Validation` and `Applicative`, but it is not necessary to understand the
details of how they work to successfully validate and compose forms. For those
interested however, [Functional Programming in
Scala](http://manning.com/bjarnason/) cannot be more highly recommended.

## Usage

See [Forms.scala](https://github.com/pbrant/lift-formlets/blob/master/example/src/main/scala/code/snippet/Forms.scala)
and the corresponding [template files](https://github.com/pbrant/lift-formlets/tree/master/example/src/main/webapp) for
a heavily commented example.

## Acknowledgements

* [Formality](https://github.com/hacklanta/lift-formality) is another form
  building library for Lift. It has a tasteful, well-documented API that
integrates more tightly with the standard Lift way of doing things than does
lift-formlets. Formality and lift-formlets share the common goal of leveraging
Lift's CSS selector transforms as much as possible to bind the dynamic portions
of a form to an existing template. Parts of the template binding code in
lift-formlets are based on Formality.

* [Formaggio](https://github.com/wrwills/formaggio) is a now-abandoned
  implementation of Formlets in Scala. lift-formlets doesn't share any code
with Formaggio and the details are ultimately very different, but it was still
incredibly helpful to find another Formlet library that also used Scala and
Scalaz.

## License

lift-formlets is provided under the terms of the MIT License. No warranties
are made, express or implied.  See the `LICENSE` file in this same directory.

# Author/Contributors

lift-formlets is copyright [Wisconsin Court System](http://wicourts.gov/), and was
originally written by [Peter Brant](http://github.com/pbrant).

