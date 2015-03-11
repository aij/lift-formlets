package code.snippet

import xml._

/** A quick hack to style `Msgs` using Foundation alert boxes */
object FoundationMsgs {
  def render: NodeSeq =
    <div data-lift="Msgs">
      <notice_class>alert-box info</notice_class>
      <warning_class>alert-box warning</warning_class>
      <error_class>alert-box alert</error_class>
    </div>
}
