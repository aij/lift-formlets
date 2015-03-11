package code.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util.CssSel
import net.liftweb.builtin.snippet.Menu

import xml.NodeSeq

/** A quick hack to style `Menu.builder` in a Foundation compatible way */
object FoundationMenuBuilder {
  def render(ns: NodeSeq): NodeSeq = {
    val standard = Menu.builder(ns)

    ("ul [class]" #> "side-nav") apply (standard)
  }
}
