import scala.language.experimental.macros

package object debox {
  def cfor[A](init: A)(test: A => Boolean, next: A => A)(body: A => Unit): Unit =
    macro spire.macros.Syntax.cforMacro[A]
}
