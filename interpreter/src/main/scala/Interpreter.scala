package scala.reflect.interpreter

// TODO: for now we work with scala-reflect.jar
// later on when reflection core is ready, we'll switch to it
import scala.reflect.macros.Universe
import scala.reflect.macros.blackbox.Context
import internal.Engine
import internal.Emulators

trait InterpreterRequires {
  val u: Universe
  def source(sym: u.Symbol): u.MemberDef
  implicit class RichSymbol(sym: u.Symbol) {
    def source = InterpreterRequires.this.source(sym)
  }
}

trait InterpreterProvides {
  self: InterpreterRequires =>
  def eval(tree: u.Tree): Any
}

final class TreeValue(value: Any)

object interpret {

  def apply(c: Context)(tree: c.Tree): Any = {
    withDefs(c)(Nil)(tree)
  }

  /*
   * This version allows one to pass trees of definitions together with the program.
   */
  def withDefs(c: Context)(defs: Seq[c.Tree])(tree: c.Tree): Any = {
    import c.universe._
    object SourceExtractor {
      def unapply(tree: Tree): Option[(Symbol, MemberDef)] = tree match {
        case src: MemberDef  => Some((src.symbol, src))
        case src: ModuleDef  => Some((src.symbol, src))
        case _ => None
      }
    }
    val engine = new {
      val u: c.universe.type = c.universe
    } with Engine with Emulators with InterpreterRequires with InterpreterProvides{
      def source(sym: Symbol): MemberDef = {
        (defs :+ tree).flatMap(_.collect{
          case SourceExtractor(`sym`, src) => src
        }).headOption.getOrElse(UnobtainableSource(sym))
      }
    }
    engine.eval(tree)
  }
}
