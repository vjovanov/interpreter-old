package scala.reflect.interpreter
package internal

trait Definitions {
  self: Engine =>

  import u._
  import definitions._

  lazy val Any_isInstanceOf = AnyClass.info.decl(TermName("isInstanceOf"))
  lazy val Any_equals = AnyClass.info.decl(TermName("equals"))
  lazy val Object_eq = ObjectClass.info.decl(TermName("eq"))
  lazy val Option_isDefined = OptionClass.info.decl(TermName("isDefined"))
  lazy val Option_get = OptionClass.info.decl(TermName("get"))

  private def method[T1: TypeTag, T2: TypeTag](x1: T1, name: String, x2: T2): Symbol = {
    val (t1, t2) = (typeOf[T1].companion, typeOf[T2].companion)
    val overloaded = t1.member(TermName(name).encodedName).alternatives
    val candidate = overloaded.find(_.info.paramLists.head.head.info == t2)
    candidate.getOrElse(throw new Exception("$t1.$name($t2) not found"))
  }

  lazy val INT_PLUS_FLOAT = method(Int, "+", Float)
  lazy val INT_PLUS_INT = method(Int, "+", Int)
  lazy val INT_MINUS_INT = method(Int, "-", Int)
  lazy val INT_LESS_INT = method(Int, "<", Int)
  lazy val INT_GT_INT = method(Int, ">", Int)
  lazy val INT_EQEQ_INT = method(Int, "==", Int)
}