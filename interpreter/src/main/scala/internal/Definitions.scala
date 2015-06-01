package scala.reflect.interpreter
package internal

trait Definitions {
  self: Engine =>

  import u._
  import definitions._

  lazy val Any_isInstanceOf = AnyClass.info.decl(TermName("isInstanceOf"))
  lazy val Any_equals = AnyClass.info.decl(TermName("equals"))
  lazy val Any_hashCode = AnyClass.info.decl(TermName("hashCode"))
  lazy val Object_eq = ObjectClass.info.decl(TermName("eq"))
  lazy val Object_hashcode = ObjectClass.info.decl(TermName("hashCode"))
  lazy val Object_init = ObjectClass.info.decl(termNames.CONSTRUCTOR)
  lazy val Option_isDefined = OptionClass.info.decl(TermName("isDefined"))
  lazy val Option_get = OptionClass.info.decl(TermName("get"))

  private def method1[T1: TypeTag, T2: TypeTag](x1: T1, name: String, x2: T2): Symbol = {
    val (t1, t2) = (typeOf[T1].companion, typeOf[T2].companion)
    val overloaded = t1.member(TermName(name).encodedName).alternatives
    val candidate = overloaded.find(_.info.paramLists.head.head.info == t2)
    candidate.getOrElse(throw new Exception(s"$t1.$name($t2) not found"))
  }

  private def method0(x1: Type, name: String): Symbol = {
    val candidate = x1.member(TermName(name)).alternatives.find(_.typeSignature.paramLists.head.isEmpty)
    candidate.getOrElse(throw new Exception(s"$x1.$name() not found"))
  }
  lazy val INT_PLUS_FLOAT = method1(Int, "+", Float)
  lazy val INT_TIMES_DOUBLE = method1(Int, "*", Double)
  lazy val INT_TIMES_LONG = method1(Int, "*", Long)
  lazy val INT_PLUS_INT = method1(Int, "+", Int)
  lazy val INT_MINUS_INT = method1(Int, "-", Int)
  lazy val INT_LESS_INT = method1(Int, "<", Int)
  lazy val INT_GT_INT = method1(Int, ">", Int)
  lazy val INT_EQEQ_INT = method1(Int, "==", Int)
  lazy val LONG_EQEQ_LONG = method1(Long, "==", Long)
  lazy val LONG_EQEQ_INT  = method1(Long, "==", Int)
  lazy val LONG_PLUS_LONG = method1(Long, "+", Long)
  lazy val LONG_MINUS_LONG = method1(Long, "-", Long)
  lazy val LONG_MINUS_INT = method1(Long, "-", Int)
  lazy val DOUBLE_PLUS_DOUBLE = method1(Double, "+", Double)
  lazy val DOUBLE_TIMES_DOUBLE = method1(Double, "*", Double)
  lazy val DOUBLE_MINUS_DOUBLE = method1(Double, "-", Double)
  lazy val DOUBLE_DIV_INT = method1(Double, "/", Int)
  lazy val ANYREF_EQ_ANYREF = typeOf[AnyRef].members.find(x => x.name.toString == "eq").get

  lazy val Throwable_init = method0(typeOf[Throwable], "<init>")
}
