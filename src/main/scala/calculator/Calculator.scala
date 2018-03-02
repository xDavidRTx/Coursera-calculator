package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val names: List[String] = namedExpressions.keySet.toList
    val e: List[Signal[Double]] =for (x <- names)
     yield Signal(eval(namedExpressions(x)(), namedExpressions))
    (names zip e).toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(l) => l
      case Plus(a,b) => eval(a , references) + eval(b , references)
      case Divide(a,b) => eval(a , references) / eval(b , references)
      case Minus(a, b) => eval(a , references) - eval(b , references)
      case Times(a,b) =>  eval(a , references) * eval(b , references)
      case Ref(a) => eval(getReferenceExpr(a, references), references.filterKeys(_!=a))
    }
  }




  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
