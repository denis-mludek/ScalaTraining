package Calculator

object CaseClasses {

  sealed trait ArithmeticExpression
  case class Num(a: Double) extends ArithmeticExpression
  case class Add(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression
  case class Multiplication(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression

  val unPlusDeux = Add(Num(1), Num(2))
  val unPlusDeuxPlusTrois = Add(unPlusDeux, Num(3))
  val unPlusDeuxPlusTroisMultiplieParDeux = Multiplication(unPlusDeuxPlusTrois, Num(2))

  def eval(e: ArithmeticExpression): Double = e match {
    case Num(value) => value
    case Add(a, b) => eval(a) + eval(b)
    case Multiplication(a, b) => eval(a) * eval(b)
  }

  def pretty(e: ArithmeticExpression): String =  e match {
    case Num(value) => value.toString()
    case Add(a, b) =>  s"(${pretty(a)} + ${pretty(b)})"
    case Multiplication(a, b) => s"${pretty(a)} * ${pretty(b)}"
  }

  eval(unPlusDeux)
  eval(unPlusDeuxPlusTrois)
  eval(unPlusDeuxPlusTroisMultiplieParDeux)
}

object OOP {

  trait Expr {
    def eval(): Int
    def pretty(): String
  }

  class Num(value: Int) extends Expr {
    def eval() = value
    def pretty() = value.toString
  }

  class Add(l: Expr, r: Expr) extends Expr {
    def eval() = l.eval() + r.eval()
    def pretty() = s"(${l.pretty()} + ${r.pretty()})"
  }

  class Mul(l: Expr, r: Expr) extends Expr {
    def eval() = l.eval() * r.eval()
    def pretty() = s"(${l.pretty()} * ${r.pretty()})"
  }
}


