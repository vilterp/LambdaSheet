package lambdasheet

import util.parsing.combinator.syntactical.StandardTokenParsers

// TODO: char / string
// TODO: int / float?
// TODO: parser doesn't parse floats or negatives (lazy ass parser...) custom one probs necessary

abstract class Expression {
  def toCode:String
  override def toString = toCode
}
  case class IfExpression(cond:Expression, theIf:Expression,  theElse:Expression) extends Expression {
    def toCode = "if " + cond.toCode + " then " + theIf.toCode + " else " + theElse.toCode 
  }
  case class Application(funName:String, args:List[Expression]) extends Expression {
    def toCode = funName + args.map(_.toCode).mkString("(", ",", ")")
  }
  case class Lambda(params:List[String], body:Expression) extends Expression {
    def toCode = "lambda" + params.mkString("(", ",", ")") + " = " + body.toCode
  }
  abstract class Literal extends Expression
    case class StringLit(string:String) extends Literal {
      def toCode = "\"" + string.replaceAll("\"", "\\\"")
                                .replaceAll("\n", "\\\n")
                                .replaceAll("\t", "\\\t") + "\""
    }
    abstract class NumericLit extends Literal
      case class IntLit(int:Int) extends NumericLit {
        def toCode = int.toString
      }
//      case class FloatLit(float:Float) extends NumericLit {
//        def toCode = float.toString
//      }

object ExprParser extends StandardTokenParsers {

  def apply(input:String):Either[(String, String), Expression] =
    phrase(expr)(new lexical.Scanner(input).asInstanceOf[Input]) match {
      case Success(n, _) => Right(n)
      case n:NoSuccess => Left((n.msg, n.next.pos.longString))
    }

  lexical.reserved ++= Set("if", "then", "else", "lambda")
  lexical.delimiters ++= Set("(", ")", ",", "+", "-", "*", "/", "%", "==", "!=", ">", "<", ">=", "<=", "=", "&&", "||")

  def expr:Parser[Expression] = ifExpr | lambda | binOp

  def ifExpr:Parser[IfExpression] = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ { case i ~ t ~ e => IfExpression(i, t, e) }

  def application:Parser[Application] = ident ~ opt("(" ~> repsep(expr, ",") <~ ")") ^^ { case i ~ Some(args) => Application(i, args)
  case i ~ None => Application(i, Nil) }

  def lambda:Parser[Lambda] = (("lambda" ~ "(") ~> repsep (ident, ",") <~ ")") ~ ("=" ~> expr) ^^ { case params ~ body => Lambda(params, body) }

  def binOp = orExpr

  def orExpr = mkBinOp(andExpr, "||")

  def andExpr = mkBinOp(cmpOp, "&&")

  def cmpOp = mkBinOp(multOp, List(">", "<", ">=", "<=", "==", "!="))

  def multOp = mkBinOp(addOp, List("*", "/", "%"))

  def addOp = mkBinOp(atom, List("+", "-"))

  def mkBinOp(nextDown:Parser[Expression], ops:List[String]):Parser[Expression] =
    nextDown ~ opt(ops.foldLeft(success[String]("x"))({ (p, op) => keyword(op) | p }) ~ nextDown) ^^ { case e1 ~ Some(op ~ e2) => Application(op, List(e1, e2))
    case e1 ~ None => e1 }

  def mkBinOp(nextDown:Parser[Expression], op:String):Parser[Expression] = mkBinOp(nextDown, List(op))

  def atom:Parser[Expression] = application | strLit | numLit | parenExpr

  def strLit:Parser[StringLit] = stringLit ^^ { str => StringLit(str) }

  def numLit:Parser[IntLit] = numericLit ^^ { chars => IntLit(chars.toInt) }

  def parenExpr:Parser[Expression] = "(" ~> expr <~ ")"

}
