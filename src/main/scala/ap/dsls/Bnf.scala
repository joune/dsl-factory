package ap.dsls

import org.parboiled.scala._

/**
 <pre>
   bnf        = { comment } | "{" { production } "}"
   production = { comment } | identifier "=" expression ( "." | ";" ) .
   expression = term { "|" term } .
   term       = factor { factor } .
   factor     = identifier
              | literal
              | ntimes
              | function
              | "[" expression "]"
              | "(" expression ")"
              | "{" expression "}" .
   identifier = character { character } .
   comment    = "#" any .
   literal    = "'" character { character } "'"
              | '"' character { character } '"' .
   ntimes     = count '*' identifier
   function   = '$' identifier [ '(' arg [ { ',' arg } ] ')' ]
   arg        = literal | count
   count      = { digit }
 </pre>
*/
object Bnf {
  sealed trait AstNode
  case class Root(heading:Comment,rules:List[Production]) extends AstNode
  case class Comment(lines:List[String]) extends AstNode
  case class Production(heading:Comment,id:Identifier,expression:Expression) extends AstNode
  case class Expression(terms:List[Term]) extends AstNode
  case class Term(factors:List[Factor]) extends AstNode
  sealed trait Factor extends AstNode
  case class Identifier(s:String) extends Factor
  case class Literal(s:String) extends Factor
  case class Optional(expression:Expression) extends Factor
  case class Ntimes(n:Int, id:Identifier) extends Factor
  case class Repeat(expression:Expression) extends Factor
  case class Group(expression:Expression) extends Factor
  case class Function(name:String,args:Option[List[Argument]]) extends Factor
  sealed trait Argument
  case class StringArgument(s:String) extends Argument
  case class IntArgument(i:Int) extends Argument
}

class Bnf extends Parser 
{
  import Bnf._

  override val buildParseTree = true


  def bnf = rule { zeroOrMore(comment) ~ "{" ~ oneOrMore(production) ~ "}" ~ zeroOrMore(lineFeed) ~ EOI } ~~> { 
    (comments:List[String], prod:List[Production]) => Root(Comment(comments),prod)
  }

  def production:Rule1[Production] = rule { zeroOrMore(comment) ~ identifier ~ "=" ~ expression ~ ";" ~ lineFeed } ~~> {
    (comments:List[String], id:Identifier, exp:Expression) => 
      Production(Comment(comments), id, exp)
  }

  def expression:Rule1[Expression] = rule { oneOrMore(term, "|") } ~~> { l:List[Term] => Expression(l) }

  def term:Rule1[Term] = rule { oneOrMore(factor, anySpace) } ~~> { l => Term(l) }

  def factor:Rule1[Factor] = rule { identifier | literal | optional | repeat | ntimes | function }

  def optional:Rule1[Optional] = rule { "[" ~ expression ~ "]" } ~~> { e:Expression => Optional(e) }

  def repeat:Rule1[Repeat] = rule { "{" ~ expression ~ "}" } ~~> { e:Expression => Repeat(e) }

  def ntimes:Rule1[Ntimes] = rule { integer ~ "*" ~ identifier } ~~> { (n:Int, i:Identifier) => Ntimes(n,i) }

  def group:Rule1[Group] = rule { "(" ~ expression ~ ")" } ~~> { e:Expression => Group(e) }

  def function:Rule1[Function] = rule { "$" ~ identifier ~ optional("(" ~ oneOrMore(argument, ",") ~ ")") } ~~> { 
    (id:Identifier,args:Option[List[Argument]]) => Function(id.s,args)
  }

  def argument:Rule1[Argument] = rule {
    literal ~~> {l:Literal => StringArgument(l.s)} |
    integer ~~> {n:Int => IntArgument(n)}
  } 

  def literal:Rule1[Literal] = rule { "\"" ~ zeroOrMore(noneOf("\"\r\n")) ~ "\"" |
                                       "'" ~ zeroOrMore(noneOf("'\r\n")) ~ "'"
                                    } ~> { s =>
                                      Literal(s.substring(1,s.length-1)) //drop quotes
                                    }

  def comments:Rule1[Comment] = rule { zeroOrMore(comment, lineFeed) } ~~> { s:List[String] => Comment(s) }

  def comment:Rule1[String] = rule { "#" ~ zeroOrMore(noneOf("\r\n")) ~ lineFeed } ~> { s => s.trim.tail } //drop the "#" and "\n"

  def identifier:Rule1[Identifier] = rule { oneOrMore(alphaChar) } ~> { Identifier(_) }

  def alphaChar = rule { "0" - "9" | "a" - "z" | "A" - "Z" | "_" }

  def integer:Rule1[Int] = rule { oneOrMore("0" - "9") } ~> {_.toInt}

  def whiteSpace = rule { oneOrMore(anyOf(" \t")) }

  def anySpace = rule { oneOrMore(anyOf(" \n\r\t\f")) }

  def lineFeed = rule { oneOrMore(anyOf("\n\r")) }
}

/** now we can generate scala case classes from the grammar description */
object ScalaSerializer
{
  import Bnf._

  sealed trait Node2Scala { def toScala:String }
  implicit class Root2Scala(root:Root) extends Node2Scala
  {
    //if we have alternative terms, we generate a different case class for each alternative
    def toScala = (root.heading.toScala :: root.rules.map(_.toScala)).mkString("\n")
  }
  implicit class Comment2Scala(c:Comment) extends Node2Scala
  {
    def toScala = s"""
            |/**
            |${c.lines.mkString("\n * ")}
            |*/
            |""".stripMargin
  }
  implicit class Production2Scala(r:Production) extends Node2Scala
  {
    def toScala = r.expression.terms match {
      case term::Nil =>
        val args = term.factors.map(_.toScalaArg).mkString(", ")
        s"${r.heading.toScala}\ncase class ${r.id}($args)"

      case _ => ??? //zipWithIndex and generate one class per alternative (?)
    }
  }

  sealed trait Factor2Scala { def toScalaArg:String }
  implicit class FixmeFactor2Scala(f:Factor) extends Factor2Scala
  {
    def toScalaArg:String = ??? //FIXME, make one for each case
  }
}


/** and we can generate a new parboiled parser matching the grammar description */
object ParserGenerator
{
  import Bnf._

  sealed trait Node2Parser { def toParser:String }
  implicit class Root2Parser(r:Root) extends Node2Parser
  {
    def toParser = {
      val rules = r.rules.map(_.toParser).mkString("\n")
      s"""package FIXME
      |import org.parboiled.scala._
      |class FIXME extends Parser {
      |$rules
      |}""".stripMargin
    }
  }
  implicit class Production2Parser(r:Production) extends Node2Parser
  {
    def toParser = s"def ${r.id} = rule { ${r.expression.toParser} }"
  }
  implicit class Expression2Parser(e:Expression) extends Node2Parser
  {
    def toParser = e.terms.map(_.toParser).mkString(" | ")
  }
  implicit class Term2Parser(t:Term) extends Node2Parser
  {
    def toParser = t.factors.map(_.toParser).mkString(" ~ ")
  }
  implicit class Factor2Parser(f:Factor) extends Node2Parser
  {
    def toParser = f match {
      case Identifier(i) => i
      case Literal(s)    =>s""""$s""""
      case Optional(e)   => s"optional(${e.toParser})"
      case Ntimes(n,id)  => s"nTimes($n, $id)"
      case Repeat(e)     => s"oneOrMore(${e.toParser})"
      case Group(e)      => s"(${e.toParser})"
      case Function(id,params) =>
        // these are predef functions of the DSL
        params match {
        // if no param, just return the function name, it's defined elsewhere
        case None => id
        // if params, generate a rule with the given param values
        case Some(args) => ??? //FIXME
      }
    }
  }
}

