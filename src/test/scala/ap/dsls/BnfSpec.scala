package ap.dsls

import org.scalatest.{FlatSpec, Matchers, BeforeAndAfterAll}

import io.Source

import org.parboiled.scala.{ReportingParseRunner, ParsingResult}
//import org.parboiled.scala.testing.ParboiledTest

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class BnfSpec extends FlatSpec with Matchers
{
  //required by ParboiledTest
  //def fail(msg: String) = throw new Exception(msg)

  import Bnf._

  val parser = new Bnf

  implicit class ResultMatcher(r:ParsingResult[_]) {
    def ok = {
      assert(r.matched, r.parseErrors.map(e => s"${e.getStartIndex}:${e.getErrorMessage}"))
      r
    }
  }

  "BnfParser" should "parse an LineFeed" in {
    ReportingParseRunner(parser.lineFeed).run("\n\r\n").ok
  }

  it  should "parse an identifier" in {
    val parsed = ReportingParseRunner(parser.identifier).run("id_Toto").ok
    parsed.result.get shouldEqual Identifier("id_Toto")
  }

  it should "parse a integer" in {
    val parsed = ReportingParseRunner(parser.integer).run("123456890").ok
    parsed.result.get shouldEqual 123456890
  }

  it should "parse a literal" in {
    val s = " bla bla 'youpi' #$i"
    val parsed = ReportingParseRunner(parser.literal).run(s""""$s"""").ok
    parsed.result.get shouldEqual Literal(s)
  }

  it should "parse a comment" in {
    val s = " bla bla 'you_pi\" #$i"
    val parsed = ReportingParseRunner(parser.comment).run(s"#$s\n").ok
    parsed.result.get shouldEqual s
  }

  it should "parse ntimes syntax" in {
    val parsed = ReportingParseRunner(parser.ntimes).run("5*toto").ok
    parsed.result.get shouldEqual Ntimes(5,Identifier("toto"))
  }

  it should "parse a function0" in {
    val parsed = ReportingParseRunner(parser.function).run("$fun").ok
    parsed.result.get shouldEqual Function(Identifier("fun"),None)
  }

  it should "parse a function(string)" in {
    val parsed = ReportingParseRunner(parser.function).run("$fun('arg')").ok
    parsed.result.get shouldEqual Function(Identifier("fun"),Some(List(StringArgument("arg"))))
  }

  it should "parse a function(int,int)" in {
    val parsed = ReportingParseRunner(parser.function).run("$fun(12,34)").ok
    parsed.result.get shouldEqual Function(Identifier("fun"),Some(List(IntArgument(12),IntArgument(34))))
  }

  it should "parse an identifier expression" in {
    val parsed = ReportingParseRunner(parser.expression).run("id").ok
    parsed.result.get shouldEqual Expression(List(Term(List(Identifier("id")))))
  }

  it should "parse alternative expressions" in {
    val parsed = ReportingParseRunner(parser.expression).run("id1|id2").ok
    parsed.result.get shouldEqual Expression(List(
      Term(List(Identifier("id1"))),
      Term(List(Identifier("id2")))
    ))
  }

  it should "parse a literal expression" in {
    val parsed = ReportingParseRunner(parser.expression).run("'#'").ok
    parsed.result.get shouldEqual Expression(List(Term(List(Literal("#")))))
  }

  it should "parse an optional identifier" in {
    val parsed = ReportingParseRunner(parser.optional).run("[id]").ok
    parsed.result.get shouldEqual Optional(Expression(List(Term(List(Identifier("id"))))))
  }

  it should "parse a production rule" in {
    val parsed = ReportingParseRunner(parser.production).run("#comment\nroot=id;\n").ok
    parsed.result.get shouldEqual Production(
      Comment(List("comment")),
      Identifier("root"),
      Expression(List(Term(List(Identifier("id")))))
    )
  }

  it should "parse a grammar" in {
    val parsed = ReportingParseRunner(parser.bnf).run(
      """#title 1
        |#title 2
        |{root=header body;
        |header='#head';
        |#body comment
        |body='hello' 'world';
        |}""".stripMargin).ok
    parsed.result.get shouldEqual Root(
      Comment(List("title 1", "title 2")),
      List(
        Production(Comment(Nil),Identifier("root"),Expression(
          List(Term(List(Identifier("header"),Identifier("body")))))),
        Production(Comment(Nil),Identifier("header"),Expression(
          List(Term(List(Literal("#head")))))),
        Production(Comment(List("body comment")),
          Identifier("body"), Expression(
            List(Term(List(
              Literal("hello"),Literal("world"))))))
      ))
  }

  it should "parse a grammar file " in {
    ReportingParseRunner(parser.bnf).run(Source.fromFile("src/test/resources/sample.grammar").mkString).ok 
  }

}
