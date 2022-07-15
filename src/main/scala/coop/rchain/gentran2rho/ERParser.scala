package coop.rchain.gentran2rho

import coop.rchain.gtrn2rho.ast.extended_rules.Absyn.ProgramType
import coop.rchain.gtrn2rho.ast.extended_rules.{parser, Yylex}

import java.io.Reader

object ERParser {
  private def lexer(fileReader: Reader): Yylex = new Yylex(fileReader)
  private def parser(lexer: Yylex): ErrorHandlingParser =
    new ErrorHandlingParser(lexer, lexer.getSymbolFactory)

  /**
    * Signal errors to the caller rather than printing them to System.err.
    *
    * Please excuse the use of throw; we didn't design the CUP API.
    *
    * Ref Section 4. Customizing the Parser in
    * CUP User's Manual Last updated 06/2014 (v0.11b)
    * http://www2.cs.tum.edu/projects/cup/docs.php#parser
    */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  class ErrorHandlingParser(s: Yylex, sf: java_cup.runtime.SymbolFactory) extends parser(s, sf) {
    import java_cup.runtime.ComplexSymbolFactory.ComplexSymbol
    import java_cup.runtime.Symbol

    override def unrecovered_syntax_error(cur_token: Symbol): Unit =
      throw SyntaxError(
        cur_token match {
          case cs: ComplexSymbol =>
            s"syntax error(${cs.getName}): ${s
              .yytext()} at ${cs.getLeft.getLine}:${cs.getLeft.getColumn}-${cs.getRight.getLine}:${cs.getRight.getColumn}"
          case _ => cur_token.toString()
        }
      )

    /**
      *  "This method is called by the parser as soon as a syntax error
      *  is detected (but before error recovery is attempted). In the
      *  default implementation it calls: `report_error("Syntax error",
      *  null);`." -- section 4.
      *
      * The Rholang grammar has no error recovery productions, so this is
      * always immediately followed by a call to
      * `unrecovered_syntax_error`.
      */
    override def syntax_error(cur_token: Symbol): Unit = ()

    /** always followed by report_fatal_error, so noop is appropriate
      */
    override def report_error(message: String, info: Object): Unit = ()

    override def report_fatal_error(message: String, info: Object): Unit =
      throw ParserError(message + info)
  }

  sealed abstract class InterpreterError(message: String) extends Throwable(message)
  case class ParserError(message: String)                 extends InterpreterError(message)
  case class SyntaxError(message: String)                 extends InterpreterError(message)
  case class LexerError(message: String)                  extends InterpreterError(message)

  def sourceToAST(reader: Reader): ProgramType = {
    val l = lexer(reader)
    val p = parser(l)
    p.pProgramType()
  }
}
