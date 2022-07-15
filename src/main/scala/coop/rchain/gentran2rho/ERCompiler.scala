package coop.rchain.gentran2rho

import coop.rchain.gtrn2rho.ast.extended_rules.Absyn._

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

object Compiler {

  def createRhoFromAST(term: ProgramType): (String, String, String) =
    term match {
      case prog: Program =>
        val decl   = createDecls(prog.declarationlist_)
        val init   = createInit(prog.stmlist_)
        val blocks = createProcessRecordContract(prog.blocklist_)
        (decl, init, blocks)
    }

  val ident = "  "

  def createDecls(declList: DeclarationList): String = {
    val decls = declList match {
      case x: Declarations => x.listdeclaration_.toList
    }
    val strDecls = decls.map(x => procDecl(x))
    val res      = strDecls.mkString(", ")
    s"""{"LOCAL": [$res]}"""
  }

  // TODO: Add type of vars
  def procDecl(decl: Declaration): String = {
    val str = decl match {
      case x: DeclString   => procIdent(x.identliteral_)
      case x: DeclInteger  => procIdent(x.identliteral_)
      case x: DeclDateTime => procIdent(x.identliteral_)
      case _ =>
        assert(
          assertion = false,
          s"Extended Rules Error: declaration $decl doesn't supported"
        )
        ""
    }
    s""""$str""""
  }

  def createInit(stmList: StmList): String = {
    val strBody = procStatements(stmList, 1, 0)
    s"""contract init(ret0, s) = {
         #$strBody
         #}""".stripMargin('#')
  }

  def createProcessRecordContract(blockList: BlockList): String = {
    def id(level: Int)          = ident * level
    val strProcessFieldContract = createProcessFieldContract(blockList, 2)
    s"""${id(0)}contract processRecord(return, @state, @(recKey, recValues)) = {
       #${id(1)}new processField, loop in {    
       #$strProcessFieldContract |
       #${id(2)}contract loop(@keys, @s) = {
       #${id(3)}match keys {
       #${id(4)}Set(head ...tail) => {
       #${id(5)}new newState in {
       #${id(6)}for (newState <- processField!?(s, head)) { loop!(tail, *newState) }
       #${id(5)}}
       #${id(4)}}
       #${id(4)}_ => { return!(s) }
       #${id(3)}}
       #${id(2)}} |
       #${id(2)}loop!(recValues.keys(), state)
       #${id(1)}}
       #${id(0)}}""".stripMargin('#')
  }

  def createProcessFieldContract(blockList: BlockList, identLevel: Int): String = {
    def id(addLevel: Int) = ident * (identLevel + addLevel)
    def createCase(name: String): String =
      s"""${id(3)}"$name" => { for(newS <- $name!?(*s)) { ret!(*newS) } }"""

    val blocks = blockList match {
      case x: Blocks => x.listblock_.toList
    }
    val strBlocks       = blocks.map(x => createBlock(x, identLevel + 2))
    val strBlockSection = strBlocks.mkString(" |\n")
    val names           = blocks.map(getBlockName)
    val strNewNames     = names.mkString(", ")
    val strCases        = names.map(createCase).mkString("\n")

    s"""${id(0)}contract processField(ret, s, fieldName) = {
        #${id(1)}new $strNewNames in {
        #$strBlockSection |
        #${id(2)}match *fieldName {
        #$strCases
        #${id(3)}_ => ret!(*s)
        #${id(2)}}
        #${id(1)}}
        #${id(0)}}""".stripMargin('#')
  }

  def getBlockName(block: Block): String =
    block match {
      case x: BlockImpl =>
        val (_, fieldName) = parseHeader(x.header_)
        fieldName
    }

  def createBlock(block: Block, identLevel: Int): String =
    block match {
      case x: BlockImpl =>
        val (groupName, fieldName) = parseHeader(x.header_)
        val strBody                = procStatements(x.stmlist_, identLevel + 1, 0)
        // TODO: Add groupName in block name in the future
        val _  = groupName
        def id = ident * identLevel
        s"""${id}contract $fieldName(ret0, s, recName) = {
             #$strBody
             #$id}""".stripMargin('#')
    }

  def parseHeader(header: Header): (String, String) =
    header match {
      case x: HeaderImpl =>
        val str = x.headerliteral_
        val arr = str.split('.')
        assert(arr.length == 2, s"Extended Rules Error: invalid block header format: $str.")
        (procIdent(arr(0)), procIdent(arr(1)))
    }

  def procStatements(stmList: StmList, identLevel: Int, nestedLevel: Int): String = {
    val statements = stmList match {
      case x: Statements => x.liststatement_.toList
    }
    recProcStatement(statements, identLevel, nestedLevel)
  }

  def recProcStatement(stms: List[Statement], identLevel: Int, nestedLevel: Int): String = {

    def id     = ident * identLevel
    def idPlus = ident * (identLevel + 1)

    def procAssignment(strVar: String, strExpr: String): String = {
      val strBody = recProcStatement(stms.tail, identLevel + 1, nestedLevel)
      s"""${id}let s <- *s.set($strVar, $strExpr) in {
           #$strBody
           #$id}""".stripMargin('#')
    }

    def procIfExpr(ifExp: IfExp): String = {
      val expr = ifExp match {
        case x: IfExpCapital => x.expr_
        case x: IfExpSmall   => x.expr_
      }
      recProcExpr(expr)
    }

    def procBody(body: Body): String = {
      val stmList = body match {
        case x: BodyCapital => x.stmlist_
        case x: BodySmall   => x.stmlist_
      }
      val statements = stmList match {
        case x: Statements => x.liststatement_.toList
      }
      recProcStatement(statements, identLevel + 2, nestedLevel + 1)
    }

    def procElse(els: Else): String =
      els match {
        case x: ElseCapital => procBody(x.body_)
        case x: ElseSmall   => procBody(x.body_)
        case _: ElseNon     => recProcStatement(List(), identLevel + 2, nestedLevel + 1)
      }

    def procStm(stm: Statement) = stm match {
      case stmIf: StmConditionIf =>
        stmIf.conditioniftype_ match {
          case condIf: ConditionIf =>
            val strIfExpr   = procIfExpr(condIf.ifexp_)
            val strIfBody   = procBody(condIf.body_)
            val strElseBody = procElse(condIf.else_)
            val strBody     = recProcStatement(stms.tail, identLevel + 2, nestedLevel)
            assert(
              condIf.listelsethen_.toList.isEmpty,
              s"Extended Rules Error: condition ELSE THEN doesn't supported"
            )
            s"""${id}new ret${nestedLevel + 1} in {
                 #${idPlus}if$strIfExpr {
                 #$strIfBody
                 #$idPlus}
                 #${idPlus}else {
                 #$strElseBody
                 #$idPlus} |
                 #${idPlus}for(s<-ret${nestedLevel + 1}) {
                 #$strBody
                 #$idPlus}
                 #$id}""".stripMargin('#')
          case _ =>
            assert(
              assertion = false,
              s"Extended Rules Error: condition ${stmIf.conditioniftype_} doesn't supported"
            )
            ???
        }
      case assignment: StmAssignment =>
        val (strVar, strExpr) = assignment.assignmenttype_ match {
          case method: MethodCall => procMethodAssignment(method.methodtype_)
          case assignment: Assignment =>
            val strVar  = procVar(assignment.variable_)
            val strExpr = recProcExpr(assignment.expr_)
            (strVar, strExpr)
        }
        procAssignment(strVar, strExpr)
    }

    def ret = s"${id}ret$nestedLevel!(*s)"

    stms.headOption.map(procStm).getOrElse(ret)
  }

// Convert a method to assignment in format: variable = expression.
// Return (variable, expression).
  def procMethodAssignment(methodType: MethodType): (String, String) =
    methodType match {
      case method: Method =>
        val params = method.listexpr_.toList
        assert(
          params.nonEmpty,
          s"Extended Rules Error: no parameters in the method ${method.identliteral_}"
        )
        def checkNumParams(number: Int): Unit =
          assert(
            params.length == number,
            s"Extended Rules Error: method ${method.identliteral_} should contain $number parameters"
          )
        method.identliteral_ match {
          case "strdate" | "STRDATE" =>
            // strdate(datetime,"format",string);
            // The strdate function converts a datetime type into a string using a format that you specify.
            checkNumParams(3)
            val variable = recProcExpr(params(2))
            val dateExpr = recProcExpr(params.head)
            val format   = recProcExpr(params(1))
            format match {
              case """"%y%m%d"""" =>
                (variable, dateExpr)
              case _ =>
                assert(
                  assertion = false,
                  s"""Extended Rules Error: format "$format" in method STRDATE doesn't supported"""
                )
                ???
            }

          case _ =>
            assert(
              assertion = false,
              s"Extended Rules Error: method ${method.identliteral_} doesn't supported"
            )
            ???
        }
    }

  def procIdent(identLiteral: String): String = identLiteral.replace(":", "_colon_")

  def procVar(variable: Variable): String =
    variable match {
      case x: VarFieldInGroup =>
        val groupName = procIdent(x.identliteral_1)
        val fieldName = procIdent(x.identliteral_2)
        s""""$groupName", "$fieldName""""
      case x: VarField =>
        val fieldName = procIdent(x.identliteral_)
        s"""*recName, "$fieldName""""
      case x: VarLocal =>
        val fieldName = procIdent(x.identliteral_)
        s""""local", "$fieldName""""
    }

  def recProcExpr(expr: Expr): String = {
    def procConstant(const: Constant): String =
      const match {
        case x: ConstString  => x.stringliteral_
        case x: ConstInteger => x.integerliteral_
        case _ =>
          assert(assertion = false, s"Extended Rules Error: constant $const doesn't supported")
          ???
      }

    def procSymbol(symbol: Symbol): String =
      symbol match {
        case x: SVariable => s"*s.get(${procVar(x.variable_)})"
        case x: SConstant => procConstant(x.constant_)
      }

    def procMethodExpr(methodType: MethodType): String =
      methodType match {
        case method: Method =>
          val exprs = method.listexpr_.toList.map(recProcExpr)
          assert(
            exprs.nonEmpty,
            s"Extended Rules Error: no parameters in the method ${method.identliteral_}"
          )
          val (target, params) = (exprs.head, exprs.tail)
          def checkNumParams(number: Int): Unit =
            assert(
              params.length == number,
              s"Extended Rules Error: method ${method.identliteral_} should contain ${number + 1} parameters"
            )
          method.identliteral_ match {
            case "RIGHT" | "right" =>
              checkNumParams(1)
              val sliceEnd   = s"($target.length() - 1)"
              val sliceStart = s"($sliceEnd - ${params.head})"
              s"$target.slice($sliceStart, $sliceEnd)"
            case "LEFT" | "left" =>
              checkNumParams(1)
              val sliceStart = s"0"
              val sliceEnd   = s"(${params.head} - 1)"
              s"$target.slice($sliceStart, $sliceEnd)"
            case "MID" | "mid" =>
              checkNumParams(2)
              val sliceStart = s"${params.head}"
              val sliceEnd   = s"(${params.head} + ${params(1)})"
              s"$target.slice($sliceStart, $sliceEnd)"
            case _ =>
              assert(
                assertion = false,
                s"Extended Rules Error: method ${method.identliteral_} doesn't supported"
              )
              ???
          }
      }

    expr match {
      case x: ExprOr             => s"(${recProcExpr(x.expr_1)} or ${recProcExpr(x.expr_2)})"
      case x: ExprAnd            => s"(${recProcExpr(x.expr_1)} and ${recProcExpr(x.expr_2)})"
      case x: ExprEqual          => s"(${recProcExpr(x.expr_1)} == ${recProcExpr(x.expr_2)})"
      case x: ExprNotEqual       => s"(${recProcExpr(x.expr_1)} != ${recProcExpr(x.expr_2)})"
      case x: ExprLt             => s"(${recProcExpr(x.expr_1)} < ${recProcExpr(x.expr_2)})"
      case x: ExprLte            => s"(${recProcExpr(x.expr_1)} <= ${recProcExpr(x.expr_2)})"
      case x: ExprGt             => s"(${recProcExpr(x.expr_1)} > ${recProcExpr(x.expr_2)})"
      case x: ExprGte            => s"(${recProcExpr(x.expr_1)} >= ${recProcExpr(x.expr_2)})"
      case x: ExprAddition       => s"(${recProcExpr(x.expr_1)} + ${recProcExpr(x.expr_2)})"
      case x: ExprSubtraction    => s"(${recProcExpr(x.expr_1)} - ${recProcExpr(x.expr_2)})"
      case x: ExprMultiplication => s"(${recProcExpr(x.expr_1)} * ${recProcExpr(x.expr_2)})"
      case x: ExprDivision       => s"(${recProcExpr(x.expr_1)} / ${recProcExpr(x.expr_2)})"
      case x: ExprNot            => s"(not ${recProcExpr(x.expr_)})"
      case x: ExprMethod         => procMethodExpr(x.methodtype_)
      case x: ExprSymbol         => procSymbol(x.symbol_)
      case _ =>
        assert(assertion = false, s"Extended Rules Error: expression $expr doesn't supported")
        ???
    }
  }
}
