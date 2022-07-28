package coop.rchain.gentran2rho

import coop.rchain.gtrn2rho.ast.extended_rules.Absyn._

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

object Compiler {

  def createRhoFromAST(term: ProgramType): (String, String, String) =
    term match {
      case prog: Program =>
        val localDecl = createDecls(prog.declarationlist_)
        val localInit = createInit(prog.stmlist_)
        val blocks    = createProcessRecordContract(prog.blocklist_)
        (localDecl, localInit, blocks)
    }

  val ident = "  "

  def createDecls(declList: DeclarationList): String = {
    def procDecl(decl: Declaration): String =
      decl match {
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
    val decls = declList match {
      case x: Declarations => x.listdeclaration_.toList
    }
    val strDecls     = decls.map(x => procDecl(x))
    val strNewDecl   = strDecls.mkString(", ")
    val strStateDecl = strDecls.map(x => s""""$x":*$x""").mkString(", ")
    s"""new LMap, state in {
       #  contract LMap(return, @"decl") = {
       #    new $strNewDecl in {
       #      state!({$strStateDecl}) |
       #      for(_ <<- state) {return!(true)}
       #    }
       #  } |
       #  contract LMap(return, @"get", @field) = {
       #    for(@s <<- state) {
       #      let chan <- s.get(field) in {
       #        for(x <<- chan) {return!(*x)}
       #      }
       #    }    
       #  } |
       #  contract LMap(return, @"update", @field, @value) = {
       #    for(@s <<- state) {
       #      let chan <- s.get(field) in {
       #        for(_ <- chan) {chan!(value) | for(_ <<- chan) {return!(true)}}
       #      }
       #    }
       #  }
       #}""".stripMargin('#')
  }

  def createInit(stmList: StmList): String = {
    val strBody = procStatements(stmList, 1, 0)
    s"""contract localInit(ret0, l) = {
       #$strBody
       #}""".stripMargin('#')
  }

  def createProcessRecordContract(blockList: BlockList): String = {
    def id(level: Int)          = ident * level
    val strProcessFieldContract = createProcessFieldContract(blockList, 2)
    s"""${id(0)}contract processRecord(return, s, l, @(recKey, recValues)) = {
       #${id(1)}new processField, saveRecordDataLoop, processFieldLoop in {    
       #$strProcessFieldContract |
       #${id(2)}contract processFieldLoop(@keys) = {
       #${id(3)}match keys {
       #${id(4)}Set(head... tail) => {
       #${id(5)}for(_ <- processField!?(head)) { processFieldLoop!(tail) }
       #${id(4)}}
       #${id(4)}_ => { return!(true) }
       #${id(3)}}
       #${id(2)}} |
       #${id(2)}contract saveRecordDataLoop(@KVs) = {
       #${id(3)}match KVs {
       #${id(4)}[head... tail] => {
       #${id(5)}for(_ <- s!?("update", recKey, head.nth(0), head.nth(1))) {
       #${id(6)}saveRecordDataLoop!(tail)
       #${id(5)}}
       #${id(4)}}
       #${id(4)}_ => { processFieldLoop!(recValues.keys()) }
       #${id(3)}}
       #${id(2)}} |
       #${id(2)}saveRecordDataLoop!(recValues.toList())
       #${id(1)}}
       #${id(0)}}""".stripMargin('#')
  }

  def createProcessFieldContract(blockList: BlockList, identLevel: Int): String = {
    def id(addLevel: Int) = ident * (identLevel + addLevel)
    def createCase(name: String): String =
      s"""${id(3)}"$name" => { for(@res <- $name!?()) { ret!(res) } }"""

    val blocks = blockList match {
      case x: Blocks => x.listblock_.toList
    }
    val strBlocks       = blocks.map(x => createBlock(x, identLevel + 2))
    val strBlockSection = strBlocks.mkString(" |\n")
    val names           = blocks.map(getBlockName)
    val strNewNames     = names.mkString(", ")
    val strCases        = names.map(createCase).mkString("\n")

    s"""${id(0)}contract processField(ret, @fieldName) = {
        #${id(1)}new $strNewNames in {
        #$strBlockSection |
        #${id(2)}match fieldName {
        #$strCases
        #${id(3)}_ => ret!(true)
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
        s"""${id}contract $fieldName(ret0) = {
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

    def id(addLevel: Int = 0) = ident * (identLevel + addLevel)

    def createGetConsume(simpleVar: simpleVariable): String =
      simpleVar match {
        case simpleVarFieldInGroup(groupName, fieldName) =>
          s"""s!?("get", "$groupName", "$fieldName")"""
        case simpleVarField(fieldName) => s"""s!?("get", recKey, "$fieldName")"""
        case simpleVarLocal(fieldName) => s"""l!?("get", "$fieldName")"""
      }

    def createValues(vars: Set[simpleVariable]): String = {
      val strings: List[String] = vars.toList.map { simpleVar =>
        val strName       = createValName(simpleVar)
        val strGetConsume = createGetConsume(simpleVar)
        s"""@$strName <- $strGetConsume"""
      }
      s"for(${strings.mkString("; ")}) {"
    }

    def createUpdateConsume(simpleVar: simpleVariable, strExpr: String): String =
      simpleVar match {
        case simpleVarFieldInGroup(groupName, fieldName) =>
          s"""s!?("update", "$groupName", "$fieldName", $strExpr)"""
        case simpleVarField(fieldName) => s"""s!?("update", recKey, "$fieldName", $strExpr)"""
        case simpleVarLocal(fieldName) => s"""l!?("update", "$fieldName", $strExpr)"""
      }

    def procAssignment(variable: Variable, strExpr: String, addIdent: Int): String = {
      val strUpdateConsume = createUpdateConsume(createSimpleVar(variable), strExpr)
      val strBody          = recProcStatement(stms.tail, identLevel + addIdent + 1, nestedLevel)
      s"""${id(addIdent)}for(_ <- $strUpdateConsume) {
         #$strBody
         #${id(addIdent)}}""".stripMargin('#')
    }

    def procIfExpr(ifExp: IfExp): (String, Set[simpleVariable]) = {
      val expr = ifExp match {
        case x: IfExpCapital => x.expr_
        case x: IfExpSmall   => x.expr_
      }
      recProcExpr(expr, Set())
    }

    def procBody(body: Body, addIdent: Int): String = {
      val stmList = body match {
        case x: BodyCapital => x.stmlist_
        case x: BodySmall   => x.stmlist_
      }
      val statements = stmList match {
        case x: Statements => x.liststatement_.toList
      }
      recProcStatement(statements, identLevel + 2 + addIdent, nestedLevel + 1)
    }

    def procElse(els: Else, addIdent: Int): String =
      els match {
        case x: ElseCapital => procBody(x.body_, addIdent)
        case x: ElseSmall   => procBody(x.body_, addIdent)
        case _: ElseNon     => recProcStatement(List(), identLevel + 2 + addIdent, nestedLevel + 1)
      }

    def procStm(stm: Statement) = {
      val (strStm, varList) = stm match {
        case stmIf: StmConditionIf =>
          stmIf.conditioniftype_ match {
            case condIf: ConditionIf =>
              val (strIfExpr, vars) = procIfExpr(condIf.ifexp_)
              val addIdent          = if (vars.nonEmpty) 1 else 0
              val strIfBody         = procBody(condIf.body_, addIdent)
              val strElseBody       = procElse(condIf.else_, addIdent)
              val strBody           = recProcStatement(stms.tail, identLevel + addIdent + 2, nestedLevel)
              assert(
                condIf.listelsethen_.toList.isEmpty,
                s"Extended Rules Error: condition ELSE THEN doesn't supported"
              )
              val str =
                s"""${id(addIdent)}new ret${nestedLevel + 1} in {
                   #${id(addIdent + 1)}if$strIfExpr {
                   #$strIfBody
                   #${id(addIdent + 1)}}
                   #${id(addIdent + 1)}else {
                   #$strElseBody
                   #${id(addIdent + 1)}} |
                   #${id(addIdent + 1)}for(_<-ret${nestedLevel + 1}) {
                   #$strBody
                   #${id(addIdent + 1)}}
                   #${id(addIdent)}}""".stripMargin('#')
              (str, vars)
            case _ =>
              assert(
                assertion = false,
                s"Extended Rules Error: condition ${stmIf.conditioniftype_} doesn't supported"
              )
              ???
          }
        case assignment: StmAssignment =>
          val (variable, strExpr, vars) = assignment.assignmenttype_ match {
            case method: MethodCall => procMethodAssignment(method.methodtype_)
            case assignment: Assignment =>
              val (strExpr, vars) = recProcExpr(assignment.expr_, Set())
              (assignment.variable_, strExpr, vars)
          }
          val addIdent = if (vars.nonEmpty) 1 else 0
          val str      = procAssignment(variable, strExpr, addIdent)
          (str, vars)
      }
      if (varList.nonEmpty)
        s"""${id()}${createValues(varList)}
           #$strStm
           #${id()}}""".stripMargin('#')
      else strStm
    }

    def ret = s"${id()}ret$nestedLevel!(true)"

    stms.headOption.map(procStm).getOrElse(ret)
  }

  sealed trait simpleVariable
  final case class simpleVarFieldInGroup(groupName: String, fieldName: String)
      extends simpleVariable
  final case class simpleVarField(fieldName: String) extends simpleVariable
  final case class simpleVarLocal(fieldName: String) extends simpleVariable

  // Convert a method to assignment in format: variable = expression.
  // Return (variable, strExpression, set of all variables in method expressions).
  def procMethodAssignment(methodType: MethodType): (Variable, String, Set[simpleVariable]) =
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
        def getVariable(expr: Expr): Option[Variable] =
          expr match {
            case s: ExprSymbol =>
              s.symbol_ match {
                case v: SVariable => Some(v.variable_)
                case _            => None
              }
            case _ => None
          }
        method.identliteral_ match {
          case "strdate" | "STRDATE" =>
            // strdate(datetime,"format",string);
            // The strdate function converts a datetime type into a string using a format that you specify.
            checkNumParams(3)
            val variable: Variable = getVariable(params(2)).getOrElse {
              assert(
                assertion = false,
                s"""Extended Rules Error: there isn't variable in method STRDATE"""
              )
              ???
            }
            val (dateExpr, vars1) = recProcExpr(params.head, Set())
            val (format, vars2)   = recProcExpr(params(1), vars1)
            format match {
              case """"%y%m%d"""" =>
                (variable, dateExpr, vars2)
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

  def createValName(simpleVar: simpleVariable): String =
    simpleVar match {
      case x: simpleVarFieldInGroup =>
        s"val_${x.groupName}_${x.fieldName}"
      case x: simpleVarField =>
        s"val_current_${x.fieldName}"
      case x: simpleVarLocal =>
        s"val_local_${x.fieldName}"
    }

  def createSimpleVar(variable: Variable): simpleVariable =
    variable match {
      case x: VarFieldInGroup =>
        simpleVarFieldInGroup(procIdent(x.identliteral_1), procIdent(x.identliteral_2))
      case x: VarField =>
        simpleVarField(procIdent(x.identliteral_))
      case x: VarLocal =>
        simpleVarLocal(procIdent(x.identliteral_))
    }

  /**
    *
    * @param expr
    * @return (term, Set[(prefix, fieldName)]), where
    *         term - string term of expression;
    *         Set[Variable]) - list of all variables which used in expression.
    */
  def recProcExpr(expr: Expr, vars: Set[simpleVariable]): (String, Set[simpleVariable]) = {
    def procConstant(const: Constant): String =
      const match {
        case x: ConstString  => x.stringliteral_
        case x: ConstInteger => x.integerliteral_
        case _ =>
          assert(assertion = false, s"Extended Rules Error: constant $const doesn't supported")
          ???
      }

    def procVar(variable: Variable): (String, simpleVariable) = {
      val simpleVar = createSimpleVar(variable)
      (createValName(simpleVar), simpleVar)
    }

    def procSymbol(symbol: Symbol): (String, Option[simpleVariable]) =
      symbol match {
        case x: SVariable =>
          val (term, simpleVar) = procVar(x.variable_)
          (term, Some(simpleVar))
        case x: SConstant => (procConstant(x.constant_), None)
      }

    def procMethodExpr(methodType: MethodType): (String, Set[simpleVariable]) =
      methodType match {
        case method: Method =>
          val listExpr = method.listexpr_.toList
          val (exprs, newVars) = listExpr.foldLeft((List[String](), vars)) { (acc, expr) =>
            val (accExpr, accV) = acc
            val (str, newV)     = recProcExpr(expr, accV)
            (accExpr :+ str, newV)
          }
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
              (s"$target.slice($sliceStart, $sliceEnd)", newVars)
            case "LEFT" | "left" =>
              checkNumParams(1)
              val sliceStart = s"0"
              val sliceEnd   = s"(${params.head} - 1)"
              (s"$target.slice($sliceStart, $sliceEnd)", newVars)
            case "MID" | "mid" =>
              checkNumParams(2)
              val sliceStart = s"${params.head}"
              val sliceEnd   = s"(${params.head} + ${params(1)})"
              (s"$target.slice($sliceStart, $sliceEnd)", newVars)
            case _ =>
              assert(
                assertion = false,
                s"Extended Rules Error: method ${method.identliteral_} doesn't supported"
              )
              ???
          }
      }

    def proc2Exprs(expr1: Expr, expr2: Expr, bundle: String): (String, Set[simpleVariable]) = {
      val (str1, newVars1) = recProcExpr(expr1, vars)
      val (str2, newVars2) = recProcExpr(expr2, newVars1)
      (s"($str1 $bundle $str2)", newVars2)
    }

    expr match {
      case x: ExprOr             => proc2Exprs(x.expr_1, x.expr_2, "or")
      case x: ExprAnd            => proc2Exprs(x.expr_1, x.expr_2, "and")
      case x: ExprEqual          => proc2Exprs(x.expr_1, x.expr_2, "==")
      case x: ExprNotEqual       => proc2Exprs(x.expr_1, x.expr_2, "!=")
      case x: ExprLt             => proc2Exprs(x.expr_1, x.expr_2, "<")
      case x: ExprLte            => proc2Exprs(x.expr_1, x.expr_2, "<=")
      case x: ExprGt             => proc2Exprs(x.expr_1, x.expr_2, ">")
      case x: ExprGte            => proc2Exprs(x.expr_1, x.expr_2, ">=")
      case x: ExprAddition       => proc2Exprs(x.expr_1, x.expr_2, "+")
      case x: ExprSubtraction    => proc2Exprs(x.expr_1, x.expr_2, "-")
      case x: ExprMultiplication => proc2Exprs(x.expr_1, x.expr_2, "*")
      case x: ExprDivision       => proc2Exprs(x.expr_1, x.expr_2, "/")
      case x: ExprNot =>
        val (str, newVars) = recProcExpr(x.expr_, vars)
        (s"(not $str)", newVars)
      case x: ExprMethod => procMethodExpr(x.methodtype_)
      case x: ExprSymbol =>
        val (str, vOpt) = procSymbol(x.symbol_)
        (str, vOpt.map(vars + _).getOrElse(vars))
      case _ =>
        assert(assertion = false, s"Extended Rules Error: expression $expr doesn't supported")
        ???
    }
  }
}
