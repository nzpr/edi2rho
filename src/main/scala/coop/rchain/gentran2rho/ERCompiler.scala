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
    val strProcessFieldContract = createProcessFieldContract(blockList, 2)
    s"""contract processRecord(return, @state, @(recKey, recValues)) = {
       #    new processField, loop in {    
       #  $strProcessFieldContract |
       #      contract loop(@keys, @s) = {
       #        match keys {
       #          Set(head ...tail) => {
       #            new newState in {
       #              for (newState <- processField!?(s, head)) { loop!(tail, *newState) }
       #            }
       #          }
       #          _ => { return!(s) }
       #        }
       #      } |
       #      loop!(recValues.keys(), state)
       #    }
       #  }""".stripMargin('#')
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
      procExpr(expr)
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
                 #${idPlus}if($strIfExpr) {
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
              assertion = true,
              s"Extended Rules Error: condition ${stmIf.conditioniftype_} doesn't supported"
            )
            ???
        }
      case assignment: StmAssignment =>
        val (strVar, strExpr) = assignment.assignmenttype_ match {
          case method: MethodCall => procMethodAssignment(method.methodtype_)
          case assignment: Assignment =>
            val strVar  = procVar(assignment.variable_)
            val strExpr = procExpr(assignment.expr_)
            (strVar, strExpr)
        }
        procAssignment(strVar, strExpr)
    }

    def ret = s"${id}ret$nestedLevel!(*s)"

    stms.headOption.map(procStm).getOrElse(ret)
  }

  def procMethodAssignment(method: MethodType): (String, String) =
    (""""METHOD_VAR"""", """"METHOD_EXPR"""")

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

  def procExpr(expr: Expr): String =
    """"EXPR""""
}
