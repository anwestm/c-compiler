
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.{Source, StdIn}
import scala.util.control.Breaks.{break, breakable}

sealed abstract class BaseType

case class UOPType(op: String) extends BaseType
case class BOPType(op: String) extends BaseType


sealed abstract class TType extends BaseType
case class TVoid() extends TType
case class TInt() extends TType
case class TChar() extends TType
case class TIdent(r: String) extends TType
case class TPoint(t: TType) extends TType

sealed abstract class EType extends BaseType
case class EVar(r: String) extends EType
case class EInt(i: String) extends EType
case class EChar(c: String) extends EType
case class EString(r: String) extends EType
case class EBinOp(var bop: BOPType, var e1: EType, var e2: EType) extends EType
case class EUnOp(bop: UOPType, var e: EType) extends EType
case class ECall(r: String, e: List[EType]) extends EType
case class ENew(t: TType, e: EType) extends EType
case class EArrayAccess(r: String, e: EType, ro: String) extends EType

sealed abstract class SType extends BaseType
case class SExpr(e: EType) extends SType
case class SVarDef(t: TType, r: String, e: EType) extends SType
case class SVarAssign(r: String, var e: EType) extends SType
case class SArrayAssign(r: String, e1: EType, ro: String, var e2: EType) extends SType
case class SScope(s: List[SType]) extends SType
case class SIf(e: EType, s: SType, so: SType) extends SType
case class SWhile(e: EType, s: SType) extends SType
case class SBreak() extends SType
case class SReturn(eo: EType) extends SType
case class SDelete(r: String) extends SType


sealed abstract class GType() extends BaseType
case class GFuncDef(t: TType, r: String, t_r: List[(TType, String)], s: SType) extends GType
case class GFuncDecl(t: TType, r: String, t_r: List[(TType, String)]) extends GType
case class GVarDef(t: TType, r: String, e: EType) extends GType
case class GVarDecl(t: TType, r: String) extends GType
case class GStruct(r: String, t_r: List[(TType, String)]) extends GType

case class Prog(g: List[GType]) extends BaseType


object Main {

  class Printer {
    var messages: String = ""
    var verbose = false

    def print(s: String) = messages += s
    def verbose_println(s: String) = {
      if (verbose)
        Console.println(s)
    }
    def println(s: String) = messages += s + "\n"
    def flush() = Console.println(messages)
  }

  var printer = new Printer
  var lex = new Lexer("")
  var onlyLinePrint = false

  def main(args: Array[String]) {
    var filename = ""
    var doPrettyPrint = false


    if (args.length > 0) {
      for (a <- args.iterator) {
        if (a.trim.matches("--pretty-print"))
          doPrettyPrint = true
        else if (a.trim.matches("--line-error"))
          onlyLinePrint = true
        else if (a.trim.matches("--.*"))
          System.exit(1)
      }
    } else {
      println("Expected arguments")
      System.exit(1)
    }
    filename = args.last

    //println("FILENAME: " + filename)
    //println("PRETTYPRINT: " + doPrettyPrint)

    if (filename != "") {
      var lines = Source.fromFile(filename).getLines().mkString("\n")
      lex = new Lexer(lines)
    }

    var c = lex.nextToken();
    val P = parseProg(c)

    if (doPrettyPrint) {
      prettyPrint(P)
      printer.flush()
    }
  }

  def prettyPrint(baseType: BaseType): Unit = {
    baseType match {
    case p: Prog => p.g.foreach(i => {
      printer.print("\n")
      prettyPrint(i)
      printer.print("\n")
    } )
    case g: GFuncDef=>
      printer.print("GFuncDef(")
      prettyPrint(g.t)
      printer.print(", \"" + g.r + "\", { ")
      g.t_r.foreach(ts => {
        printer.print("(")
        prettyPrint(ts._1)
        printer.print(", \"" + ts._2 + "\") ")
      })
      printer.print("},\n")
      prettyPrint(g.s)
      printer.print(")\n")
    case g: GFuncDecl=>
      printer.print("GFuncDecl(")
      prettyPrint(g.t)
      printer.print(", \"" + g.r + "\", { ")
      g.t_r.foreach(ts => {
        printer.print("(")
        prettyPrint(ts._1)
        printer.print(", \"" + ts._2 + "\")")
      })
      printer.print("})\n")
    case g: GVarDef =>
      printer.print("GVarDef(")
      prettyPrint(g.t)
      printer.print(", \"" + g.r + "\", ")
      prettyPrint(g.e)
      printer.print(")\n")
    case g: GVarDecl =>
      printer.print("GVarDecl(")
      prettyPrint(g.t)
      printer.print(", \"" + g.r + "\"")
      printer.print(")\n")
    case g: GStruct =>
      printer.print("GStruct(\"" + g.r + "\",{\n")
      g.t_r.foreach(t_r => {
        printer.print("(")
        prettyPrint(t_r._1)
        printer.print(", \"" + t_r._2 + "\")\n")
      })
      printer.print("})\n")
    case s: SExpr =>
      printer.print("SExpr(")
      prettyPrint(s.e)
      printer.print(")")
    case s: SVarDef =>
      printer.print("SVarDef(")
      prettyPrint(s.t)
      printer.print(", \"" + s.r + "\"" + ", ")
      prettyPrint(s.e)
      printer.print(")")
    case s: SVarAssign =>
      printer.print("SVarAssign(\"" + s.r + "\", ")
      prettyPrint(s.e)
      printer.print(")")
    case s: SArrayAssign =>
      printer.print("SArrayAssign(\"" + s.r + "\", ")
      prettyPrint(s.e1)
      printer.print(", ")
      if (s.ro != null)
        printer.print("\"" + s.ro + "\"")
      printer.print(", ")
      prettyPrint(s.e2)
      printer.print(")")
    case s: SScope =>
      printer.print("SScope({")
      s.s.foreach(ts => {
        printer.print("\n")
        prettyPrint(ts)

      })
      printer.print("\n})")
    case s: SIf =>
      printer.print("SIf(")
      prettyPrint(s.e)
      printer.print(",\n")
      prettyPrint(s.s)
      printer.print(",\n")
      if (s.so != null) {

        prettyPrint(s.so)
      }
      printer.print(")")
    case s: SWhile =>
      printer.print("SWhile(")
      prettyPrint(s.e)
      printer.print(",\n")
      prettyPrint(s.s)
      printer.print(")")
    case s: SBreak =>
      printer.print("SBreak")
    case s: SReturn =>
      printer.print("SReturn(")
      prettyPrint(s.eo)
      printer.print(")")
    case s: SDelete =>
      printer.print("SDelete(\"" + s.r + "\")")
    case e: EVar =>
      printer.print("EVar(\"" + e.r + "\")")
    case e: EInt =>
      printer.print("EInt(" + e.i + ")")
    case e: EChar =>
      printer.print("EChar(" + e.c + ")")
    case e: EString =>
      printer.print("EString(" + e.r + ")")
    case e: EBinOp =>
      printer.print("EBinOp(" + e.bop.op + ", ")
      prettyPrint(e.e1)
      printer.print(", ")
      prettyPrint(e.e2)
      printer.print(")")
    case e: EUnOp =>
      printer.print("EUnOp(" + e.bop.op + ", ")
      prettyPrint(e.e)
      printer.print(")")
    case e: ECall =>
      printer.print("ECall(\"" + e.r + "\", {")
      for (i <- e.e.indices) {
        prettyPrint(e.e(i))
        if (i+1 != e.e.length)
          printer.print(" ")
      }
      printer.print("})")
    case e: ENew =>
      printer.print("ENew(")
      prettyPrint(e.t)
      printer.print(", ")
      prettyPrint(e.e)
      printer.print(")")
    case e: EArrayAccess =>
      printer.print("EArrayAccess(\"" + e.r + "\", ")
      prettyPrint(e.e)
      printer.print(", ")
      if (e.ro != null) {
        printer.print("\"" + e.ro + "\"")
      }
      printer.print(")")
    case t: TVoid =>
      printer.print("TVoid")
    case t: TInt =>
      printer.print("TInt")
    case t: TChar =>
      printer.print("TChar")
    case t: TIdent =>
      printer.print("TIdent(\"" + t.r + "\")")
    case t: TPoint =>
      printer.print("TPoint(")
      prettyPrint(t.t)
      printer.print(")")
    case bop: BOPType =>
      printer.print(bop.op)
    case uop: UOPType =>
      printer.print(uop.op)
    case a =>
      if (a != null)
        printer.print(a.toString)

  }
  }

  def printParseError(msg: String, tok: Token): Unit = {
    printParseError(msg, tok, null)
  }

  def printParseError(msg: String, tok: Token, prev: Token): Unit = {
    if (onlyLinePrint) {
      if (prev != null)
        System.err.println(prev.lineNumber)
      else {
        if (tok != null)
          System.err.println(tok.lineNumber)
        else
          System.err.println(lex.lineNumber)
      }
    } else
      println(msg)
    System.exit(1)
    return null
  }


  def parseTy(tok: Token): TType = {
    printer.verbose_println("parseTy"+ tok)
    var tp = null
    if (lex.peekToken() == TKN_OPERATOR("*")) {
      var next = lex.nextToken()
      return  TPoint(parseTy(tok))
    }
    tok match {
      case key: TKN_KEYWORD => key.keyword match {
        case "void" => TVoid()
        case "int" => TInt()
        case "char" => TChar()
        case _ =>
          printParseError("Error: Unexpected keyword: ",key)
          System.exit(1)
          null
      }
      case iden: TKN_IDENTIFIER => TIdent(iden.name)
      case op: TKN_OPERATOR if op.op == "*" =>
        TPoint(parseTy(lex.nextToken()))
      case _ =>
        printParseError("Ty not found: ", tok)
        null
    }
  }

  def parseP(tok: Token): EType = {
    var prev = tok
    tok match {
      case iden: TKN_IDENTIFIER if lex.peekToken() == TKN_SEPARATOR("[") =>
        var next = lex.nextToken() // consume [
        next = lex.nextToken()
        val e = parseExpr(next)
        prev = next
        next = lex.nextToken()
        if (next == TKN_SEPARATOR("]")) {
          if (lex.peekToken() == TKN_SEPARATOR(".")) {
            next = lex.nextToken() // consume .
            prev = next
            next = lex.nextToken()
            if (next.isInstanceOf[TKN_IDENTIFIER]) {
              EArrayAccess(iden.name, e, next.asInstanceOf[TKN_IDENTIFIER].name)
            } else {
              printParseError("Error: Expetected Identifier. Got: ", next)
              null
            }
          } else {
            EArrayAccess(iden.name, e, null)
          }

        } else {
          printParseError("Error: Expetected ]", next)
          null
        }
      case iden: TKN_IDENTIFIER if lex.peekToken() == TKN_SEPARATOR("(") =>
        var next = lex.nextToken()
        if (next == TKN_SEPARATOR("(")) {
          next = lex.nextToken()
          var exprList = new ListBuffer[EType]()
          while (next != TKN_SEPARATOR(")")) {
            exprList += parseExpr(next)
            next = lex.nextToken()
            if (next != TKN_SEPARATOR(")")) {
              if (next == TKN_SEPARATOR(","))
                next = lex.nextToken()
              else {
                printParseError("Expected , Got: ", next)
                null
              }
            }
          }
          ECall(iden.name, exprList.toList)
        } else {
          printParseError("Expected ( Got: ", next)
          null
        }
      case keyw: TKN_KEYWORD =>
        if (keyw.keyword == "new") {
          val ty = parseTy(lex.nextToken())
          var next = lex.nextToken()
          if (next == TKN_SEPARATOR("[")) {
            next = lex.nextToken()
            val expr = parseExpr(next)
            next = lex.nextToken()
            if (next == TKN_SEPARATOR("]")) {
              return ENew(ty, expr)
            }  else {
              printParseError("Expected ] Got: ", next)
              null
            }
          } else {
            printParseError("Expected [ Got: ", next)
            null
          }
        } else {
          printParseError("Expected keyword new! Got: ", tok)
          null
        }
      case iden: TKN_IDENTIFIER =>
        EVar(iden.name)
      case uint: TKN_UINT =>
        EInt(uint.cnst)
      case chr: TKN_CHAR =>
        EChar(chr.cnst)
      case str: TKN_STRING => EString(str.cnst)
        EString(str.cnst)
      case unop: TKN_OPERATOR if List("!", "~", "-").contains(unop.op)=>
        val q = getOpPrecedence(unop, binop = false)
        val next = lex.nextToken()
        val expr = parseExpr(next, q)
        return EUnOp(UOPType(unop.op), expr)
      case tknseparator: TKN_SEPARATOR if tknseparator.sep == "(" =>
        var nxt = lex.nextToken()
        val expr = parseExpr(nxt, 0)
        nxt = lex.nextToken()
        if (nxt == TKN_SEPARATOR(")")) {
          return expr
        } else {
          printParseError("Error: Exptected ) Got: ", nxt)
          System.exit(1)
          null
        }
      case _ =>
        null
      case _ =>
        printParseError("Expected P: ", tok)
        null
    }
  }

  def getOpPrecedence(tok: Token, binop: Boolean = true): Int = {
    if (binop) {
      tok match {
        case op: TKN_OPERATOR =>
          op.op match {
            case "||" => 0
            case "&&" => 1
            case "|" => 2
            case "&" => 3
            case "==" | "!=" => 4
            case "<" | ">" | "<=" | ">=" => 5
            case "<<" | ">>" => 6
            case "+" | "-" => 7
            case "*" | "/" | "%" => 8
            case _ =>
              printParseError("Expected Binary Operator! Got:", tok)
              -1
          }
        case _ =>
          printParseError("Expected Operator! Got:", tok)
          -1
      }
    } else {
      tok match {
        case op: TKN_OPERATOR =>
          op.op match {
            case "!" | "~" | "-" => 9
            case _ =>
              printParseError("Expected Unary Operator! Got:", tok)
              -1
          }
        case _ =>
          printParseError("Expected Operator! Got:", tok)
          -1

      }
    }
  }

  def parseExpr(tok: Token, prec: Int = 0): EType = {
    printer.verbose_println("parseExpr"+tok)

    var t = parseP(tok)
    if (t == null)  {
      printParseError("Expected P! Got: ", tok)
      null
    }

    if (lex.peekToken().isInstanceOf[TKN_SEPARATOR])
      return t

    var next = lex.peekToken()

    while (next.isInstanceOf[TKN_OPERATOR] && getOpPrecedence(next) >= prec) {
      next = lex.nextToken()

      val op = next
      val op_str = next.asInstanceOf[TKN_OPERATOR].op
      val q = 1 + getOpPrecedence(op)

      next = lex.nextToken()
      val t1 = parseExpr(next, q)
      if (t1 == null) {
        printParseError("Expected valid expr: Got null from: ",next)
        null
      }

      t = EBinOp(BOPType(op_str), t, t1)
      next = lex.peekToken()
    }

    return t
  }



  def parseStatement(tok: Token):SType = {
    printer.verbose_println("parseStatement" + tok)
    tok match {

      case key: TKN_KEYWORD => key.keyword match {
        case "if" =>
          var next = lex.nextToken()
          if (next == TKN_SEPARATOR("(")) {
            next = lex.nextToken()
            var expr = parseExpr(next)
            next = lex.nextToken()
            if (next == TKN_SEPARATOR(")")) {
              next = lex.nextToken()
              val stmt = parseStatement(next)
              if (lex.peekToken() == TKN_KEYWORD("else")) {
                lex.nextToken() // consume
                next = lex.nextToken()
                val stmt2 = parseStatement(next)
                return SIf(expr, stmt, stmt2)
              } else {
                return SIf(expr, stmt, null)
              }
            } else {
              printParseError("Expected \')\'. Got:", next)
              null
            }
          } else {
            printParseError("Expected (", next)
            null
          }
        case "while" =>
          var next = lex.nextToken()
          if (next == TKN_SEPARATOR("(")) {
            next = lex.nextToken()
            val expr = parseExpr(next)

            next = lex.nextToken()
            if (next == TKN_SEPARATOR(")")) {
              next = lex.nextToken()
              val stmt = parseStatement(next)
              return SWhile(expr, stmt)
            } else {
              printParseError("Expected )", next)
              null
            }
          } else {
            printParseError("Expected (", next)
            null
          }
        case "break" =>
          val next = lex.nextToken()
          if (next == TKN_SEPARATOR(";"))
            return SBreak()
          else {
            printParseError("Expected ;", next)
            null
          }
        case "return" =>
          var next = lex.nextToken()
          if (next == TKN_SEPARATOR(";")) {
            return SReturn(null)
          }
          val expr = parseExpr(next)
          next = lex.nextToken()
          if (next == TKN_SEPARATOR(";")) {
            SReturn(expr)
          } else {
            printParseError("Expected ; Got:", next)
            null
          }
        case "delete" =>
          var next = lex.nextToken()
          if (next == TKN_SEPARATOR("[")) {
            next = lex.nextToken()
            if (next == TKN_SEPARATOR("]")) {
              next = lex.nextToken()
              if (next.isInstanceOf[TKN_IDENTIFIER]) {
                val name = next.asInstanceOf[TKN_IDENTIFIER].name
                next = lex.nextToken()
                if (next == TKN_SEPARATOR(";")) {
                  return SDelete(name)
                } else {
                  printParseError("Expected ;Got:  ", next)
                  null
                }
              } else {
                printParseError("Expected Identifier Got: ", next)
                null
              }
            } else {
              printParseError("Expected [ Got: ", next)
              null
            }
          } else {
            printParseError("Expected [ Got: ", next)
            null
          }
        case "for" =>
          var next = lex.nextToken()
          if (next == TKN_SEPARATOR("(")) {
            next = lex.nextToken()
            val varassign = parseVarAssign(next)

            next = lex.nextToken()

            if (next == TKN_SEPARATOR(";")) {
              next = lex.nextToken()
              val expr = parseExpr(next)
              next = lex.nextToken()
              if (next == TKN_SEPARATOR(";")) {
                next = lex.nextToken()
                val assign = parseAssign(next)
                next = lex.nextToken()
                if (next == TKN_SEPARATOR(")")) {
                  next = lex.nextToken()
                  val stmt = parseStatement(next)
                  SScope(List(varassign, SWhile(expr, SScope(List(stmt, assign)))))
                } else {
                  printParseError("Expected ) Got: ", next)
                  null
                }
              } else {
                printParseError("Expected ; Got: ", next)
                null
              }
            } else {
              printParseError("Expected ; Got: ", next)
              null
            }
          } else {
            printParseError("Expected ( Got: ", next)
            null
          }
        case _ =>
          var next = tok
          val assign = parseVarAssign(next)

          next = lex.nextToken()
          if (next == TKN_SEPARATOR(";")) {
            assign
          } else {
            printParseError("S Expected ; Got: ", next)
            null
          }
      }
      case seP: TKN_SEPARATOR if seP.sep == "{" =>
        var next = lex.nextToken()
        var stmtList = new ListBuffer[SType]()
        while (next != TKN_SEPARATOR("}")) {
          stmtList += parseStatement(next)
          next = lex.nextToken()
        }
        SScope(stmtList.toList)

      case _ =>
        var next = tok
        val assign = parseVarAssign(next)

        next = lex.nextToken()
        if (next == TKN_SEPARATOR(";")) {
          assign
        } else {
          printParseError("S Expected ; Got: ", next, tok)
          null
        }
    }
  }

  def parseLValue(tok: Token): SType = {
    printer.verbose_println("parseLValue"+ tok)
    if (tok.isInstanceOf[TKN_IDENTIFIER]) {
      if (lex.peekToken() == TKN_SEPARATOR("[")) {
        var next = lex.nextToken() // consume [
        next = lex.nextToken()
        val e = parseExpr(next)
        next = lex.nextToken()
        if (next == TKN_SEPARATOR("]")) {
          if (lex.peekToken() == TKN_SEPARATOR(".")) {
            next = lex.nextToken() // consume .
            next = lex.nextToken()
            if (next.isInstanceOf[TKN_IDENTIFIER]) {
              SArrayAssign(tok.asInstanceOf[TKN_IDENTIFIER].name, e, next.asInstanceOf[TKN_IDENTIFIER].name, null)
            } else {
              printParseError("Error: Expetected Identifier. Got: ", next)
              null
            }
          } else {
            SArrayAssign(tok.asInstanceOf[TKN_IDENTIFIER].name, e, null, null)
          }

        } else {
          printParseError("Error: Expected ]", next)
          null
        }
      } else {
        SVarAssign(tok.asInstanceOf[TKN_IDENTIFIER].name, null)
      }
    } else  {
      null
    }

  }

  def parseAssign(tok: Token): SType = {
    printer.verbose_println("parseAssign"+tok)
    if (tok.isInstanceOf[TKN_IDENTIFIER] && lex.peekToken() == TKN_SEPARATOR("(")) {
      var next = lex.nextToken()
      if (next == TKN_SEPARATOR("(")) {
        next = lex.nextToken()
        var exprList = new ListBuffer[EType]()
        while (next != TKN_SEPARATOR(")")) {
          exprList += parseExpr(next)
          next = lex.nextToken()
          if (next!= TKN_SEPARATOR(")")) {
            if (next == TKN_SEPARATOR(","))
              next = lex.nextToken()
            else {
              printParseError("Expected , Got: ", next)
              null
            }
          }
        }
        if (next == TKN_SEPARATOR(")")) {
          SExpr(ECall(tok.asInstanceOf[TKN_IDENTIFIER].name, exprList.toList))
        } else {
          printParseError("Expected \')\'. Got:", next)
          null
        }

      } else {
        printParseError("Expected \'(\'. Got:", next)
        null
      }
    } else {
      val lval = parseLValue(tok)
      lval match {                  //S ArrayAssign("a1", EInt(2), "x", EBinOp(+, EArrayAccess("a1", EInt(2), "x"), EInt(1)))
        case arra: SArrayAssign => // SArrayAssign("a", EInt(6),  , EBinOp(+, EArrayAccess("a", EInt(6), ), EInt(1)))
          var str_bop = ""
          if (lex.peekToken() == TKN_OPERATOR("++")) {
            lex.nextToken() // consume
            str_bop = "+"
          } else if (lex.peekToken() == TKN_OPERATOR("--")) {
            lex.nextToken() // consume
            str_bop = "-"
          } else if (lex.peekToken() == TKN_OPERATOR("=")) {
            lex.nextToken() // consume
            var next = lex.nextToken()
            lval.asInstanceOf[SArrayAssign].e2 = parseExpr(next)
            return lval;
          }
          lval.asInstanceOf[SArrayAssign].e2 = EBinOp(BOPType(str_bop), EArrayAccess(arra.r, arra.e1, arra.ro), EInt("1"))
          return lval
        case vara: SVarAssign =>
          var str_bop = ""
          if (lex.peekToken() == TKN_OPERATOR("++")) {
            lex.nextToken() // consume
            str_bop = "+"
          } else if (lex.peekToken() == TKN_OPERATOR("--")) {
            lex.nextToken() // consume
            str_bop = "-"
          } else if(lex.peekToken() == TKN_OPERATOR("=")) {
            lex.nextToken() // consume
            var next = lex.nextToken()
            lval.asInstanceOf[SVarAssign].e = parseExpr(next)
            return lval
          }
          lval.asInstanceOf[SVarAssign].e = EBinOp(BOPType(str_bop), EVar(vara.r), EInt("1"))
          return lval
        case _ =>
          null
      }


    }
  }

  def parseVarAssign(tok: Token): SType = {
    printer.verbose_println("parseVarAssign"+ tok)
    val ty = parseTy(tok)
    if (ty != null && lex.peekToken().isInstanceOf[TKN_IDENTIFIER]) {
      val iden = lex.nextToken()
      if (iden.isInstanceOf[TKN_IDENTIFIER]) {
        var next = lex.nextToken()
        if (next == TKN_OPERATOR("=")) {
          next = lex.nextToken()
          val e = parseExpr(next)
          return SVarDef(ty, iden.asInstanceOf[TKN_IDENTIFIER].name, e)
        } else {
          printParseError("Error: Expected \'=\'! Got: ", next)
          System.exit(1)
          null
        }
      } else {
        printParseError("Error: Expected identifier! Got: " ,iden)
        System.exit(1)
        null
      }
    } else {
      print("")
      parseAssign(tok) // Maybe add check here
    }

  }


  def parseParams(tok: Token): List[(TType, String)] = {
    printer.verbose_println("parseParams"+ tok)
    if (tok == TKN_SEPARATOR(")"))
      return List.empty

    val ty = parseTy(tok)
    if (ty == null) {
      printParseError("Error: Expected [ty] from: ", tok)
      return List.empty
    }

    val id = lex.nextToken()
    id match {
      case identifier: TKN_IDENTIFIER =>
        if (lex.peekToken() == TKN_SEPARATOR(",")) {
          lex.nextToken()
          List((ty, identifier.name)) ++ parseParams(lex.nextToken())
        } else {
          List((ty, identifier.name))
        }
      case _ =>
        printParseError("Error: Expected identifier. Got: ", id)
        System.exit(1)
        null
    }
  }


  def parseGlobal(tok: Token): GType = {
    printer.verbose_println("parseGlobal" + tok)
    if (tok == TKN_KEYWORD("extern")) {
      var next = lex.nextToken()
      val Ty = parseTy(next)
      if (Ty != null) {
        val iden = lex.nextToken()
        if (iden.isInstanceOf[TKN_IDENTIFIER]) {
          next = lex.nextToken()
          if (next == TKN_SEPARATOR(";"))
            return GVarDecl(Ty, iden.asInstanceOf[TKN_IDENTIFIER].name)
          else if (next == TKN_SEPARATOR("(")) {
            next = lex.nextToken()
            val params = parseParams(next)
            if (params.nonEmpty)
              next = lex.nextToken()
            if (next == TKN_SEPARATOR(")")) {
              next = lex.nextToken()
              if (next == TKN_SEPARATOR(";")) {
                return GFuncDecl(Ty, iden.asInstanceOf[TKN_IDENTIFIER].name, params)
              } else {
                printParseError("Error: Expected ; Got: ", next)
              }
            } else {
              printParseError("Error: Expected ) Got: ", next)
            }
          } else {
            printParseError("Error: could not parse extern! Tok: ", next)
          }
        } else {
          printParseError("Error: Expected identifier Got: ",iden)
        }
      } else {
        printParseError("Error: Expected Type Got: ", next)
      }
    } else if (tok == TKN_KEYWORD("struct")) {
      var iden = lex.nextToken()
      if (iden.isInstanceOf[TKN_IDENTIFIER]) {
        var next = lex.nextToken()
        if (next == TKN_SEPARATOR("{")) {
          next = lex.nextToken()
          var structList = ListBuffer[(TType, String)]()
          while (next != TKN_SEPARATOR("}")) {
            val ty = parseTy(next)
            if (ty != null) {
              next = lex.nextToken()
              if (next.isInstanceOf[TKN_IDENTIFIER]) {
                val iden = next.asInstanceOf[TKN_IDENTIFIER].name
                next = lex.nextToken()
                if (next == TKN_SEPARATOR(";")) {
                  structList.append((ty, iden))
                } else {
                  printParseError("Exptected ; Got: ", next)
                }
              } else {
                printParseError("Exptected identifier! Got: ", next)
              }
            } else {
              printParseError("Exptected Ty Got: ",iden)
            }
            next = lex.nextToken()
          }
          next = lex.nextToken()
          if (next == TKN_SEPARATOR(";")) {
            return GStruct(iden.asInstanceOf[TKN_IDENTIFIER].name, structList.toList)
          } else {
            printParseError("Exptected ; Got: ", next)
          }

        } else {
          printParseError("Exptected { Got: ", next)
        }
      } else {
        printParseError("Exptected identifier! Got: ", iden)
      }
      //var next = lex.nextToken()
    } else {

      val Ty = parseTy(tok)
      if (Ty != null) {
        val iden = lex.nextToken()
        if (iden.isInstanceOf[TKN_IDENTIFIER]) {
          val idenName = iden.asInstanceOf[TKN_IDENTIFIER].name

          var next = lex.nextToken()
          if (next == TKN_SEPARATOR("(")) {

            next = lex.nextToken()
            val p = parseParams(next)
            if (p.nonEmpty)
              next = lex.nextToken()
            if (next == TKN_SEPARATOR(")")) {
              next = lex.nextToken()
              if (next == TKN_SEPARATOR(";")) {
                return GFuncDecl(Ty, idenName, p)
              } else if (next == TKN_SEPARATOR("{")) {
                next = lex.nextToken()
                var stmtList = new ListBuffer[SType]()
                while (next != TKN_SEPARATOR("}")) {
                  stmtList += parseStatement(next)
                  next = lex.nextToken()
                }
                if (next == TKN_SEPARATOR("}")) {
                  return GFuncDef(Ty, idenName, p, SScope(stmtList.toList))
                } else {
                  printParseError("Error: Expected \'}\'. Got: ", next)
                  System.exit(1)
                }
              }

            } else {
              printParseError("Error: Expected \')\'. Got: ", next)
              System.exit(1)
            }
          } else if (next == TKN_OPERATOR("=")) {
            next = lex.nextToken()
            val expr = parseExpr(next)
            next = lex.nextToken()
            if (next == TKN_SEPARATOR(";")) {
              return GVarDef(Ty, idenName, expr)
            } else {
              printParseError("Error: Expected \';\'. Got: ", next)
              System.exit(1)
            }

          } else {
            printParseError("Error: Expected identifier. Got: ", next)
            System.exit(1)
          }
        }

      } else {
        printParseError("Error: Expected Ty. Got: ", tok)
      }
    }
    return null
  }

  def parseProg(tok: Token): Prog = {
    var gList = ListBuffer[GType]()

    var next = tok
    var g: GType = null
    do {
      if (next != null) {
        g = parseGlobal(next)
      } else {
        g = null
      }
      if (g != null) {
        gList += g
      }

      next = lex.nextToken()
    } while (g != null)

    Prog(gList.toList)
  }

}