import Main.printParseError

sealed abstract class Token(var lineNumber: Int)

case class TKN_IDENTIFIER(name: String) extends Token(0)

case class TKN_KEYWORD(keyword: String) extends Token(0)

case class TKN_UINT(cnst: String) extends Token(0)
case class TKN_CHAR(cnst: String) extends Token(0)
case class TKN_STRING(cnst: String) extends Token(0)

case class TKN_OPERATOR(op: String) extends Token(0)

case class TKN_SEPARATOR(sep: String) extends Token(0)

class Lexer (val line: String){

  val keywords = List("break", "char", "delete", "else", "extern", "for", "if", "int", "new", "return", "struct", "void", "while");

  var i = 0

  var peekTok: Token = null
  var currentTok: Token = null
  var internalLineNumber: Int = 1
  var lineNumber: Int = 1
  var last_c = ""

  def peek(): String = {
    if (i < line.length)
      line.charAt(i).toString
    else
      ""
  }

  def nextChar(): String = {
    i += 1
    //println("NEXTCHAR: " + line.charAt(i - 1).toString)
    if (i - 1 < line.length) {
      if (line.charAt(i - 1).toString == "\n")
        internalLineNumber += 1
      line.charAt(i - 1).toString
    } else {
      ""
    }


  }


  def parseIdentifierAndKeyWord(chrSeq: String): Token = {
    //println(chrSeq + ", " + peek())
    var p = peek()
    if ((chrSeq + p).matches("[_a-zA-Z][_a-zA-Z0-9]*") && p != "") {
      parseIdentifierAndKeyWord(chrSeq + nextChar())
    } else if (chrSeq.matches("[_a-zA-Z][_a-zA-Z0-9]*")) {
      if (chrSeq.length > 0) {
        if (keywords.contains(chrSeq))
          TKN_KEYWORD(chrSeq)
        else
          TKN_IDENTIFIER(chrSeq)
      } else
        null
    } else
      null
  }

  def parseSeparator(str: String): Token = str match {
    case "{" | "}" | "(" | ")" | ";" | "," | "[" | "]" | "." => TKN_SEPARATOR(str)
    case _ => null
  }

  def parseOperator(str: String): Token = str match {
    case "!" | "~" | "-" | "*" | "/" | "%" | "+" | "-" | "<" | ">" | "&" | "|" | "=" =>
      val p = peek()
      if (p != "")
        parseOperator(str + peek())
      else
        TKN_OPERATOR(str)
    case "<<" | ">>" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "++" | "--" =>
      nextChar()
      TKN_OPERATOR(str)
    case _ =>
      if (str.length > 1)
        TKN_OPERATOR(str.substring(0, 1))
      else
        null
  }

  def parseIntConstant(str: String): Token = {
    val p = peek()
    if ((str + peek()).matches("0|[1-9][0-9]*") && p != "") {
      parseIntConstant(str + nextChar())
    } else if (str.matches("0|[1-9][0-9]*")) {
      TKN_UINT(str)
    } else
      null
  }

  def parseIntHexConstant(str: String): Token = {
    val p = peek()
    if (str.length == 1 && peek().matches("[xX]"))
      return parseIntHexConstant(str + nextChar())
    if ((str + peek()).matches("0[xX][0-9a-fA-F]+")) {
      parseIntHexConstant(str + nextChar())
    } else if (str.matches("0[xX][0-9a-fA-F]+")) {
      val intstr = Integer.parseInt(str.replace("0x", "").replace("0X", ""), 16)
      TKN_UINT(Integer.toString(intstr))
    } else
      null
  }

  def parseCharConstant(str: String): Token = {

    var cnst = str
    if (str == "\'")
      cnst += nextChar()
    else
      return null

    if (cnst.matches("('[\\x20-\\x7E])"))
      cnst += nextChar()
    else {
      println("Parse error: Expected valid char or \' Got: " + (str+peek()))
      System.exit(1)
      null
    }

    if ((cnst.matches("(''')|('\\\\')|('\"')")&& !(cnst+peek()).matches("\'\\\\\'\'"))) {
      println("Parse error: Character constant is either to long or an invalid escape character is used Got: " + str)
      System.exit(1)
    }
    if (List("\'\\n\'", "\'\\t\'", "\'\\\\\'", "\'\\\'\'", "\'\\\"\'").contains(cnst + peek()))
       TKN_CHAR(cnst + nextChar())
    else if (str.endsWith("\'"))
      TKN_CHAR(cnst)
    else {
      println("Parse error: Character constant is either to long or an invalid escape character is used Got: " + str)
      System.exit(1)
      null
    }
  }


    def parseStrConstant(str: String): Token = {
    if (str.endsWith("\"") && (!str.endsWith("\\\"") || str.endsWith("\\\\\""))&& str.length > 1) {

      var i = 0
      var last_i = 0
      while(str.indexOf('\'', last_i) != -1) {
        i = str.indexOf('\'', last_i)
        var j = 1
        while(i-j >= 0 && str.charAt(i-j) == '\\') {
          j+=1
        }
        if ((j-1)%2 == 0) {
          //println("Invalid string: " + i + " " + str + " " + j)
          printParseError("Invalid String: ", null)
        }
        last_i = i + 1
      }
      return TKN_STRING(str)
    }

    val p = peek()
    if (str.startsWith("\"") && p != "") {
      if (str.matches("(\\\"[\\x20-\\x7E]*)")) {
        val next = nextChar()


        /*if (str.last != '\\' && (next.last == '\'' || next.last == '\"')) {
          printParseError("Invalid string, ", null)
          return null
        }*/

        if (str.last == '\\' && (next.charAt(0) == 'n' || next.charAt(0)  == 't' || next.charAt(0)  == '\\' || next.charAt(0) == '\"' || next.charAt(0) == '\''))
          parseStrConstant(str + next + nextChar())
        else if(str.last != '\\')
          parseStrConstant(str + next)
        else {
          null
        }
      } else {
        null
      }

    } else
      null

  }



  def peekToken(): Token = {
    if (peekTok == null)
      peekTok = nextToken()
    peekTok
  }

  def nextToken(): Token = {
    if (peekTok != null) {
      val tok = peekTok
      peekTok = null
      return tok
    }

    if (peek().isEmpty) {
      return null
    }
    var illegalCommentPossibility = false
    var c = ""

    do {
      c = nextChar()

      // Skip of multiline comments
      if (c == "/" && peek() == "*") {
          illegalCommentPossibility = last_c.matches("[_a-zA-Z][_a-zA-Z0-9]*")
        while(!(c == "*" && peek() == "/") && c != "") {
          c = nextChar()
          if (c == "/" && peek() == "*") { // nested comment
            println("Nested comments are not allowed")
            System.exit(1)
          }
          if (c == "") {
            println("Unclosed block-comment")
            System.exit(1)
          }

        }
        c = nextChar() // consume /
        c = nextChar()
        if (illegalCommentPossibility && c.matches("[_a-zA-Z][_a-zA-Z0-9]*")) {
          println("You can not do a comment inside an identier")
          System.exit(1)
        }
      }

      // Skip of single line comments
      if ((c == "/" && peek() == "/") || c == "#") {
        while(c != "\n" && c != "")
          c = nextChar()
      }

      last_c = c
      //println("LEX LOOP: " + c)
    } while (peek() != "" && (c == " " || c == "\n"))

    //println("NEXT TOKEN CHAR: " + c)

    var tok = parseIdentifierAndKeyWord(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    tok = parseSeparator(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    tok = parseIntHexConstant(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    tok = parseIntConstant(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    tok = parseCharConstant(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    tok = parseStrConstant(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    tok = parseOperator(c)
    if (tok != null) {
      currentTok = tok
      lineNumber = internalLineNumber
      tok.lineNumber = internalLineNumber
      return tok
    }

    /*val tmp = TKN_STRING("asd")
    tmp.lineNumber = internalLineNumber
    printParseError("Lexer found no token", TKN_STRING("LEX_ERROR"))*/

    currentTok = null
    return null

  }
}
