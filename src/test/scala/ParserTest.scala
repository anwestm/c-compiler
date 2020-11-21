import Main.{Printer}
import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  def cleanStr(s: String): String = s.replaceAll("[\\n\\t\\s]", "")


  test("Parser.empty") {
    Main.printer = new Printer
    Main.lex = new Lexer("void empty(){\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"empty\", {},\n  SScope({\n\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.one_param_return") {
    Main.printer = new Printer
    Main.lex = new Lexer("int one_param_return(int x){\n  return x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"one_param_return\", {(TInt,\"x\")},\n  SScope({\n    SReturn(EVar(\"x\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.two_params_return") {
    Main.printer = new Printer
    Main.lex = new Lexer("int two_params_return(int x, int y){\n  return x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"two_params_return\", {(TInt,\"x\") (TInt,\"y\")},\n  SScope({\n    SReturn(EVar(\"x\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.call_no_args_call_statemant") {
    Main.printer = new Printer
    Main.lex = new Lexer("void call_no_args_call_statemant(){\n  empty();\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"call_no_args_call_statemant\", {},\n  SScope({\n    SExpr(ECall(\"empty\",{}))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.call_one_args") {
    Main.printer = new Printer
    Main.lex = new Lexer("int call_one_args(){\n  return one_param_return(123);\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"call_one_args\", {},\n  SScope({\n    SReturn(ECall(\"one_param_return\",{EInt(123)}))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.call_two_args_var_expr") {
    Main.printer = new Printer
    Main.lex = new Lexer("int call_two_args_var_expr(int x){\n  return two_params_return(321,x);\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"call_two_args_var_expr\", {(TInt,\"x\")},\n  SScope({\n    SReturn(ECall(\"two_params_return\",{EInt(321) EVar(\"x\")}))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.print_char_consts") {
    Main.printer = new Printer
    Main.lex = new Lexer("void print_char_consts(){\n  putchar('A');\n  putchar('\\n');\n  putchar('\\t');\n  putchar('.');\n  putchar('\\\\');\n  putchar('\\'');\n  putchar('\\\"');\n  putchar('\\n');\n  return;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"print_char_consts\", {},\n  SScope({\n    SExpr(ECall(\"putchar\",{EChar('A')}))\n    SExpr(ECall(\"putchar\",{EChar('\\n')}))\n    SExpr(ECall(\"putchar\",{EChar('\\t')}))\n    SExpr(ECall(\"putchar\",{EChar('.')}))\n    SExpr(ECall(\"putchar\",{EChar('\\\\')}))\n    SExpr(ECall(\"putchar\",{EChar('\\'')}))\n    SExpr(ECall(\"putchar\",{EChar('\\\"')}))\n    SExpr(ECall(\"putchar\",{EChar('\\n')}))\n    SReturn()\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.arith_assignments") {
    Main.printer = new Printer
    Main.lex = new Lexer("void arith_assignments(int x, int y, int z) {\n  x = x + 1;\n  x = x - y * 8;\n  y = 12 % z / 3;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"arith_assignments\", {(TInt,\"x\") (TInt,\"y\") (TInt,\"z\")},\n  SScope({\n    SVarAssign(\"x\", EBinOp(+, EVar(\"x\"), EInt(1)))\n    SVarAssign(\"x\", EBinOp(-, EVar(\"x\"), EBinOp(*, EVar(\"y\"), EInt(8))))\n    SVarAssign(\"y\", EBinOp(/, EBinOp(%, EInt(12), EVar(\"z\")), EInt(3)))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.arith_assignments_ext") {
    Main.printer = new Printer
    Main.lex = new Lexer("void arith_assignments_ext(int x){\n  x = 8 + -2 - -8 * (9 + 1);\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"arith_assignments_ext\", {(TInt,\"x\")},\n  SScope({\n    SVarAssign(\"x\", EBinOp(-, EBinOp(+, EInt(8), EUnOp(-, EInt(2))), EBinOp(*, EUnOp(-, EInt(8)), EBinOp(+, EInt(9), EInt(1)))))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.logic_operations") {
    Main.printer = new Printer
    Main.lex = new Lexer("void logic_operations(int x, int y, int z){\n  x = y | z & 7; \n  x = y || z && y | 1;\n  y = x > y && y < x || z >= y && ((x <= y) == 0);\n  z = x == y || x != z;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"logic_operations\", {(TInt,\"x\") (TInt,\"y\") (TInt,\"z\")},\n  SScope({\n    SVarAssign(\"x\", EBinOp(|, EVar(\"y\"), EBinOp(&, EVar(\"z\"), EInt(7))))\n    SVarAssign(\"x\", EBinOp(||, EVar(\"y\"), EBinOp(&&, EVar(\"z\"), EBinOp(|, EVar(\"y\"), EInt(1)))))\n    SVarAssign(\"y\", EBinOp(||, EBinOp(&&, EBinOp(>, EVar(\"x\"), EVar(\"y\")), EBinOp(<, EVar(\"y\"), EVar(\"x\"))), EBinOp(&&, EBinOp(>=, EVar(\"z\"), EVar(\"y\")), EBinOp(==, EBinOp(<=, EVar(\"x\"), EVar(\"y\")), EInt(0)))))\n    SVarAssign(\"z\", EBinOp(||, EBinOp(==, EVar(\"x\"), EVar(\"y\")), EBinOp(!=, EVar(\"x\"), EVar(\"z\"))))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.logic_operations_ext") {
    Main.printer = new Printer
    Main.lex = new Lexer("void logic_operations_ext(int x, int y, int z){\n  y = ~z >= y && !(x <= y);\n  x = x << 3 & z >> 7;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"logic_operations_ext\", {(TInt,\"x\") (TInt,\"y\") (TInt,\"z\")},\n  SScope({\n    SVarAssign(\"y\", EBinOp(&&, EBinOp(>=, EUnOp(~, EVar(\"z\")), EVar(\"y\")), EUnOp(!, EBinOp(<=, EVar(\"x\"), EVar(\"y\")))))\n    SVarAssign(\"x\", EBinOp(&, EBinOp(<<, EVar(\"x\"), EInt(3)), EBinOp(>>, EVar(\"z\"), EInt(7))))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.if_then_else") {
    Main.printer = new Printer
    Main.lex = new Lexer("int if_then_else(int x, int y){\n  if(x == y)\n    return y;\n  \n  if(x > 0){\n    if(x == 3){\n      x = x + 1;\n      return x;\n    }\n  }\n  else{\n    x = 7;\n    return x + 1;\n  }\n  return x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"if_then_else\", {(TInt,\"x\") (TInt,\"y\")},\n  SScope({\n    SIf(EBinOp(==, EVar(\"x\"), EVar(\"y\")),\n      SReturn(EVar(\"y\")), )\n    SIf(EBinOp(>, EVar(\"x\"), EInt(0)),\n      SScope({\n        SIf(EBinOp(==, EVar(\"x\"), EInt(3)),\n          SScope({\n            SVarAssign(\"x\", EBinOp(+, EVar(\"x\"), EInt(1)))\n            SReturn(EVar(\"x\"))\n          }), )\n      }),\n      SScope({\n        SVarAssign(\"x\", EInt(7))\n        SReturn(EBinOp(+, EVar(\"x\"), EInt(1)))\n      }))\n    SReturn(EVar(\"x\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.if_then_dangeling_else") {
    Main.printer = new Printer
    Main.lex = new Lexer("int if_then_dangeling_else(int x){\n  if(x > 0)\n    if(x < 10)\n      return x + 1;\n    else\n      return x + 2;\n  return x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"if_then_dangeling_else\", {(TInt,\"x\")},\n  SScope({\n    SIf(EBinOp(>, EVar(\"x\"), EInt(0)),\n      SIf(EBinOp(<, EVar(\"x\"), EInt(10)),\n        SReturn(EBinOp(+, EVar(\"x\"), EInt(1))),\n        SReturn(EBinOp(+, EVar(\"x\"), EInt(2)))), )\n    SReturn(EVar(\"x\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.while_break") {
    Main.printer = new Printer
    Main.lex = new Lexer("int while_break(int x){\n  while(x < 10){\n    if(x == 7)\n      break;\n    x = x + 1;      \n  }\n  return x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"while_break\", {(TInt,\"x\")},\n  SScope({\n    SWhile(EBinOp(<, EVar(\"x\"), EInt(10)),\n      SScope({\n        SIf(EBinOp(==, EVar(\"x\"), EInt(7)),\n          SBreak, )\n        SVarAssign(\"x\", EBinOp(+, EVar(\"x\"), EInt(1)))\n      }))\n    SReturn(EVar(\"x\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.pluspluss_minusminus") {
    Main.printer = new Printer
    Main.lex = new Lexer("void pluspluss_minusminus(int x){\n  x++;\n  x--;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"pluspluss_minusminus\", {(TInt,\"x\")},\n  SScope({\n    SVarAssign(\"x\", EBinOp(+, EVar(\"x\"), EInt(1)))\n    SVarAssign(\"x\", EBinOp(-, EVar(\"x\"), EInt(1)))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.defining_local_variables") {
    Main.printer = new Printer
    Main.lex = new Lexer("void defining_local_variables(){\n  int x = 1;\n  int y = 5;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"defining_local_variables\", {},\n  SScope({\n    SVarDef(TInt, \"x\", EInt(1))\n    SVarDef(TInt, \"y\", EInt(5))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.hexadecimal_numbers") {
    Main.printer = new Printer
    Main.lex = new Lexer("void hexadecimal_numbers(){\n  int x = 0x1a3F;\n  int y = 0X001fA;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"hexadecimal_numbers\", {},\n  SScope({\n    SVarDef(TInt, \"x\", EInt(6719))\n    SVarDef(TInt, \"y\", EInt(506))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.global_x") {
    Main.printer = new Printer
    Main.lex = new Lexer("int global_x = 5 + 10;")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GVarDef(TInt, \"global_x\", EBinOp(+, EInt(5), EInt(10)))")
    assert(outStr == testStr)
  }

  test("Parser.global_y") {
    Main.printer = new Printer
    Main.lex = new Lexer("extern int global_y;")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GVarDecl(TInt, \"global_y\")")
    assert(outStr == testStr)
  }

  test("Parser.use_global") {
    Main.printer = new Printer
    Main.lex = new Lexer("int use_global(int x){\n  return global_x + global_y + x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"use_global\", {(TInt,\"x\")},\n  SScope({\n    SReturn(EBinOp(+, EBinOp(+, EVar(\"global_x\"), EVar(\"global_y\")), EVar(\"x\")))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.use_global_y") {
    Main.printer = new Printer
    Main.lex = new Lexer("int global_y = 7;")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GVarDef(TInt, \"global_y\", EInt(7))")
    assert(outStr == testStr)
  }

  test("Parser.for_loop_with_break") {
    Main.printer = new Printer
    Main.lex = new Lexer("int for_loop_with_break(int x, int y){\n  for(int i=0; i < x; i++){\n    if(i >= y)\n      break;\n  }\n  for(int i=0; i < x; i++)\n    x = x + 1;\n \n  return x;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"for_loop_with_break\", {(TInt,\"x\") (TInt,\"y\")},\n  SScope({\n    SScope({\n      SVarDef(TInt, \"i\", EInt(0))\n      SWhile(EBinOp(<, EVar(\"i\"), EVar(\"x\")),\n        SScope({\n          SScope({\n            SIf(EBinOp(>=, EVar(\"i\"), EVar(\"y\")),\n              SBreak, )\n          })\n          SVarAssign(\"i\", EBinOp(+, EVar(\"i\"), EInt(1)))\n        }))\n    })\n    SScope({\n      SVarDef(TInt, \"i\", EInt(0))\n      SWhile(EBinOp(<, EVar(\"i\"), EVar(\"x\")),\n        SScope({\n          SVarAssign(\"x\", EBinOp(+, EVar(\"x\"), EInt(1)))\n          SVarAssign(\"i\", EBinOp(+, EVar(\"i\"), EInt(1)))\n        }))\n    })\n    SReturn(EVar(\"x\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.Empty struct") {
    Main.printer = new Printer
    Main.lex = new Lexer("struct S1 {\n};")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GStruct(\"S1\",{\n})")
    assert(outStr == testStr)
  }

  test("Parser.Struct With Elements") {
    Main.printer = new Printer
    Main.lex = new Lexer("struct S2 {\n  int x;\n  char y;\n};")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GStruct(\"S2\",{\n  (TInt, \"x\")\n  (TChar, \"y\")\n})")
    assert(outStr == testStr)
  }

  test("Parser.struct_arrays") {
    Main.printer = new Printer
    Main.lex = new Lexer("void struct_arrays(){ \n  S2 * a1 = new S2[5]; \n  a1[2].x = 3;\n  a1[2].x++;\n  a1[2].x--;\n  int y = a1[2].y + 1;\n  delete[] a1;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"struct_arrays\", {},\n  SScope({\n    SVarDef(TPoint(TIdent(\"S2\")), \"a1\", ENew(TIdent(\"S2\"), EInt(5)))\n    SArrayAssign(\"a1\", EInt(2), \"x\", EInt(3))\n    SArrayAssign(\"a1\", EInt(2), \"x\", EBinOp(+, EArrayAccess(\"a1\", EInt(2), \"x\"), EInt(1)))\n    SArrayAssign(\"a1\", EInt(2), \"x\", EBinOp(-, EArrayAccess(\"a1\", EInt(2), \"x\"), EInt(1)))\n    SVarDef(TInt, \"y\", EBinOp(+, EArrayAccess(\"a1\", EInt(2), \"y\"), EInt(1)))\n    SDelete(\"a1\")\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.int_arrays") {
    Main.printer = new Printer
    Main.lex = new Lexer("void int_arrays(int x, int* p){\n  int* a = new int[x];\n  int y = x % 10;\n  a[7] = a[19+y];\n  a[6]++;\n  a[6]--;  \n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"int_arrays\", {(TInt,\"x\") (TPoint(TInt),\"p\")},\n  SScope({\n    SVarDef(TPoint(TInt), \"a\", ENew(TInt, EVar(\"x\")))\n    SVarDef(TInt, \"y\", EBinOp(%, EVar(\"x\"), EInt(10)))\n    SArrayAssign(\"a\", EInt(7),  , EArrayAccess(\"a\", EBinOp(+, EInt(19), EVar(\"y\")), ))\n    SArrayAssign(\"a\", EInt(6),  , EBinOp(+, EArrayAccess(\"a\", EInt(6), ), EInt(1)))\n    SArrayAssign(\"a\", EInt(6),  , EBinOp(-, EArrayAccess(\"a\", EInt(6), ), EInt(1)))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.string_test") {
    Main.printer = new Printer
    Main.lex = new Lexer("char string_test(int x){\n  char * a = \"Test\\n\\t\\\\\\'\\\"\";\n  return a[x];\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TChar, \"string_test\", {(TInt,\"x\")},\n  SScope({\n    SVarDef(TPoint(TChar), \"a\", EString(\"Test\\n\\t\\\\\\'\\\"\"))\n    SReturn(EArrayAccess(\"a\", EVar(\"x\"), ))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.print_int") {
    Main.printer = new Printer
    Main.lex = new Lexer("void print_int(int x){\n  if(x < 0){\n    putchar('-');\n    x = x * (0 - 1);  \n  }\n  int i = 1000000;\n  while(i != 0){\n    if(x >= i || (x == 0 && i == 1))\n      putchar('0' + x / i);\n    x = x % i;\n    i = i / 10;\n  }\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"print_int\", {(TInt,\"x\")},\n  SScope({\n    SIf(EBinOp(<, EVar(\"x\"), EInt(0)),\n      SScope({\n        SExpr(ECall(\"putchar\",{EChar('-')}))\n        SVarAssign(\"x\", EBinOp(*, EVar(\"x\"), EBinOp(-, EInt(0), EInt(1))))\n      }), )\n    SVarDef(TInt, \"i\", EInt(1000000))\n    SWhile(EBinOp(!=, EVar(\"i\"), EInt(0)),\n      SScope({\n        SIf(EBinOp(||, EBinOp(>=, EVar(\"x\"), EVar(\"i\")), EBinOp(&&, EBinOp(==, EVar(\"x\"), EInt(0)), EBinOp(==, EVar(\"i\"), EInt(1)))),\n          SExpr(ECall(\"putchar\",{EBinOp(+, EChar('0'), EBinOp(/, EVar(\"x\"), EVar(\"i\")))})), )\n        SVarAssign(\"x\", EBinOp(%, EVar(\"x\"), EVar(\"i\")))\n        SVarAssign(\"i\", EBinOp(/, EVar(\"i\"), EInt(10)))\n      }))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.print_int_ln") {
    Main.printer = new Printer
    Main.lex = new Lexer("void print_int_ln(int x){\n  print_int(x);\n  putchar('\\n');\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"print_int_ln\", {(TInt,\"x\")},\n  SScope({\n    SExpr(ECall(\"print_int\",{EVar(\"x\")}))\n    SExpr(ECall(\"putchar\",{EChar('\\n')}))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.print_string") {
    Main.printer = new Printer
    Main.lex = new Lexer("void print_string(char* s){\n  int i = 0;\n  while(s[i]){\n    putchar(s[i]);\n    i++;\n  }\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"print_string\", {(TPoint(TChar),\"s\")},\n  SScope({\n    SVarDef(TInt, \"i\", EInt(0))\n    SWhile(EArrayAccess(\"s\", EVar(\"i\"), ),\n      SScope({\n        SExpr(ECall(\"putchar\",{EArrayAccess(\"s\", EVar(\"i\"), )}))\n        SVarAssign(\"i\", EBinOp(+, EVar(\"i\"), EInt(1)))\n      }))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.Tree") {
    Main.printer = new Printer
    Main.lex = new Lexer("struct Tree{\n  int val;\n  Tree * left;\n  Tree * right;  \n};")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GStruct(\"Tree\",{\n  (TInt, \"val\")\n  (TPoint(TIdent(\"Tree\")), \"left\")\n  (TPoint(TIdent(\"Tree\")), \"right\")\n})")
    assert(outStr == testStr)
  }

  test("Parser.node") {
    Main.printer = new Printer
    Main.lex = new Lexer("Tree* node(Tree* left, int x, Tree* right){\n  Tree* t = new Tree[1];\n  t[0].val = x;\n  t[0].left = left;\n  t[0].right = right;\n  return t;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TPoint(TIdent(\"Tree\")), \"node\", {(TPoint(TIdent(\"Tree\")),\"left\") (TInt,\"x\") (TPoint(TIdent(\"Tree\")),\"right\")},\n  SScope({\n    SVarDef(TPoint(TIdent(\"Tree\")), \"t\", ENew(TIdent(\"Tree\"), EInt(1)))\n    SArrayAssign(\"t\", EInt(0), \"val\", EVar(\"x\"))\n    SArrayAssign(\"t\", EInt(0), \"left\", EVar(\"left\"))\n    SArrayAssign(\"t\", EInt(0), \"right\", EVar(\"right\"))\n    SReturn(EVar(\"t\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.leaf") {
    Main.printer = new Printer
    Main.lex = new Lexer("Tree* leaf(int x){\n  Tree* t = new Tree[1];\n  t[0].val = x;\n  t[0].left = 0;\n  t[0].right = 0;\n  return t;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TPoint(TIdent(\"Tree\")), \"leaf\", {(TInt,\"x\")},\n  SScope({\n    SVarDef(TPoint(TIdent(\"Tree\")), \"t\", ENew(TIdent(\"Tree\"), EInt(1)))\n    SArrayAssign(\"t\", EInt(0), \"val\", EVar(\"x\"))\n    SArrayAssign(\"t\", EInt(0), \"left\", EInt(0))\n    SArrayAssign(\"t\", EInt(0), \"right\", EInt(0))\n    SReturn(EVar(\"t\"))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.inorder_traversal") {
    Main.printer = new Printer
    Main.lex = new Lexer("void inorder_traversal(Tree* t){\n  if(t[0].left == 0){\n    print_int(t[0].val);\n    putchar(',');\n  }\n  else{\n    inorder_traversal(t[0].left);\n    print_int(t[0].val);\n    putchar(',');\n    inorder_traversal(t[0].right);    \n  }\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"inorder_traversal\", {(TPoint(TIdent(\"Tree\")),\"t\")},\n  SScope({\n    SIf(EBinOp(==, EArrayAccess(\"t\", EInt(0), \"left\"), EInt(0)),\n      SScope({\n        SExpr(ECall(\"print_int\",{EArrayAccess(\"t\", EInt(0), \"val\")}))\n        SExpr(ECall(\"putchar\",{EChar(',')}))\n      }),\n      SScope({\n        SExpr(ECall(\"inorder_traversal\",{EArrayAccess(\"t\", EInt(0), \"left\")}))\n        SExpr(ECall(\"print_int\",{EArrayAccess(\"t\", EInt(0), \"val\")}))\n        SExpr(ECall(\"putchar\",{EChar(',')}))\n        SExpr(ECall(\"inorder_traversal\",{EArrayAccess(\"t\", EInt(0), \"right\")}))\n      }))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.delete_tree") {
    Main.printer = new Printer
    Main.lex = new Lexer("extern void delete_tree(Tree* t);")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDecl(TVoid, \"delete_tree\", {(TPoint(TIdent(\"Tree\")),\"t\")})")
    assert(outStr == testStr)
  }

  test("Parser.test_recursive_data_structures") {
    Main.printer = new Printer
    Main.lex = new Lexer("void test_recursive_data_structures(){\n  print_string(\"---test-recursive-data-structures---\\n\");\n  Tree* t = node(node(leaf(1), 2, leaf(3)), 4, leaf(5));\n  inorder_traversal(t);\n  putchar('\\n');\n  delete_tree(t);\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"test_recursive_data_structures\", {},\n  SScope({\n    SExpr(ECall(\"print_string\",{EString(\"---test-recursive-data-structures---\\n\")}))\n    SVarDef(TPoint(TIdent(\"Tree\")), \"t\", ECall(\"node\",{ECall(\"node\",{ECall(\"leaf\",{EInt(1)}) EInt(2) ECall(\"leaf\",{EInt(3)})}) EInt(4) ECall(\"leaf\",{EInt(5)})}))\n    SExpr(ECall(\"inorder_traversal\",{EVar(\"t\")}))\n    SExpr(ECall(\"putchar\",{EChar('\\n')}))\n    SExpr(ECall(\"delete_tree\",{EVar(\"t\")}))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.delete_tree_rec") {
    Main.printer = new Printer
    Main.lex = new Lexer("void delete_tree(Tree* t){\n  if(t[0].left != 0){\n    delete_tree(t[0].left);\n    t[0].left = 0;\n    delete_tree(t[0].right);\n  }\n  delete[] t;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"delete_tree\", {(TPoint(TIdent(\"Tree\")),\"t\")},\n  SScope({\n    SIf(EBinOp(!=, EArrayAccess(\"t\", EInt(0), \"left\"), EInt(0)),\n      SScope({\n        SExpr(ECall(\"delete_tree\",{EArrayAccess(\"t\", EInt(0), \"left\")}))\n        SArrayAssign(\"t\", EInt(0), \"left\", EInt(0))\n        SExpr(ECall(\"delete_tree\",{EArrayAccess(\"t\", EInt(0), \"right\")}))\n      }), )\n    SDelete(\"t\")\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.text1") {
    Main.printer = new Printer
    Main.lex = new Lexer("char* text1 = \"Text\\n\"; ")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GVarDef(TPoint(TChar), \"text1\", EString(\"Text\\n\"))")
    assert(outStr == testStr)
  }

  test("Parser.print_test_strings") {
    Main.printer = new Printer
    Main.lex = new Lexer("void print_test_strings(){\n  print_string(text1);\n  char * text2 = \"Text2\\n\";\n  print_string(text2);\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TVoid, \"print_test_strings\", {},\n  SScope({\n    SExpr(ECall(\"print_string\",{EVar(\"text1\")}))\n    SVarDef(TPoint(TChar), \"text2\", EString(\"Text2\\n\"))\n    SExpr(ECall(\"print_string\",{EVar(\"text2\")}))\n  }))")
    assert(outStr == testStr)
  }

  test("Parser.main") {
    Main.printer = new Printer
    Main.lex = new Lexer("int main(){\n  test_recursive_data_structures();\n  print_test_strings();\n  return 0;\n}")
    val P = Main.parseProg(Main.lex.nextToken())
    Main.prettyPrint(P)

    val outStr = cleanStr(Main.printer.messages)
    val testStr = cleanStr("GFuncDef(TInt, \"main\", {},\n  SScope({\n    SExpr(ECall(\"test_recursive_data_structures\",{}))\n    SExpr(ECall(\"print_test_strings\",{}))\n    SReturn(EInt(0))\n  }))")
    assert(outStr == testStr)
  }

}