package rta.syntax

import rta.syntax.CtlFormula.*
import rta.syntax.Program2.QName

object CtlParser {

  def parseCtlFormula(str: String): CtlFormula = {
    val tokens = tokenize(str)
    if (tokens.isEmpty) throw new RuntimeException("Fórmula CTL vazia")
    val reader = new TokenReader(tokens)
    val formula = parseFormula(reader)
    if (reader.hasNext) throw new RuntimeException(s"Tokens inesperados no fim: ${reader.current}")
    formula
  }

  private def tokenize(input: String): List[String] = {
    val pattern = """(EX|EF|EG|EU|AX|AF|AG|AU|<->|->|=>|&&|\|\||==|!=|<=|>=|[!~\[\]\(\)<>\.]|[↓∃∀□♢⃝]|[a-zA-Z_][\w\.]*(\/[a-zA-Z_][\w\.]*)*|-?\d+(\.\d+)?)""".r
    pattern.findAllIn(input).toList
  }

  private class TokenReader(tokens: List[String]) {
    var pos = 0
    def current: String = if (pos < tokens.length) tokens(pos) else ""
    def hasNext: Boolean = pos < tokens.length
    def consume(): String = { val t = current; pos += 1; t }
    def eat(s: String): Boolean = if (current == s) { pos += 1; true } else false
    def expect(s: String): Unit = if (!eat(s)) throw new RuntimeException(s"Esperado '$s', encontrado '$current'")
  }

  private def parseFormula(reader: TokenReader): CtlFormula = parseIff(reader)

  private def parseIff(reader: TokenReader): CtlFormula = {
    var left = parseImpl(reader)
    while (reader.current == "<->") { reader.consume(); left = Iff(left, parseImpl(reader)) }
    left
  }

  private def parseImpl(reader: TokenReader): CtlFormula = {
    val left = parseOr(reader)
    if (reader.current == "->" || reader.current == "=>") {
      reader.consume()
      Impl(left, parseImpl(reader))
    } else left
  }

  private def parseOr(reader: TokenReader): CtlFormula = {
    var left = parseAnd(reader)
    while (reader.current == "||" || reader.current == "OR") { reader.consume(); left = Or(left, parseAnd(reader)) }
    left
  }

  private def parseAnd(reader: TokenReader): CtlFormula = {
    var left = parseUnary(reader)
    while (reader.current == "&&" || reader.current == "AND") { reader.consume(); left = And(left, parseUnary(reader)) }
    left
  }

  private def parseUnary(reader: TokenReader): CtlFormula = {
    val t = reader.current
    if (t == "!" || t == "~" || t == "¬") { reader.consume(); Not(parseUnary(reader)) }
    else if (t == "EX") { reader.consume(); EX(parseUnary(reader)) }
    else if (t == "EF") { reader.consume(); EF(parseUnary(reader)) }
    else if (t == "EG") { reader.consume(); EG(parseUnary(reader)) }
    else if (t == "AX") { reader.consume(); AX(parseUnary(reader)) }
    else if (t == "AF") { reader.consume(); AF(parseUnary(reader)) }
    else if (t == "AG") { reader.consume(); AG(parseUnary(reader)) }
    else if (t == "E" || t == "∃") {
       reader.consume()
       val op = reader.current
       if (op == "X" || op == "⃝") { reader.consume(); EX(parseUnary(reader)) }
       else if (op == "F" || op == "♢") { reader.consume(); EF(parseUnary(reader)) }
       else if (op == "G" || op == "□") { reader.consume(); EG(parseUnary(reader)) }
       else if (op == "(" || op == "[") {
          val open = reader.consume()
          val close = if (open == "(") ")" else "]"
          val p = parseIff(reader)
          reader.expect("U")
          val q = parseIff(reader)
          reader.expect(close)
          EU(p, q)
       } else throw new RuntimeException(s"Esperado X, F, G, ou ( após E/∃, encontrado $op")
    }
    else if (t == "A" || t == "∀") {
       reader.consume()
       val op = reader.current
       if (op == "X" || op == "⃝") { reader.consume(); AX(parseUnary(reader)) }
       else if (op == "F" || op == "♢") { reader.consume(); AF(parseUnary(reader)) }
       else if (op == "G" || op == "□") { reader.consume(); AG(parseUnary(reader)) }
       else if (op == "(" || op == "[") {
          val open = reader.consume()
          val close = if (open == "(") ")" else "]"
          val p = parseIff(reader)
          reader.expect("U")
          val q = parseIff(reader)
          reader.expect(close)
          AU(p, q)
       } else throw new RuntimeException(s"Esperado X, F, G, ou ( após A/∀, encontrado $op")
    }
    else if (t == "↓" || t == "bind") {
       reader.consume()
       val varName = reader.consume() 
       
       val actualVarName = if (varName.endsWith(".")) {
         varName.dropRight(1)
       } else {
         reader.expect(".")
         varName
       }
       
       val p = parseUnary(reader)
       Bind(actualVarName, p)
    }
    else if (t == "[") {
       reader.consume()
       val c = parseCondition(reader)
       reader.expect("]")
       CondProp(c)
    }
    else parseAtom(reader)
  }

  private def parseAtom(reader: TokenReader): CtlFormula = {
    if (reader.eat("(")) { val f = parseFormula(reader); reader.expect(")"); f }
    else if (reader.current == "true") { reader.consume(); True }
    else if (reader.current == "false") { reader.consume(); False }
    else StateProp(parseQName(reader))
  }

  private def parseQName(reader: TokenReader): QName = {
    val s = reader.consume()
    if (s.contains("/")) QName(s.split('/').toList) else QName(s.split('.').toList)
  }

  private def parseCondition(reader: TokenReader): Condition = {
    val lhsName = parseQName(reader)
    val op = reader.consume()
    val validOps = Set("==", "!=", "<=", ">=", "<", ">")
    if (!validOps.contains(op)) throw new RuntimeException(s"Operador de condição inválido: $op")
    val rhsToken = reader.consume()
    val rhsExpr: UpdateExpr =
      if (rhsToken.matches("-?\\d+(\\.\\d+)?")) {
        if (rhsToken.contains(".")) UpdateExpr.LitFloat(rhsToken.toDouble) else UpdateExpr.LitInt(rhsToken.toInt)
      } else UpdateExpr.Var(parseQName2(rhsToken))
    Condition.AtomicCond(UpdateExpr.Var(lhsName), op, rhsExpr)
  }

  private def parseQName2(s: String): QName =
    if (s.contains("/")) QName(s.split('/').toList) else QName(s.split('.').toList)
}