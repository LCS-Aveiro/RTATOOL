package rta.syntax

import rta.syntax.LtlFormula.*
import rta.syntax.Program2.QName

object LtlParser {

  // Tokens exclusivos de LTL: presença de X/U/G/F "soltos" indica que a
  // string deve ser tratada como LTL e não como PDL. Mantém a mesma
  // ambiguidade que já existia antes (um estado chamado "X" colide com o
  // operador Next) — não é novo, só foi movido para aqui.
  def looksLikeLtl(str: String): Boolean =
    tokenize(str).exists(t => Set("X", "U", "G", "F").contains(t))

  def parseLtlFormula(str: String): LtlFormula = {
    val tokens = tokenize(str)
    if (tokens.isEmpty) throw new RuntimeException("Fórmula LTL vazia")
    val reader = new TokenReader(tokens)
    val formula = parseFormula(reader)
    if (reader.hasNext) throw new RuntimeException(s"Tokens inesperados no fim: ${reader.current}")
    formula
  }

  private def tokenize(input: String): List[String] = {
    val pattern =
      """(<->|->|=>|&&|\|\||[!~\[\]\(\)<>]|==|!=|<=|>=|X|U|G|F|[a-zA-Z_][\w\.]*(\/[a-zA-Z_][\w\.]*)*|-?\d+(\.\d+)?)""".r
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

  private def parseFormula(reader: TokenReader): LtlFormula = parseIff(reader)

  private def parseIff(reader: TokenReader): LtlFormula = {
    var left = parseImpl(reader)
    while (reader.current == "<->") { reader.consume(); left = Iff(left, parseImpl(reader)) }
    left
  }

  private def parseImpl(reader: TokenReader): LtlFormula = {
    val left = parseUntil(reader)
    if (reader.current == "->" || reader.current == "=>") {
      reader.consume()
      Impl(left, parseImpl(reader)) // right-assoc
    } else left
  }

  private def parseUntil(reader: TokenReader): LtlFormula = {
    var left = parseOr(reader)
    while (reader.current == "U") { reader.consume(); left = Until(left, parseOr(reader)) }
    left
  }

  private def parseOr(reader: TokenReader): LtlFormula = {
    var left = parseAnd(reader)
    while (reader.current == "||" || reader.current == "OR") { reader.consume(); left = Or(left, parseAnd(reader)) }
    left
  }

  private def parseAnd(reader: TokenReader): LtlFormula = {
    var left = parseUnary(reader)
    while (reader.current == "&&" || reader.current == "AND") { reader.consume(); left = And(left, parseUnary(reader)) }
    left
  }

  private def parseUnary(reader: TokenReader): LtlFormula = {
    val t = reader.current
    if (t == "!" || t == "~" || t == "¬") { reader.consume(); Not(parseUnary(reader)) }
    else if (t == "X") { reader.consume(); Next(parseUnary(reader)) }
    else if (t == "G") { reader.consume(); Globally(parseUnary(reader)) }
    else if (t == "F") { reader.consume(); Eventually(parseUnary(reader)) }
    else if (t == "[") { reader.consume(); val c = parseCondition(reader); reader.expect("]"); CondProp(c) }
    else parseAtom(reader)
  }

  private def parseAtom(reader: TokenReader): LtlFormula = {
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