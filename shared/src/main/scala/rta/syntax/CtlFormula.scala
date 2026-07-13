package rta.syntax

import rta.syntax.Program2.QName

sealed trait CtlFormula
object CtlFormula {
  case object True extends CtlFormula
  case object False extends CtlFormula
  case class StateProp(name: QName) extends CtlFormula
  case class CondProp(cond: Condition) extends CtlFormula

  // Lógica Proposicional
  case class Not(p: CtlFormula) extends CtlFormula
  case class And(p: CtlFormula, q: CtlFormula) extends CtlFormula
  case class Or(p: CtlFormula, q: CtlFormula) extends CtlFormula
  case class Impl(p: CtlFormula, q: CtlFormula) extends CtlFormula
  case class Iff(p: CtlFormula, q: CtlFormula) extends CtlFormula

  // Operadores Existenciais
  case class EX(p: CtlFormula) extends CtlFormula
  case class EF(p: CtlFormula) extends CtlFormula
  case class EG(p: CtlFormula) extends CtlFormula
  case class EU(p: CtlFormula, q: CtlFormula) extends CtlFormula

  // Operadores Universais
  case class AX(p: CtlFormula) extends CtlFormula
  case class AF(p: CtlFormula) extends CtlFormula
  case class AG(p: CtlFormula) extends CtlFormula
  case class AU(p: CtlFormula, q: CtlFormula) extends CtlFormula

  //  Hybrid CTL Binder
  case class Bind(x: String, p: CtlFormula) extends CtlFormula // ↓ x . Φ
}