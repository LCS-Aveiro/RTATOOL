package rta.syntax

import rta.syntax.Program2.QName

sealed trait LtlFormula
object LtlFormula {
  case object True extends LtlFormula
  case object False extends LtlFormula
  case class StateProp(name: QName) extends LtlFormula     // ReTA: aresta/hyperaresta ativa
  case class CondProp(cond: Condition) extends LtlFormula  // ReGA: valor de variável

  case class Not(p: LtlFormula) extends LtlFormula
  case class And(p: LtlFormula, q: LtlFormula) extends LtlFormula
  case class Or(p: LtlFormula, q: LtlFormula) extends LtlFormula
  case class Impl(p: LtlFormula, q: LtlFormula) extends LtlFormula
  case class Iff(p: LtlFormula, q: LtlFormula) extends LtlFormula

  case class Next(p: LtlFormula) extends LtlFormula              // X
  case class Until(p: LtlFormula, q: LtlFormula) extends LtlFormula // U
  case class Globally(p: LtlFormula) extends LtlFormula          // G
  case class Eventually(p: LtlFormula) extends LtlFormula        // F
}