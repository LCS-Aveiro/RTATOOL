package rta.syntax
import rta.syntax.PdlProgram
import rta.syntax.Program2.QName
import rta.syntax.Condition.*



sealed trait Formula
object Formula {
  case object True extends Formula
  case object False extends Formula
  case class StateProp(name: QName) extends Formula      // Representa uma verificação de estado, ex: "s1"
  case class CondProp(cond: Condition) extends Formula // Representa uma verificação de condição, ex: "[c < 2]"
  case class Not(p: Formula) extends Formula
  case class And(p: Formula, q: Formula) extends Formula
  case class Or(p: Formula, q: Formula) extends Formula
  case class Impl(p: Formula, q: Formula) extends Formula
  case class Iff(p: Formula, q: Formula) extends Formula

  case class PipeAnd(p: Formula, q: Formula) extends Formula //  '&|&'

  // --- NÓS PDL ---
  case class Box(p: Formula) extends Formula
  case class Diamond(p: Formula) extends Formula
  case class BoxP(act: PdlProgram, p: Formula) extends Formula      // [α]φ
  case class DiamondP(act: PdlProgram, p: Formula) extends Formula  // <α>φ


  // --- NÓS LTL (Traços) ---
  case class LtlNext(p: Formula) extends Formula              // X φ
  case class LtlUntil(p: Formula, q: Formula) extends Formula // φ U ψ
  case class LtlGlobally(p: Formula) extends Formula          // G φ
  case class LtlEventually(p: Formula) extends Formula        // F φ

  



}
