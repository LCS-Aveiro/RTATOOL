package rta.syntax

import rta.backend.RxSemantics
import rta.syntax.Program2.EdgeMap
import rta.syntax.{Condition, UpdateExpr, Statement, AssignStmt, ArrayAssignStmt, IfThenStmt, ForeachStmt, ReturnStmt, RuntimeValue, PrintStmt, FunctionDef}
import rta.backend.DBM
import scala.annotation.tailrec
import scala.language.implicitConversions

object Program2:

  type Rel[A,B] = Map[A,Set[B]]
  def empty[A,B] = Map[A,Set[B]]().withDefaultValue(Set())
  def add[A,B](ab:(A,B), r:Rel[A,B]) = r + (ab._1 -> (r(ab._1)+(ab._2)))
  def join[A,B](r1:Rel[A,B], r2:Rel[A,B]) = r1 ++ (r2.map(ab => ab._1 -> (r1(ab._1)++(ab._2))))

  private def isGlobalControlVar(q: QName): Boolean = q.n.mkString.contains("_")

  case class QName(n:List[String]):
    override def toString = n.mkString("/")
    def show = if n.isEmpty then "-" else toString
    def /(other:QName) = if (other.n.isEmpty) other else if (n.isEmpty) other else QName(n ::: other.n)
    def /(other:String) = QName(n:::List(other))

    def /(e:EdgeMap):EdgeMap = e.map((src, targets) => (this / src) -> targets.map((to, id, lbl) => (this / to, this / id, this / lbl)))
    def /(es:Edges): Edges = es.map((src, to, id, lbl) => (this / src, this / to, this / id, this / lbl))
    def /-(lblsMap:Map[QName,Edges]): Map[QName,Edges] = lblsMap.map((lbl, edges) => (this / lbl) -> (this / edges))
    def /-(ns:Set[QName]): Set[QName] = ns.map(n => this / n)
    def scope: QName = if n.isEmpty then this else QName(n.init)

    def /(rx: RxGraph): RxGraph =
      rx.copy( 
        edg = this / rx.edg,
        on = this / rx.on,
        off = this / rx.off,
        lbls = this /- rx.lbls,
        inits = this /- rx.inits,
        act = this / rx.act,
        val_env = rx.val_env.map { case (k, v) => (this / k) -> v },
        zone = rx.zone,
        functions = rx.functions.map { case (k, f) => 
          (this / k) -> FunctionDef(this / f.name, f.params, f.body.map(s => applyPrefixToStatement(this, s))) 
        },
        edgeConditions = rx.edgeConditions.map { case (edge, condOpt) =>
          (this / edge._1, this / edge._2, this / edge._3, this / edge._4) -> condOpt.map(c => applyPrefixToCondition(this, c))
        },
        edgeUpdates = rx.edgeUpdates.map { case (edge, stmtList) =>
          (this / edge._1, this / edge._2, this / edge._3, this / edge._4) -> stmtList.map(stmt => applyPrefixToStatement(this, stmt))
        },
        clocks = rx.clocks.map(this / _),
        clock_env = rx.clock_env.map { case (k, v) => (this / k) -> v },
        invariants = rx.invariants.map { case (k, v) => (this / k) -> applyPrefixToCondition(this, v) },
        delays = rx.delays.map { case (rule, (clock, value)) => (this / rule) -> (this / clock, value) },
        pendingDelays = rx.pendingDelays.map { case (edge, op, clock, value) => ((this / edge._1, this / edge._2, this / edge._3, this / edge._4), op, this / clock, value) }
      )
  
  def applyPrefixToExpr(prefix: QName, expr: UpdateExpr): UpdateExpr = expr match {
    case UpdateExpr.LitInt(_) | UpdateExpr.LitFloat(_) | UpdateExpr.LitBool(_) => expr
    case UpdateExpr.LitArray(elems) => UpdateExpr.LitArray(elems.map(applyPrefixToExpr(prefix, _)))
    case UpdateExpr.Var(q) => UpdateExpr.Var(if(isGlobalControlVar(q)) q else prefix / q)
    case UpdateExpr.ArrayAccess(arr, idx) => UpdateExpr.ArrayAccess(if(isGlobalControlVar(arr)) arr else prefix / arr, applyPrefixToExpr(prefix, idx))
    case UpdateExpr.MathOp(l, op, r) => UpdateExpr.MathOp(applyPrefixToExpr(prefix, l), op, applyPrefixToExpr(prefix, r))
    case UpdateExpr.FuncCall(f, args) => UpdateExpr.FuncCall(if(isGlobalControlVar(f)) f else prefix / f, args.map(applyPrefixToExpr(prefix, _)))
  }

  def applyPrefixToCondition(prefix: QName, cond: Condition): Condition = {
    cond match {
      case Condition.AtomicCond(left, op, right) => Condition.AtomicCond(applyPrefixToExpr(prefix, left), op, applyPrefixToExpr(prefix, right))
      case Condition.And(l, r) => Condition.And(applyPrefixToCondition(prefix, l), applyPrefixToCondition(prefix, r))
      case Condition.Or(l, r) => Condition.Or(applyPrefixToCondition(prefix, l), applyPrefixToCondition(prefix, r))
    }
  }

  def applyPrefixToStatement(prefix: QName, stmt: Statement): Statement = {
    stmt match {
      case AssignStmt(variable, expr) =>
        val newVar = if (isGlobalControlVar(variable)) variable else prefix / variable
        AssignStmt(newVar, applyPrefixToExpr(prefix, expr))
      case ArrayAssignStmt(arrName, index, expr) =>
        val newArr = if (isGlobalControlVar(arrName)) arrName else prefix / arrName
        ArrayAssignStmt(newArr, applyPrefixToExpr(prefix, index), applyPrefixToExpr(prefix, expr))
      case IfThenStmt(cond, thenStmts) =>
        IfThenStmt(applyPrefixToCondition(prefix, cond), thenStmts.map(s => applyPrefixToStatement(prefix, s)))
      case ForeachStmt(iter, arr, body) =>
        val newIter = if (isGlobalControlVar(iter)) iter else prefix / iter
        val newArr = if (isGlobalControlVar(arr)) arr else prefix / arr
        ForeachStmt(newIter, newArr, body.map(s => applyPrefixToStatement(prefix, s)))
      case ReturnStmt(expr) => ReturnStmt(applyPrefixToExpr(prefix, expr))
      case PrintStmt(expr) => PrintStmt(applyPrefixToExpr(prefix, expr))
    }
  }

  type Edge = (QName, QName, QName, QName)
  type Edges = Set[Edge]
  type EdgeMap = Rel[QName, (QName, QName, QName)]

  def showEdge(e: Edge): String = {
    val (from, to, transId, label) = e
    if (transId == label) s"${from.show} ---> ${to.show} : ${label.show}"
    else s"${from.show} -${transId.show}-> ${to.show} : ${label.show}"
  }
  
  def showEdges(abc:Edges): String = abc.map(showEdge).mkString(", ")

  private def showEdgeMap(abc:EdgeMap): String =
    val es = for (a, bcs) <- abc.toSet; (b, id, lbl) <- bcs yield (a, b, id, lbl)
    showEdges(es)

  case class RxGraph(edg:EdgeMap,
                     on:EdgeMap, off: EdgeMap,
                     lbls: Map[QName,Edges],
                     inits: Set[QName],
                     act: Edges,
                     val_env: Map[QName, RuntimeValue],
                     zone: DBM.Zone, 
                     functions: Map[QName, FunctionDef],
                     clocks: Set[QName], 
                     clock_env: Map[QName, Double],
                     invariants: Map[QName, Condition],
                     edgeConditions: Map[Edge, Option[Condition]], 
                     edgeUpdates: Map[Edge, List[Statement]],
                     delays: Map[QName, (QName, Double)] = Map.empty,
                     pendingDelays: Set[(Edge, String, QName, Double)] = Set.empty
                    ):

    override def toString: String =
      s"""[init]  ${inits.mkString(",")}
         |[clocks] ${clock_env.map(kv => s"${kv._1}=${kv._2}").mkString(", ")}
         |[vars]  ${val_env.map(kv => s"${kv._1}=${kv._2.value}").mkString(", ")}
         |[act]   ${showEdges(act)}
         |[edges] ${showEdgeMap(edg)}
         |[on]    ${showEdgeMap(on)}
         |[off]   ${showEdgeMap(off)}
         |[pending] ${pendingDelays.map(p => s"${p._1._4.show}(${p._2})@${p._3}=${p._4}").mkString(", ")}"""
    
    def states = for (src, dests) <- edg.toSet; (d, _, _) <- dests; st <- Set(src, d) yield st

    def addClock(name: QName) = {
      val newClocks = clocks + name
      this.copy(
        clocks = newClocks, 
        clock_env = clock_env + (name -> 0.0), 
        zone = DBM.initial(newClocks)
      )
    }
    def addInvariant(state: QName, cond: Condition) = this.copy(invariants = invariants + (state -> cond))
    def addDelay(rule: QName, clock: QName, value: Double) = this.copy(delays = delays + (rule -> (clock, value)))

    def addEdge(s1:QName, s2:QName, transId:QName, label:QName, cond: Option[Condition] = None, upd: List[Statement] = Nil) = {
      val edge: Edge = (s1, s2, transId, label)
      this.copy(edg = add(s1 -> (s2, transId, label), edg), lbls = add(label -> edge, lbls), act = act + edge, edgeConditions = edgeConditions + (edge -> cond), edgeUpdates = edgeUpdates + (edge -> upd))
    }

    def addOn(s1: QName, s2: QName, transId: QName, label: QName, cond: Option[Condition] = None, upd: List[Statement] = Nil) = {
      val edge: Edge = (s1, s2, transId, label)
      this.copy(on = add(s1 -> (s2, transId, label), on), lbls = add(label -> edge, lbls), act = act + edge, edgeConditions = edgeConditions + (edge -> cond), edgeUpdates = edgeUpdates + (edge -> upd))
    }

    def addOff(s1: QName, s2: QName, transId: QName, label: QName, cond: Option[Condition] = None, upd: List[Statement] = Nil) = {
      val edge: Edge = (s1, s2, transId, label)
      this.copy(off = add(s1 -> (s2, transId, label), off), lbls = add(label -> edge, lbls), act = act + edge, edgeConditions = edgeConditions + (edge -> cond), edgeUpdates = edgeUpdates + (edge -> upd))
    }

    def deactivate(s1:QName, s2:QName, tId:QName, l:QName) = this.copy(act = act - ((s1, s2, tId, l)))
    def addInit(s:QName) = this.copy(inits = inits + s)
    def addVariable(name: QName, value: RuntimeValue) = this.copy(val_env = val_env + (name -> value))

    def ++(r:RxGraph) =
      RxGraph(
        join(edg,r.edg),
        join(on,r.on),
        join(off,r.off),
        join(lbls,r.lbls),
        inits++r.inits,
        act++r.act,
        val_env ++ r.val_env,
        zone, // Mantemos a zona base (idealmente a união dos clocks)
        functions ++ r.functions,
        clocks ++ r.clocks,
        clock_env ++ r.clock_env,
        invariants ++ r.invariants,
        edgeConditions ++ r.edgeConditions,
        edgeUpdates ++ r.edgeUpdates,
        delays ++ r.delays,
        pendingDelays ++ r.pendingDelays
      )

  object RxGraph: 
    def apply(): RxGraph = RxGraph(
      edg = Map().withDefaultValue(Set()), 
      on = Map().withDefaultValue(Set()), 
      off = Map().withDefaultValue(Set()),
      lbls = Map().withDefaultValue(Set()), 
      inits = Set(), 
      act = Set(),
      val_env = Map(), 
      zone = DBM.initial(Set()),
      functions = Map(),
      clocks = Set(), 
      clock_env = Map(), 
      invariants = Map(),
      edgeConditions = Map().withDefaultValue(None), 
      edgeUpdates = Map().withDefaultValue(Nil),
      delays = Map(), 
      pendingDelays = Set()
    )

    def toMermaid(rx: RxGraph): String =
      var i = -1
      def fresh(): Int = {i += 1; i}
      s"flowchart LR\n${
        drawEdges(rx.edg, rx, fresh, ">", "stroke:black, stroke-width:2px",(x,y) => Set(x.toString), withConditions = true)}${
        drawEdges(rx.on, rx, fresh, ">", "stroke:blue, stroke-width:3px",getLabel, withConditions = true)}${
        drawEdges(rx.off,rx, fresh, "x", "stroke:red, stroke-width:3px",getLabel, withConditions = true)}${
        (for s<-rx.inits yield s"  style $s fill:#8f7,stroke:#363,stroke-width:4px\n").mkString
      }"

    def toMermaidPlain(rx: RxGraph): String =
      var i = -1
      def fresh(): Int = {i += 1; i}
      s"flowchart LR\n${drawEdges(rx.edg, rx, fresh, ">", "stroke:black, stroke-width:2px",(x,y) => Set(x.toString),simple=true, withConditions = false)}${(for s<-rx.inits yield s"  style $s fill:#8f7,stroke:#363,stroke-width:4px\n").mkString }"

    private def cleanId(a: Any, b: Any, id: Any, lbl: Any): String = s"$a$b$id$lbl".replaceAll("[^a-zA-Z0-9]", "")
    private def getLabel(n: QName, rx: RxGraph): Set[String] = for (edge <- rx.lbls.getOrElse(n, Set())) yield cleanId(edge._1, edge._2, edge._3, edge._4)

    private def drawEdges(es: EdgeMap, rx: RxGraph, fresh: () => Int, tip: String, style: String, getEnds: (QName, RxGraph) => Set[String], simple: Boolean = false, withConditions: Boolean = false): String =
      (for
        (a, bs) <- es.toList
        (b, transId, lbl) <- bs.toList
        a2 <- getEnds(a, rx).toList
        b2 <- getEnds(b, rx).toList
      yield
        val edge: Edge = (a, b, transId, lbl)
        val isGloballyActive = rx.act(edge)
        val isConditionSatisfied = rx.edgeConditions.getOrElse(edge, None).forall(c => RxSemantics.evalCondition(c, rx))
        val line = if (isGloballyActive && isConditionSatisfied) then "---" else "-.-"

        val qNameLabel = if transId == lbl then lbl.show else s"${lbl.show}(${transId.show})"
        val updText    = if withConditions then rx.edgeUpdates.getOrElse(edge, Nil).map(_.toString).mkString(" ") else ""
        val condText   = if withConditions then rx.edgeConditions.getOrElse(edge, None).map(_.toMermaidString).getOrElse("") else ""
        
        val combined   = List(condText, qNameLabel, updText).filter(_.nonEmpty).mkString(" ")
        val edgeLabel = if combined.nonEmpty then s"|\"${combined}\"|" else ""

        if lbl.n.isEmpty && transId.n.isEmpty then s"  $a2 $line$tip $edgeLabel $b2\n  linkStyle ${fresh()} $style\n"
        else if simple then s"  $a2 $line$tip $edgeLabel $b2\n  linkStyle ${fresh()} $style\n"
        else
          val anchorId = cleanId(a, b, transId, lbl)
          s"  $a2 $line $anchorId( ) $line$tip $edgeLabel $b2\n  style $anchorId width: 0\n  linkStyle ${fresh()} $style\n  linkStyle ${fresh()} $style\n"
      ).mkString