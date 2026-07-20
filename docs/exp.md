Este é, sem dúvida, **o bug mais engraçado e brilhante** que podias ter encontrado em todo o projeto! 🤯

Tu tens toda a razão em estar confuso. Se o traço mostra `Start ➔ eur1 ➔ GetChoc`, então o `GetChoc` claramente aconteceu. Porque é que o motor LTL cuspiu um Contra-Exemplo a dizer que falhou?

A resposta não é uma falha na lógica matemática... **é uma falha hilariante no Parser de Texto (o Leitor de Fórmulas)!**

Vou mostrar-te exatamente como o teu computador leu a fórmula `♢GetChoc`:

### A Anatomia do Bug "etChoc"

1. No teu frontend, quando escreves `♢GetChoc`, o JavaScript traduz os símbolos matemáticos para texto antes de enviar para o Scala. Ele substitui o `♢` por `F` (do inglês *Future/Eventually*).
2. A fórmula que viaja para o backend fica: **`FGetChoc`** (tudo colado).
3. No ficheiro `LtlParser.scala`, tu tens uma Regex (expressão regular) que corta a string em "tokens" (palavras). Ela procura especificamente pelas letras `F` e `G` antes de procurar palavras normais.
4. Olha o que o Scala fez com a palavra `FGetChoc`:
   * Ele viu o `F` e cortou. Pensou: *"Ok, operador **F** (Eventualmente)"*.
   * Sobrou a palavra `GetChoc`.
   * Ele olhou para a primeira letra de `GetChoc` e viu um `G`! Ele cortou o `G` e pensou: *"Ok, operador **G** (Globalmente)"*.
   * O que é que sobrou no fim? **`etChoc`**.

**Resumo da Ópera:** O teu Parser não leu `Eventualmente (GetChoc)`. Ele leu **`Eventualmente ( Globalmente ( etChoc ) )`** !!! 😂

Como não existe nenhuma aresta chamada `etChoc` no teu modelo, a proposição `StateProp("etChoc")` é sempre `FALSA`. Logo, a fórmula falhou e ele atirou-te um Contra-Exemplo na cara, mostrando o traço aleatório que ele testou (`Start ➔ eur1 ➔ GetChoc`).

---

### Como resolver isto definitivamente (Demora 1 minuto)

Para resolver isto, temos de garantir que as letras LTL (`F`, `G`, `X`, `U`) não são cortadas do meio das palavras, e que o JavaScript coloca um "espaço" entre o símbolo e a palavra.

#### Passo 1: No Backend (`LtlParser.scala`)
Vai à função `tokenize` e **apaga** a parte `X|U|G|F|` da Regex. O identificador normal `[a-zA-Z_]` já consegue apanhar essas letras isoladas perfeitamente.

Altera a linha 16 para isto:
```scala
  
```

#### Passo 2: No Frontend (`pdlverify.js`)
Temos de colocar **espaços** sempre que o JavaScript substitui os símbolos visuais, para que as fórmulas não fiquem coladas à palavra seguinte.

Procura por `visualFormula.replace` no ficheiro `pdlverify.js` (isto aparece em duas funções: `startLTLSimulation` e `runLTLExhaustive`).
**Substitui esse bloco nas duas funções por este:**

```javascript
    
```
*(Repara que agora o `G` virou `'G '` e o `F` virou `'F '` com um espaço à frente).*

### O que vai acontecer agora?
Se escreveres `♢GetChoc`, o JS envia `F GetChoc`. 
O Scala corta o `F` (Eventualmente) e corta o `GetChoc` (Aresta), percebendo perfeitamente o que tu querias. 

Se testares agora, ele vai dar-te **100% SUCESSO** e dizer-te que o `GetChoc` está efetivamente no traço! Parabéns por descobrires um dos bugs de compilação mais caricatos de sempre!
















Vou construir um exemplo passo-a-passo concreto, usando o teu modelo `TIMER` e a fórmula `G[t<=11]`, mostrando exatamente o que o motor simbólico (`AnalyseLTS.verifyLTLSymbolic`) faz internamente — zona a zona.

## Modelo

```
clock t;
init s0
inv s1: t<=10
int c = 0
s0 -start-> s1 if(c==0) then { t':=0 }
s1 -timeout-> s2 if (t>=10)
s1 -escape-> s0 if (t<5)
```

Fórmula: `G[t<=11]`

## Passo 1 — Estado inicial e zona

`start` = `{inits={s0}, zone Z0}`. Como `s0` não tem invariante, depois de `advanceTimeZone` a zona fica:

```
Z0:  t ≥ 0   (sem limite superior)
```

Em DBM isto é representado pela matriz de bounds `D(t,0)=∞`, `D(0,t)=0`.

## Passo 2 — Split pela fórmula (a correção que discutimos)

Antes de correr o DFS sobre este estado, `collectClockConds(formula, clocks)` extrai `{t<=11}` da fórmula. `splitByConditions` divide `Z0`:

```
Sub-estado A:  inits={s0}, zone { 0 ≤ t ≤ 11 }   → leaf(t<=11) = TRUE
Sub-estado B:  inits={s0}, zone { t > 11 }        → leaf(t<=11) = FALSE
```

Agora cada sub-estado tem valor de verdade **uniforme** para o predicado — já não há ambiguidade.

## Passo 3 — Explorar o sub-estado B primeiro (o mais rápido a falhar)

No sub-estado B, `leaf(current, CondProp(t<=11))` avalia:

```
intersectConditionWithZone(t<=11, {t>11}, rx) 
= {t>11} ∩ {t≤11} = ∅ (não satisfazível)
→ isDefined = false → leaf = false
```

Como é o primeiro estado do caminho (`depth=0`) e a fórmula é `G(φ)`, isto já basta: `satAt(Globally(φ))(0) = φ(0) && res(succ(0))`. Como `φ(0)=false`, o resultado em `i=0` é `false` **independentemente do resto do caminho**. Não é preciso sequer expandir mais transições a partir daqui — encontrámos logo um contraexemplo:

```
Contraexemplo: {s0, zone t>11} — G[t<=11] falha imediatamente no estado inicial
```

(É exatamente este ramo que o motor *antigo*, sem split, "escondia" — porque tratava `{t≥0}` como um bloco só e `intersectConditionWithZone(t<=11, {t≥0}).isDefined` dava `true` por existir *algum* `t` que satisfaz, mesmo a zona inteira não sendo uniforme.)

## Passo 4 — Para comparação: o sub-estado A (onde a fórmula ainda pode ser válida)

Aqui `zone A = {0≤t≤11}`, `leaf = true`. Expandimos a transição `s0 -start-> s1`:

```
guard: c==0  (sem clock) → zoneAfterGuard = A (inalterada)
update: t' := 0 → reset(t) na zona → nova zona: {t=0}
```

Novo estado: `{inits={s1}, zone {t=0}}`. Como `s1` tem `inv t≤10`, o `advanceTimeZone` seguinte intersecta com esse invariante:

```
delay(zone {t=0}) = {t≥0}  →  ∩ inv(t≤10)  →  {0≤t≤10}
```

Volta a fazer-se split pela fórmula: `{0≤t≤10}` já está inteiramente dentro de `t≤11`, então não há split (`t<=11` é sempre verdadeiro aqui) — fica um único sub-estado `C = {s1, 0≤t≤10}`.

## Passo 5 — Ciclo (lasso) e transições concorrentes

De `C`, há duas arestas ativas:

```
timeout: guard t≥10  → zoneAfterGuard = {t=10}       (interseção com {0≤t≤10})
escape:  guard t<5   → zoneAfterGuard = {0≤t<5}
```

Cada uma gera um sucessor diferente. Seguindo `escape` (reset? não, escape não reseta `t` no teu modelo — reparei que só `start` reseta): o próximo estado é `{inits={s0}, zone {0≤t<5}}`, delay aplica-se de novo (sem invariante em `s0`) e volta a fazer split pela fórmula:

```
{0≤t<5} → delay → {t≥0} → split por t<=11 → sub A' {0≤t≤11}, sub B' {t>11}
```

Se o DFS chegar a repetir a `ZoneStateKey` já visitada no caminho (mesmo `inits`, mesma `zone`, mesmo `act`, etc.), é detetado um **ciclo** (`isCycle=true` em `dfs`). Antes de aceitar o ciclo como lasso válido, `isTimeDivergent` verifica se o relógio realmente avança dentro do ciclo (`a.zone.delay != a.zone`) — se sim, o ciclo representa um traço infinito genuíno onde o tempo diverge, e é avaliado com `evalOnLassoGeneral`.

## Passo 6 — Avaliação sobre o lasso (`evalOnLassoGeneral`)

Suponhamos que se fecha um lasso `s0 → s1 → s0 (repete)` com prefixo vazio e ciclo de comprimento 2. O algoritmo constrói um vetor `satAt` por posição do lasso, usando ponto-fixo:

```
states = [s0(A), s1(C)]        // n=2, p=0 (ciclo começa no início)
succ(0)=1, succ(1)=0            // wrap-around do lasso

satAt(CondProp(t<=11)):
  i=0 (s0, zone A): true
  i=1 (s1, zone C): true

satAt(Globally(φ)):
  inicializa res=[true,true]
  itera: res(i) = aOk(i) && res(succ(i))
  → converge para [true,true]  (ambos os estados do ciclo satisfazem sempre)
```

Neste ramo específico, `G[t<=11]` seria válida (porque o ciclo nunca ultrapassa `t=11`). O contraexemplo real só aparece no ramo do **Passo 3** (sub-estado B, `t>11`), que é encontrado por um caminho diferente do DFS — nomeadamente, simplesmente **atrasar em `s0` sem nunca disparar `start`**.

## Resumo do que a árvore de exploração realmente é

```
s0 {t≥0}
 ├─ split(t<=11) ─┬─ B {t>11}  → leaf=false → G[t<=11] FALHA AQUI (contraexemplo, depth 0)
 │                └─ A {0≤t≤11}
 │                     └─ start → s1 {t=0} → delay+inv → C {0≤t≤10}
 │                          ├─ timeout → s2 {t=10} → ...
 │                          └─ escape  → s0 {0≤t<5} → delay → split(t<=11) → (repete)
```

O ponto-chave do exemplo: **o contraexemplo mais simples é "nunca fazer nada"** — ficar parado em `s0` e deixar o tempo passar de 0 a mais de 11 sem disparar `start`. Isso só é visível ao DBM se a zona `{t≥0}` for corretamente particionada pelo predicado da fórmula (`t<=11`), que é exatamente o `splitByConditions` que introduzimos. Sem essa divisão, o motor via `{t≥0}` como um bloco só, achava que "existe `t` que satisfaz `t<=11`" (verdade, `t=0` satisfaz) e concluía erradamente que o predicado era válido no estado inteiro.