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