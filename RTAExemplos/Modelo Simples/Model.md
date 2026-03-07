# Resumo
O modelo LikeAlgorithm demonstra como as hiper-arestas transformam um grafo estático num sistema adaptativo: o 'Like' atua como uma chave que desbloqueia novas rotas (Refresh/List), enquanto o 'DontLike' funciona como um trinco que as remove, garantindo que a interface reflete o interesse do utilizador em tempo real.


### **Lógica do Modelo: LikeAlgorithm (Controlo de Fluxo e Engajamento)**

#### **1. O Conceito de Funcionalidade sob Condição**
Neste modelo, o sistema começa de forma restrita. As opções de **`refresh`** (no estado *Watch*) e **`watchLike`** (no estado *Feed*) estão marcadas como `disabled`. Isto significa que, inicialmente, o utilizador tem uma interface limitada. Estas funcionalidades só aparecem como recompensa por comportamentos específicos.

#### **2. O Papel das Hiper-arestas (Reconfiguração Dinâmica)**
As hiper-arestas aqui funcionam como um **motor de regras** que altera a interface com base no histórico imediato do utilizador:

*   **O "Poder" do Like (`like ->> ...`):**
    *   O ato de fazer *Like* é o gatilho principal de **recompensa**. Ele ativa simultaneamente o `refresh` e o acesso à `List` (`watchLike`). 
    *   **Exclusão de Conflito (`like --! dontLike`):** Uma vez que o utilizador gosta do conteúdo, a hiper-aresta de desativação remove a opção de `dontLike`. Isto impede que o utilizador envie sinais contraditórios para o algoritmo na mesma sessão de visualização.
*   **Sanção por Rejeição (`dontLike --! watchLike`):**
    *   Se o utilizador decidir que não gosta do conteúdo, o sistema retira-lhe imediatamente o acesso à lista especial (`watchLike`). O feedback negativo atua como um filtro que "esconde" funcionalidades de exploração profunda.

#### **3. Ciclo de Estados e Navegação**
O modelo define um fluxo circular onde as ações de feedback (Like/DontLike) devolvem o utilizador ao ponto de partida, mas com o grafo reconfigurado:
*   **Feed -> Watch:** Ação de entrada.
*   **Watch -> Watch:** O *Like* permite interação contínua sem sair do conteúdo.
*   **List -> Watch:** Uma vez desbloqueada a lista, ela serve como um novo ponto de entrada para visualização (`watch2`).


# Codigo RTA

```RTA
name LikeAlgorithm
init Feed
Feed --> Watch: watch
Watch --> Watch: like
Watch --> Feed: dontLike
Watch --> Feed: refresh disabled
Feed --> List: watchLike disabled
List --> Watch: watch2
watch ->> dontLike: wd
like --! dontLike: ld
like ->> refresh: lr
like ->> watchLike: lw
dontLike --! watchLike: dw
```