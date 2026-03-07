# Resumo
O modelo utiliza invariantes para simular janelas de atenção do utilizador e hiper-arestas para mimetizar um sistema de recomendação dinâmico, onde o comportamento passado (Like/DwellTime) dita quais as funcionalidades de navegação que serão desbloqueadas no futuro.

### **Lógica do Modelo:**

#### **1. O Significado dos Invariantes Temporais**
Neste modelo, os invariantes (`inv`) não são apenas relógios, eles representam as **janelas de oportunidade** ou a **paciência do utilizador** em cada fase:

*   **Paciência de Visualização (`Watch_View: t <= 10`):** Se o utilizador ficar parado a ver um vídeo sem interagir durante 10 segundos, o sistema assume desinteresse total. O invariante força um `timeout` que penaliza o `score`.
*   **Tempo de Decisão (`Watch_React: t <= 4`):** Após demonstrar interesse inicial, o utilizador tem apenas 4 segundos para decidir se faz *Like* ou *DontLike*. Se o tempo acabar, o sistema "fecha a loja" e volta ao Feed.
*   **Carregamento (`Watch_Init: t <= 1`):** Simula o tempo técnico de buffer/abertura. O invariante obriga o sistema a passar para a visualização propriamente dita após 1 segundo.
*   **Navegação Limitada (`Feed` e `List`):** Garante que o utilizador não fique "preso" eternamente num menu, forçando a progressão do fluxo.

#### **2. O Papel das Hiper-arestas (Reconfiguração)**
As hiper-arestas são o "cérebro" do algoritmo de recomendação. Elas não mudam o estado, mudam **as regras do que é possível fazer**:

*   **Recompensa por Interesse (`like` e `dwellTime ->>`):**
    *   Quando o utilizador faz *Like* **OU** simplesmente assiste ao vídeo por tempo suficiente (`dwellTime`), o algoritmo "aprende" que ele gosta do conteúdo.
    *   **Consequência:** São ativadas as funcionalidades de `refresh` (para ver mais coisas parecidas) e o acesso à `List` (uma lista premium/curada), que estavam anteriormente bloqueadas (`disabled`).
*   **Castigo por Rejeição (`dontLike --!`):**
    *   Se o utilizador reagir negativamente, o algoritmo aplica uma "limpeza" imediata.
    *   **Consequência:** As hiper-arestas de desativação removem o acesso ao `refresh` e à `List`, "escondendo" estas opções do utilizador como resposta ao seu feedback negativo.

#### **3. Dinâmica de Pontuação (Score)**
O `score` atua como um filtro de qualidade. Mesmo que o algoritmo ative a transição para a lista especial (`watchLike`), o utilizador só consegue atravessá-la se o seu `score` for alto (>= 60). 
*   Assistir passivamente (DwellTime) ativa a função, mas só interações positivas (Like) garantem os pontos necessários para entrar na Lista.
*   Ignorar o conteúdo (Timeout) é visto como um sinal negativo silencioso, reduzindo o score.



# Codigo RTA
```RTA
name Like2
init Feed

int score = 10
clock t

inv Feed:       t <= 5
inv Watch_Init: t <= 1
inv Watch_View: t <= 10
inv Watch_React:t <= 4
inv List:       t <= 8

Feed --> Watch_Init : watch t' := 0
List --> Watch_Init : watch2 t' := 0


Watch_Init --> Watch_View : beginView if (t >= 1) then {
    t' := 0
}

Watch_View --> Watch_React : dwellTime if (t >= 3) then {
    t' := 0
}

Watch_View --> Feed : timeout if (t >= 10) then { 
  if (score >= 5) then {
    score' := score - 5
  }
  t' := 0 
}

Watch_React --> Watch_React : like if (0==0) then {
if (score <= 90) then {
    score' := score + 10
}
t' := 0
}

Watch_React --> Feed : dontLike if (0==0) then {
if (score >= 10) then {
    score' := score - 10
}
t' := 0
}

Watch_React --> Feed : refresh disabled t' := 0

Feed --> List : watchLike disabled if (score >= 60) then {
    t' := 0
}


like ->> refresh : lr
like ->> watchLike : lw
dwellTime ->> refresh : dr
dwellTime ->> watchLike : dwll

dontLike --! watchLike : dww
dontLike --! refresh : dref
```