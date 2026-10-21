# Como o motor da RTA Tool constrói o grafo de zonas

Este texto explica, de forma completa e passo a passo, como o motor da RTA Tool pega num modelo com relógios, guardas, invariantes, hiper-arestas e regras com atraso, e constrói o chamado grafo de zonas: o mapa de tudo o que o sistema pode fazer, que depois é usado para fazer provas.

A explicação usa um modelo pequeno, em duas versões: primeiro sem atraso na regra, depois com atraso. Todos os estados e intervalos de tempo que aparecem aqui foram calculados pelo próprio motor.

---

## 1. O modelo de exemplo

O modelo tem dois estados, chamados `a` e `b`, e um relógio chamado `x`.

```
init a
clock x
inv a: x <= 3
inv b: x <= 3

a ---> b: ab if (x >= 1) then { x' := 0 }
b ---> a: ba x' := 0

ba --! ab: off
delay off: 2.0
```

Lê-se assim.

O sistema começa no estado `a`.

Há um relógio, `x`, que começa a zero e avança com o tempo.

Os dois estados têm uma invariante: não se pode ficar em `a` nem em `b` com o relógio acima de 3. Antes disso, é obrigatório sair.

Há duas transições. A transição `ab` leva de `a` para `b`, mas só pode acontecer quando o relógio já vai em pelo menos 1, e põe o relógio de novo a zero. A transição `ba` leva de `b` para `a`, pode acontecer a qualquer momento, e também põe o relógio a zero.

Por fim, há uma hiper-aresta, ou seja, uma regra, chamada `off`. Diz que, quando `ba` acontece, a transição `ab` é desligada. A última linha diz que esta regra tem um atraso de 2: o desligar não acontece logo, só duas unidades de tempo depois de `ba`.

A pergunta que nos interessa é: como é que o programa pega neste modelo e descobre tudo o que pode acontecer, sabendo ao mesmo tempo dos tempos, das guardas, das invariantes e das hiper-arestas?

---

## 2. A resposta curta

O motor não "abre" primeiro todas as hiper-arestas para depois tratar do tempo. Também não calcula as combinações todas à partida.

O que faz é começar no estado inicial e explorar, uma transição de cada vez, tudo o que pode acontecer a seguir, usando exatamente as mesmas regras que o simulador usa quando se clica nos botões. Cada vez que encontra uma situação nova, guarda-a como um nó do grafo. Quando já não aparece nenhuma situação nova, o grafo está completo.

É por isso que o motor "sabe" das hiper-arestas: vai fazendo correr o sistema, e as regras são aplicadas no momento em que a transição que as dispara acontece.

Mas há duas diferenças importantes em relação ao simulador. A primeira é que não segue um único caminho: segue todos os caminhos ao mesmo tempo. A segunda é que não usa valores concretos de tempo, como "esperar 1,6": usa intervalos, como "qualquer tempo entre 0 e 2". Um único nó representa infinitas execuções concretas.

---

## 3. O que é um nó do grafo

Cada nó do grafo guarda quatro coisas, cada uma na sua "gaveta".

A primeira gaveta é o **estado atual**: onde o sistema está, por exemplo `a`.

A segunda gaveta é a **lista das arestas ligadas**: que transições e regras estão disponíveis neste momento. É aqui que vivem as hiper-arestas. Quando uma regra desliga uma transição, o que muda é esta lista.

A terceira gaveta é a **lista de pendentes**, a que chamamos ómega. Guarda as regras com atraso que já foram disparadas mas ainda não fizeram efeito, ou seja, ainda estão à espera.

A quarta gaveta é a **zona**. A zona guarda os valores possíveis dos relógios, sob a forma de intervalos e de relações entre relógios. Por exemplo, "x está entre 0 e 2", ou "x e o relógio do pendente valem sempre o mesmo".

A ideia mais importante deste texto é esta: **a zona só trata do tempo**. A zona não sabe nada de hiper-arestas. Quem sabe das hiper-arestas é a segunda gaveta, a lista de arestas ligadas, e no caso das regras com atraso, a terceira gaveta, a lista de pendentes.

Dois nós são considerados o mesmo nó só se as quatro gavetas forem iguais. Por isso o mesmo estado `a` pode aparecer várias vezes no grafo: são situações diferentes, porque mudou a zona, os pendentes ou as arestas ligadas.

---

## 4. O ciclo de exploração

O motor tem uma fila de nós por explorar. No início, a fila tem só o nó inicial. Depois repete sempre o mesmo ciclo.

Tira um nó da fila.

**Primeiro, deixa o tempo passar** dentro desse nó. Na zona, isto quer dizer que o relógio pode subir. Mas não sem limite: a invariante do estado corta-o, e se houver pendentes, cada pendente também o corta, porque um pendente não pode passar do seu prazo. Quando lá chega, tem de fazer efeito.

**Segundo, vê que transições pode tentar.** Só tenta as que saem do estado atual **e** estão na lista de arestas ligadas. Uma transição desligada nem é tentada.

**Terceiro, aplica a guarda de cada transição** à zona. Se a guarda diz "x pelo menos 1", a zona é cortada nesse ponto. Se a zona ficar vazia, a transição é impossível a partir deste nó.

**Quarto, aplica as hiper-arestas.** Vê que regras são disparadas pela transição. Uma regra sem atraso muda logo a lista de arestas ligadas do nó seguinte: liga ou desliga. Uma regra com atraso não muda nada logo: acrescenta uma entrada à lista de pendentes e acrescenta à zona um relógio novo, a começar em zero, que vai contar o tempo até ao prazo.

**Quinto, aplica os resets**, por exemplo "x passa a zero".

**Sexto, verifica a invariante** do estado de chegada.

O resultado é um nó novo. Se ainda não existia, entra na fila para ser explorado mais tarde. Se já existia, o motor só desenha uma seta até ele.

Além das transições normais, há ainda as **transições de prazo**. Para cada pendente que pode chegar ao fim do seu atraso, há uma transição interna em que o pendente vence: o seu efeito é aplicado à lista de arestas ligadas, a entrada sai da lista de pendentes e o relógio auxiliar desaparece da zona. Na interface, estas transições aparecem com um pequeno relógio e o nome da regra.

Há também uma regra de prioridade: no instante exato em que um pendente vence, ele é aplicado antes de qualquer outra transição. É o mesmo que acontece no simulador.

O ciclo termina quando a fila fica vazia, ou seja, quando já não aparece nenhum nó novo. Em modelos muito grandes termina também quando se atinge um limite de estados, e nesse caso a ferramenta avisa.

---

## 5. Primeira versão: a regra sem atraso

Comecemos pela versão do modelo sem a linha do `delay`. Aqui, a regra `off` desliga `ab` logo que `ba` acontece.

**O primeiro nó** está no estado `a`, com todas as arestas ligadas, sem pendentes, e com o relógio a zero.

O motor deixa o tempo passar: o relógio pode subir até 3, por causa da invariante de `a`. A zona fica "x entre 0 e 3".

Que transições pode tentar? Só `ab`, porque é a única que sai de `a` e está ligada. A guarda de `ab` diz "x pelo menos 1", por isso a zona é cortada: `ab` pode acontecer em qualquer instante entre 1 e 3. A transição `ab` não dispara nenhuma regra. O relógio é reposto a zero. Chegamos ao segundo nó.

**O segundo nó** está em `b`, com tudo ligado e o relógio a zero. O tempo pode passar até 3. A transição `ba` pode acontecer em qualquer momento, e aqui entra a hiper-aresta: `ba` dispara a regra `off`, que não tem atraso, por isso desliga `ab` imediatamente. O relógio volta a zero. Chegamos ao terceiro nó.

**O terceiro nó** está em `a`, com o relógio a zero, mas com `ab` desligada.

Repare-se: o primeiro e o terceiro nó estão ambos em `a` e com o relógio a zero. A zona é igual. O que os distingue é a lista de arestas ligadas: no primeiro, `ab` está ligada; no terceiro, não. É assim que a hiper-aresta aparece no grafo: não mexe na zona, mexe na lista de arestas ligadas, e isso cria um nó diferente.

No terceiro nó, o tempo pode passar até 3, mas não há nenhuma transição ligada a sair de `a`. O sistema fica bloqueado: é um deadlock.

Nesta versão, o grafo tem três nós, em linha: do primeiro para o segundo por `ab`, do segundo para o terceiro por `ba`, e o terceiro é o fim.

---

## 6. Segunda versão: a regra com atraso de 2

Agora a versão completa, com `delay off: 2.0`. A diferença é que a regra `off` não desliga `ab` logo: agenda o desligar para daqui a duas unidades de tempo. O motor gera agora nove nós.

### O início é igual

O primeiro nó está em `a`, tudo ligado, sem pendentes, relógio entre 0 e 3. A transição `ab` leva ao segundo nó, em `b`, com o relógio entre 0 e 3.

### A regra com atraso cria um pendente e um relógio

No segundo nó, quando `ba` acontece, a regra `off` é disparada, mas como tem atraso, não desliga `ab`. Em vez disso, acontecem duas coisas. A lista de pendentes ganha uma entrada: "desligar `ab`, prazo de 2". E a zona ganha um relógio novo, a que vamos chamar d zero, que começa em zero e vai contar o tempo até ao prazo.

Este é o único sítio em que uma hiper-aresta mexe na zona: quando tem atraso, ganha um relógio próprio.

### O pendente limita o tempo

O terceiro nó está em `a`, com tudo ligado e um pendente. À chegada, tanto o relógio `x` como o relógio do pendente valem zero.

Quando o motor deixa o tempo passar, a invariante de `a` diria que `x` pode ir até 3. Mas o pendente não pode passar do prazo, ou seja, o relógio d zero não pode passar de 2. E como `x` e d zero começaram juntos e andam ao mesmo ritmo, são sempre iguais. Por isso `x` também fica limitado a 2. A zona fica "x entre 0 e 2, e d zero igual a x".

É para guardar relações como "estes dois relógios são iguais" que a zona usa uma matriz, chamada matriz de diferenças, e não apenas um intervalo por relógio.

A partir deste nó há dois caminhos.

O primeiro caminho é o pendente vencer: quando d zero chega a 2, `ab` é desligada, o pendente sai da lista e o relógio d zero desaparece. O relógio `x` vale 2. Chegamos a um nó em `a`, com `ab` desligada e sem pendentes. Aí o tempo pode passar até 3, mas não há transições ligadas: é o fim, um deadlock.

O segundo caminho é fazer `ab` antes de o pendente vencer. A guarda exige `x` pelo menos 1, e a regra de prioridade exige que o pendente ainda não tenha chegado a 2. Por isso `ab` só é possível com `x` entre 1 e 2, sem incluir o 2.

### Em `b`, com o pendente ainda a correr

Depois de `ab`, estamos em `b`. O relógio `x` foi reposto a zero, mas o relógio do pendente não: continua a contar e já vai entre 1 e 2. Ao deixar o tempo passar, d zero só pode chegar a 2, e como está pelo menos 1 à frente de `x`, o relógio `x` fica limitado a 1. Ou seja, o pendente vai vencer antes de `x` chegar a 1.

Daqui há dois caminhos.

Se o pendente vencer enquanto estamos em `b`, `ab` fica desligada e chegamos a um nó em `b` com `ab` desligada. Desse nó ainda se pode fazer `ba`, que dispara outra vez a regra `off` e agenda mais um pendente. Esse pendente acaba por vencer, mas como `ab` já estava desligada, nada muda, e chegamos ao nó final.

Se fizermos `ba` antes de o pendente vencer, voltamos a `a` e a regra `off` é disparada outra vez. Agora há **dois pendentes** na lista, cada um com o seu relógio: o primeiro já correu entre 1 e 2, o segundo acabou de começar. A zona passa a ter três relógios mais o relógio fictício zero.

### Dois pendentes: só há uma saída

Neste nó, em `a` com dois pendentes, a única coisa que pode acontecer é o primeiro pendente vencer. Porquê? Para fazer `ab`, o relógio `x` teria de chegar a 1. Mas o primeiro pendente está sempre pelo menos 1 à frente de `x`, por isso, no instante em que `x` chega a 1, o primeiro pendente já chegou a 2. E nesse instante o pendente tem prioridade: `ab` é desligada antes de poder ser feita.

Isto corresponde exatamente ao que se vê no simulador: depois de fazer `ab` e `ba` duas vezes, ao fim de três unidades de tempo a transição `ab` desliga-se e já não dá para a fazer.

Depois de o primeiro pendente vencer, fica o segundo, que também acaba por vencer, e chegamos ao nó final.

### Todos os caminhos acabam no mesmo sítio

Ao todo são nove nós, e todos os caminhos acabam no mesmo nó: estado `a`, `ab` desligada, sem pendentes, relógio entre 2 e 3. É um deadlock: como nenhuma regra volta a ligar `ab`, o sistema fica sempre preso em `a`.

---

## 7. Como ler a imagem do grafo na interface

Na vista "Completo" do botão "Ver Zonas", cada caixa é um nó e mostra o estado, a zona (o intervalo de cada relógio) e os pendentes, com uma ampulheta e o tempo de atraso. As setas têm o nome da transição, e as setas com um pequeno relógio são os prazos dos pendentes.

Há um cuidado a ter: nesta vista, o rótulo não diz se `ab` está ligada ou desligada. Por isso aparecem duas caixas com o texto "estado a, x entre 0 e 2, um pendente off", que parecem iguais mas não são: numa `ab` está ligada e na outra está desligada. Para ver a diferença, pode clicar na caixa, e o painel de detalhes mostra as arestas ligadas e desligadas. Ou pode usar a vista "Por configuração", que escreve "desligadas: ab" na própria caixa.

Os nove nós da imagem são estes, pela ordem em que o motor os descobre:

1. `a`, tudo ligado, sem pendentes: o início.
2. `b`, tudo ligado, sem pendentes: depois de `ab`.
3. `a`, tudo ligado, um pendente: `ba` agendou o desligar.
4. `a`, `ab` desligada, sem pendentes, relógio entre 2 e 3: o fim.
5. `b`, tudo ligado, um pendente: fez-se `ab` antes de o pendente vencer.
6. `a`, tudo ligado, dois pendentes: fez-se `ba` outra vez.
7. `b`, `ab` desligada, sem pendentes: o pendente venceu enquanto se estava em `b`.
8. `a`, `ab` desligada, um pendente: o primeiro de dois pendentes já venceu.
9. `a`, `ab` desligada, um pendente: voltou-se de `b` com `ab` já desligada e foi agendado mais um desligar.

---

## 8. Onde as hiper-arestas e o tempo se encontram

As hiper-arestas e o tempo vivem em gavetas separadas do nó, e só se tocam em três situações.

A primeira é quando uma regra tem atraso: ganha um relógio próprio dentro da zona, que conta o tempo até ela fazer efeito. Quando vence, o relógio desaparece e o efeito é aplicado à lista de arestas ligadas.

A segunda é quando uma regra tem uma guarda que fala de relógios, por exemplo "só desliga se x for pelo menos 4". Como a zona é um intervalo, a regra pode disparar para uns valores e não para outros. O motor parte a zona em duas, a parte em que a regra dispara e a parte em que não dispara, e cada parte dá um nó diferente.

A terceira é quando uma transição está desligada: o motor simplesmente não a tenta, e por isso a zona não chega a ser cortada pela sua guarda.

Fora destas três situações, o tempo e as hiper-arestas não se misturam.

---

## 9. Porque é que o motor não calcula tudo à partida

Poderia pensar-se que o motor primeiro calcula todas as combinações possíveis de arestas ligadas e desligadas, e só depois trata do tempo. Não o faz, e há uma boa razão: com dez arestas haveria mais de mil combinações, e a grande maioria nunca acontece. Explorando a partir do estado inicial, o motor só cria as combinações que aparecem de facto. Neste modelo só aparecem duas: tudo ligado, e `ab` desligada.

---

## 10. Porque é que isto serve para provar propriedades

Cada nó representa infinitas execuções concretas, porque guarda intervalos de tempo e não valores soltos. E o grafo, no fim, é finito: tem nove nós neste exemplo. Por isso, quando uma propriedade é verdadeira em todos os nós alcançáveis, é verdadeira para qualquer escolha de tempos reais. É a diferença entre testar algumas execuções e provar para todas.

Depois de construído, o grafo é um grafo normal, com nós numerados e setas com nome. Os verificadores trabalham sobre ele com algoritmos de grafos. Neste exemplo, a fórmula CTL "é possível chegar a `b`" é verdadeira. A fórmula "a partir de qualquer situação, ainda é possível chegar a `b`" é falsa, porque a partir do nó final nunca mais se chega a `b`.

---

## 11. Resumo para dizer em voz alta

O motor começa no estado inicial e vai explorando todas as transições possíveis, uma de cada vez, com as mesmas regras do simulador.

Cada nó guarda quatro coisas: o estado, as arestas que estão ligadas, as regras com atraso que estão à espera, e a zona, que é o intervalo de tempos possíveis para cada relógio.

A zona trata das guardas, das invariantes e dos resets. As hiper-arestas mudam a lista de arestas ligadas do nó seguinte. Uma regra com atraso é o único caso em que uma hiper-aresta mexe na zona: ganha um relógio próprio, que conta o tempo até ela fazer efeito.

Quando já não aparecem nós novos, temos o mapa completo de tudo o que o sistema pode fazer. Como cada nó cobre infinitas execuções e o mapa é finito, as provas feitas sobre ele valem para qualquer escolha de tempos.
