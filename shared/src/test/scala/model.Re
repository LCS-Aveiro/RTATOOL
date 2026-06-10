name ImprovedDelayTest
init s0
clock t, g

// --- CONFIGURAÇÃO DOS DELAYS ---
// Usamos 'g' porque ele nunca reseta, garantindo que o delay 
// de 2 segundos seja contado corretamente independente das transições.
delay offA: g, 2
delay onA: g, 3

// --- INVARIANTES ---
// Forçamos o sistema a sair de cada estado em no máximo 5 segundos
inv s0: t <= 5
inv s1: t <= 5

// --- ARESTAS COM GUARDAS E RESETS ---
// 'a' só pode ocorrer após 1s (t >= 1) e reseta o relógio 't'
s0 ---> s1 : a if t >= 1 then {
  t' := 0
}

// 'b' também só ocorre após 1s e reseta 't'
s1 ---> s0 : b if t >= 1 then {
  t' := 0
}

// --- HYPEREDGES ---
// Note que as hyperedges usam as definições de delay baseadas em 'g'
a --! a : offA
b ->> a : onA