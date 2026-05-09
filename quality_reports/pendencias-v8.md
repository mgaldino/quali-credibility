# Pendências da v8 — para voltar antes de submeter

**Status**: aberto, em construção ao longo das sessões de revisão
**Última atualização**: 2026-05-09

Lista de pontos identificados durante a edição da v8 que precisam de retrabalho posterior, em geral porque o conserto exige reflexão substantiva e não é mecânico.

## P-V8-1: Jargão "U" introduzido sem scaffolding na introdução

**Status**: RESOLVIDO em 2026-05-09 (uncommitted; opção 1 adotada — substituir $U$ por linguagem em prosa, preservando consistência com §4 onde $U$ é formalizado como notação técnica). Aplicado em DOIS lugares: (i) introdução linha 43, "qualquer $U$ é livremente conjeturável" → "qualquer confundidor desse tipo é livremente conjeturável"; (ii) abstract — apontado pelo autor durante a sessão como instância adicional —, "deixa de ser 'e se houver um U?'" → "deixa de ser 'e se houver um confundidor não considerado?'". Pontuação ajustada também na intro para fluência (separar período longo em dois).

**Local**: Introdução, parágrafo da Camada 2 da contribuição (atualmente em `paper_dados_format_quali.Rmd`, ~linha 38).

**Trecho problemático** (estado atual):

> "A objeção 'e se houver um confundidor não-observado?', manobra padrão da crítica de credibilidade, perde poder discriminatório em quali de $n$ pequeno quando formulada de modo abstrato: postulado sem articulação como explicação rival concreta com implicações empíricas próprias, qualquer $U$ é livremente conjeturável e não distingue um estudo bom de um ruim."

**Problema**: a notação $U$ aparece pela primeira vez aqui, sem definição. O leitor da introdução não sabe o que $U$ representa. No texto, $U$ é definido apenas mais à frente (§4, "variável não-observada que confunde o efeito de $X$ sobre $Y$") — anacrônico e jargão técnico injetado cedo demais.

**Direção provável da solução** (sem decidir ainda):
1. Substituir $U$ por "confundidor" ou "variável não-observada" no texto da intro, sem notação matemática.
2. OU introduzir $U$ explicitamente uma frase antes ("para uma variável não-observada $U$ que possa confundir o efeito de interesse...").
3. OU reescrever a frase inteira sem o jargão técnico, focando na lógica argumentativa.

A escolha depende de quanto a intro deve ser técnica vs. acessível. Decidir junto com a revisão geral da introdução.

**Notas para o retrabalho**:
- Verificar se o mesmo problema aparece em outros pontos da intro (não só esta frase).
- A intro deve ser legível por leitor de BPSR sem treinamento em DAG/Pearl/Rubin.
- Mas tampouco deve infantilizar — o leitor é metodologicamente alfabetizado.
- Risco: se reformular para "confundidor", precisa ser consistente com §4 onde a notação $U$ é explícita. Não pode ser "intro fala em confundidor; §4 introduz $U$ como se fosse notação nova" — é a mesma coisa, marcada de modos diferentes.

## P-V8-2: Heterogeneidade de efeitos + IBE como critério de validade interna do quali (refinamento substantivo de tese)

**Status**: RESOLVIDO em 2026-05-09 (uncommitted; quatro inserções/cortes em §3.1, §4, §Transportabilidade, §Considerações Finais; PDF compila via xelatex)
**Identificado em**: 2026-05-09 (sessão de edição com autor)
**Prioridade**: alta — afeta a tese e várias passagens do texto

### Resumo do que foi feito (2026-05-09)

- **§3.1**: parágrafo das três perspectivas (deterministic-vs-probabilistic) cortado e substituído por uma frase curta sobre heterogeneidade como pressuposto operativo do PO. Linha "O ATE é simplesmente a média..." consolidada com a frase seguinte (eliminada redundância gerada pelo corte).
- **§4 (abertura)**: inserido parágrafo enquadrando a contribuição em termos de validade — heterogeneidade, transportabilidade não-trivial, e validade interna no quali pequeno-n como propriedade do procedimento IBE (não da profundidade descritiva). Cita @Pearl_Bareinboim_2011. Referente "essas tecnologias" no parágrafo seguinte ajustado para "as tecnologias de identificação descritas no parágrafo de abertura" (evitando ambiguidade pós-inserção).
- **§Transportabilidade**: parágrafo novo refutando explicitamente a tese de "validade interna intrínseca por densidade da reconstrução" (cita @Sposito_etal_2022 como instância na literatura metodológica contemporânea). Reafirma validade interna como propriedade do procedimento IBE: exaustividade da enumeração, qualidade das verossimilhanças, magnitude dos *posterior odds*.
- **§Considerações Finais**: parágrafo síntese punctuando que a tese clássica (validade interna por construção via profundidade) opera num enquadramento que antecede a separação identificação/inferência; substituída pela comparação IBE.

### O lugar-comum a evitar

A literatura metodológica qualitativa (Beach-Pedersen, Gerring, Mahoney, Goertz; Sposito et al. 2022 no BR) frequentemente defende a validade interna do estudo de caso por uma cadeia argumentativa específica:

1. "Causalidade pode ser determinística (mesma combinação de causas → mesmo resultado em casos similares)"
2. "Estudo de caso aprofundado, ao reconstruir essa relação determinística, alcança validade interna intrínseca"
3. "O custo é a validade externa — a generalização para outros casos é difícil"
4. "O paradigma PO acomoda essa ontologia ao distinguir efeitos heterogêneos (com causas determinísticas) de efeitos homogêneos (estocásticos)"

**Esta cadeia é folclore metodológico do quali e é falsa.** O autor identifica todo esse cluster como confusão a ser dissipada, não preservada. Agentes que escrevem sobre metodologia quali tipicamente reproduzem esses passos como se fossem ortodoxia legítima — não são, do ponto de vista deste paper.

### A formulação correta (a tese da v8)

1. **Efeitos causais são heterogêneos no PO moderno.** Cada unidade pode ter $Y(0)$ e $Y(1)$ próprios; o estimando médio (ATE) é uma média populacional. Ponto. Não há necessidade de invocar a dicotomia "causalidade determinística vs. probabilística" — é distração ontológica.

2. **Heterogeneidade implica validade externa não-trivial.** O efeito identificado para uma unidade (ou subpopulação) não se transporta automaticamente para outra. Esse é o problema real de transportabilidade que Pearl-Bareinboim, F&C etc. discutem. **Esta é a única implicação genuína da heterogeneidade para o debate quali-quanti.**

3. **Heterogeneidade NÃO implica validade interna intrínseca.** A presença de confundidor omitido continua possível em qualquer estudo de caso. Profundidade de análise não é proxy para identificação. A análise mais profunda pode ainda confundir $X$ com $U$ se $U$ não foi considerado.

4. **A solução do paper**: validade interna no quali é **condicional** à comparação Bayesiana de explicações rivais sob IBE. O critério é "qual a melhor explicação dado o conjunto de rivais considerado?" — não "a explicação que o pesquisador construiu cuidadosamente é a verdade pelo simples fato de ter sido construída cuidadosamente". Isso transforma validade interna de propriedade intrínseca do desenho em propriedade do **procedimento de comparação de rivais**.

5. **Conexão com Spirling-Stewart 2025**: SS argumentam que mesmo regressões com identificação CR-rigorosa precisam de IBE para o passo "do parâmetro identificado para a explicação teórica". No quanti, IBE complementa CR (CR resolve identificação, IBE resolve ponte para teoria). No quali pequeno-n, IBE substitui CR (não há infraestrutura de design para resolver identificação separadamente; IBE faz os dois passos juntos). É o adendo distintivo da v8.

### Implicações para o texto atual

**§3.1 Identificação Causal** — parágrafo "três perspectivas sobre de onde vem a aleatoriedade" (atualmente linhas ~67-71): é digressão ontológica herdada do v7 que reproduz a moldura "causalidade pode ser determinística ou probabilística" como se fosse importante para o argumento. **Cortar inteiro e substituir** por uma frase curta sobre heterogeneidade de efeitos como pressuposto operativo do PO — sem entrar em ontologias de aleatoriedade.

**§3 ou §4** — incluir parágrafo explícito articulando: "Efeitos causais são heterogêneos. Heterogeneidade cria o problema de validade externa (extrapolação não-trivial entre unidades/contextos) mas não resolve o problema de validade interna — confundidor omitido pode existir em qualquer estudo. A solução do quali pequeno-n para validade interna não é a profundidade da análise, mas a comparação IBE de explicações rivais."

**§6 Transportabilidade** — texto atual diz: "Aplicado esse vocabulário, a validade interna de um estudo qualitativo de $n$ pequeno é avaliada via a infraestrutura de credibilidade própria". Está parcialmente certo, mas **falta a refutação explícita da tese clássica**. Adicionar parágrafo refutando explicitamente "validade interna intrínseca por profundidade de análise" como artefato da confusão deterministic/probabilistic, e reafirmando que validade interna no quali é propriedade do procedimento IBE, não do desenho per se.

**§10 Considerações Finais** — incluir a versão sintética. Algo como: "a tese clássica de que estudo de caso aprofundado tem validade interna por construção é uma sobra do enquadramento pré-credibility-revolution. O paradigma PO + IBE substitui essa propriedade intrínseca por um procedimento de comparação cuja credibilidade depende da exaustividade do conjunto de rivais e da qualidade das verossimilhanças."

### Formulações que o agente NÃO deve repetir (lista negra)

Qualquer agente futuro que produzir texto contendo qualquer das seguintes formulações sem qualificação explícita está repetindo lugar-comum que esta nota ataca:

- "Causalidade pode ser determinística OU probabilística" como dicotomia substantiva relevante
- "Quali tem validade interna por análise aprofundada"
- "Estudo de caso aprofundado dá certeza causal sobre o caso estudado"
- "Tradeoff genuíno entre validade interna e validade externa"
- "O paradigma PO acomoda a causalidade determinística do quali"
- "O quali pode generalizar para 'universo causal homogêneo'" (artefato de redefinição de escopo, à la F&C; já criticado em §6)
- "Process tracing aprofundado resolve o problema de identificação" (não resolve sem comparação de rivais)

Se o agente *deve* mencionar alguma dessas para descrevê-la criticamente (como objeto a ser refutado), tem que vir acompanhada da refutação imediata.

---

## P-V8-3: Recontextualização de Sposito et al. (2022) em §2 — instância concreta de P-V8-2

**Status**: RESOLVIDO em 2026-05-09 (uncommitted; parágrafo novo na §2 entre P-V8-6 e o parágrafo de ausência Bayesiana, abordando: tradeoff falso VI/VE, "universo causal homogêneo" como redefinição de escopo (paralelo F&C scope-shifting), "lógica determinista no grupo restrito" como folclore deterministic-vs-probabilistic, dissolução da tipologia EQ/PE/TC sob PO+IBE como vestidos da mesma máquina inferencial). Confusões #1 e #2 da lista original (tradeoff falso e profundidade-como-VI) foram cobertas anteriormente em §Transportabilidade via P-V8-2; #5 (tipologia como artefato) foi tocada por P-V8-6 e agora aprofundada explicitamente; #3 e #4 entram aqui pela primeira vez.
**Identificado em**: 2026-05-09
**Depende de**: P-V8-2 (mesmo diagnóstico, aplicação BR-específica)

### Estado atual do tratamento em §2

A §2 (Recepção brasileira) cita @Sposito_etal_2022 como "avanço parcial" (Tipo B):

> "@Sposito_etal_2022 organizam seleção de casos em torno de tipos de alegação causal — probabilística, mecanística, conjuntista —, um movimento explicitamente *design-first* na linha do que a revolução da credibilidade faz no quanti, mas sem mobilizar o vocabulário formal Pearl/Rubin nem a comparação Bayesiana de rivais."

Está correto até onde vai, **mas é incompleto**. A análise do texto deles revela problemas conceituais que reforçam o argumento da v8 sobre gap arquitetural.

### Trecho-chave de Sposito et al. (2022)

> "Outra virtude correspondente à metodologia qualitativa é a validade interna dos resultados, já que a análise aprofundada permite diminuir as incertezas referentes à relação causal identificada nos casos analisados (Gerring, 2011, p. 1145). A validade interna vem às custas da validade externa, que se trata da generalização dos resultados da pesquisa a outros casos, uma das virtudes das pesquisas quantitativas. Para diminuir essa desvantagem, o pesquisador deve identificar o que o caso selecionado representa dentro de um universo causal homogêneo do qual faz parte, que é um grupo de casos que têm em comum um resultado de interesse, uma relação entre variáveis, uma explicação teórica ou, principalmente, uma relação causal. Dentro desse grupo homogêneo é possível generalizar a relação causal identificada, tendo uma lógica determinista dentro do grupo restrito de casos (Beach & Pedersen, 2016, p. 50-53)."

### Quatro confusões no trecho (todas instâncias de P-V8-2)

1. **Tradeoff validade interna ↔ validade externa**: apresentado como tradeoff genuíno. No PO, são propriedades distintas, não-substituíveis. O tradeoff é falso.

2. **"Análise aprofundada permite diminuir as incertezas referentes à relação causal identificada nos casos analisados"** = informalidade campbelliana via Gerring 2011. Não é validade interna no sentido PO; é redução de incerteza inferencial via maior peso evidencial por caso. Mas redução de incerteza não dá identificação se o desenho não identifica.

3. **"Universo causal homogêneo dentro do qual generaliza com lógica determinista"**: redefinição de escopo no estilo F&C que a §6 da nota já critica. Se o "universo homogêneo" é definido pelo achado causal, a generalização vira tautológica.

4. **"Lógica determinista dentro do grupo restrito de casos"**: invoca a moldura deterministic-vs-probabilistic que P-V8-2 identifica como folclore quali a ser deixado de lado.

### Proposta de reformulação em §2

Recontextualizar Sposito et al. com mais profundidade — não só "fizeram parte do movimento design-first em seleção de casos mas faltou Pearl/Rubin", mas "fizeram parte do movimento design-first em seleção de casos, mas mantiveram a moldura validade-interna/externa pré-credibility-revolution e a redefinição de escopo via 'universo causal homogêneo' que o framework PO-IBE da nota dispensa". Isso fortalece o argumento de gap arquitetural: não é só vocabulário Pearl/Rubin que falta, é a estrutura conceitual.

### Para a §6 (Transportabilidade)

A crítica de F&C scope-shifting na §6 atual é análoga à crítica de "universo causal homogêneo" em Sposito. **Considerar fazer essa simetria explícita**: ambos são instâncias do mesmo padrão de redefinição de escopo para preservar generalização.

### Quinta confusão: a tipologia EQ/PE/TC é ela mesma artefato da folclore deterministic-vs-probabilistic

Sposito et al. (2022), seguindo @Koivu_Damman_2015 (a verificar bib), organizam o quali em três famílias de "alegações de causalidade":

- **EQ — Emulação Quantitativa**: lógica probabilística
- **PE — Pragmatismo Eclético**: lógica mecanística
- **TC — Teoria de Conjuntos**: lógica determinística/conjuntista

A apresentação dos três como tradições com "lógicas de causação" próprias **pressupõe que há ontologias causais distintas no quali** — exatamente o folclore que P-V8-2 identifica como confusão a ser dissipada.

**Diagnóstico sob PO+IBE — o ponto sharp**:

Sob o framework PO+IBE da nota, **não há diferença substantiva entre EQ, PE e TC**. As três famílias podem todas:
- adotar a mesma ontologia (efeitos potenciais heterogêneos com funções estruturais determinísticas por unidade)
- usar Bayes
- ter mecanismos especificados na função estrutural
- usar variáveis binárias
- usar lógica conjuntiva-disjuntiva (estilo INUS)

EQ, PE e TC **não diferem em ontologia, maquinaria, ou estrutura conceitual**. Diferem em: vocabulário preferido por cada tradição (probabilístico, mecanístico, set-teorético), comunidades disciplinares de origem, convenções descritivas de superfície. Mas em nada que implique escolha ontológica ou metodológica genuína.

**A tipologia tripla é, portanto, artefato**: projeta sobre as práticas quali a folclore "deterministic vs probabilistic" como se fosse uma distinção real, e Sposito et al. importam a tipologia sem perceber que o framework PO+IBE a dissolve.

**ATENÇÃO ao agente**: NÃO traduzir EQ/PE/TC em "três variantes operacionais distintas dentro do paradigma PO+IBE" (ex.: "EQ ≈ Bayes com priors dispersos, PE ≈ Bayes sobre mecanismos, TC ≈ função estrutural binária"). Essa tradução *preserva o esqueleto da tipologia* trocando apenas o conteúdo de cada caixinha — é instância de retornar ao folclore só com vocabulário moderno. **A leitura correta é unificadora**: a tipologia inteira colapsa numa única estrutura inferencial sob PO+IBE; as "três famílias" são vestidos diferentes da mesma máquina.

**Implicação para a tese da nota**:

O que parece ser "três tradições qualitativas" é uma só estrutura inferencial vestida em três vocabulários. A v8 não precisa escolher entre EQ/PE/TC, nem propor unificação dialética entre elas — basta mostrar que o paradigma PO+IBE atravessa todas. Esse fechamento amarra os argumentos de P-V8-2 (heterogeneidade + IBE), P-V8-3 (Sposito), P-V8-5 (INUS via PO): a tipologia tripla colapsa, INUS é só uma função estrutural, e a credibilidade do estudo migra para a comparação de rivais — independentemente de qual vocabulário (probabilístico/mecanístico/set-teorético) o pesquisador prefere usar.

**Onde aparece no texto**:

A reformulação proposta em §2 (Sposito como Tipo B) deve incluir explicitamente esse fechamento: a tipologia EQ/PE/TC não é apenas "design-first sem Pearl/Rubin" — é construção que *pressupõe* ontologias distintas que o framework PO+IBE da nota mostra serem ilusórias. Reforça o gap arquitetural.

---

## P-V8-4: Pontuar que "n grande vs n pequeno" é eixo mal-enquadrado para causalidade

**Status**: RESOLVIDO em 2026-05-09 (uncommitted; parágrafo curto inserido na introdução entre o diálogo com Spirling-Stewart e o roteiro do paper, e parágrafo correlato na §Considerações Finais entre a decomposição de seleção de casos e o parágrafo sobre inferência Bayesiana. Ambos punctuam que o eixo "n grande vs n pequeno" é mal-enquadrado para inferência causal e que identificação e inferência podem operar em qualquer regime de N sob critérios de credibilidade próprios — tecnologias de desenho com múltiplas observações na quanti, comparação Bayesiana de rivais em pequeno-n qualitativo).
**Identificado em**: 2026-05-09
**Prioridade**: média — está implícito ao longo do texto, mas nunca punctuado

### Diagnóstico

O texto da v8 estabelece em §3.1, §4, §6 e §10 que identificação não depende de $n$. Mas **nunca sintetiza isso como meta-conclusão que supera a moldura KKV**. O leitor BPSR — formado em uma cultura metodológica em que $n$ é o critério principal de avaliação inferencial — pode terminar a leitura sem capturar essa virada.

### Locais onde o ponto está implícito

- §3.1 ("a identificação não depende do tamanho da amostra, mas sim do desenho")
- §4 abertura ("o estimando é recuperável... independentemente do tamanho da amostra observada")
- §5 transição ("não há diferença formal entre $n$ pequeno ou grande" — sobre Bayes)
- §6 ("a suposta incapacidade de generalização devido ao pequeno número de casos analisados não tem fundamento sólido")
- §10 ("não há nenhuma característica intrínseca à pesquisa quali ou quanti que justifique diferenças quanto à validade interna ou externa")

### Recomendação concreta

Adicionar uma frase explícita em pelo menos dois lugares:

- **Final da introdução**, após delimitar a contribuição em duas camadas:
  > "O eixo de discussão 'n grande vs n pequeno', central no debate metodológico pós-KKV, é mal-enquadrado para inferência causal: identificação e inferência podem operar em qualquer regime de $n$, sob critérios de credibilidade próprios."

- **§10 Considerações Finais**, fechando o arco do paper. Reformulação correspondente.

Esse claim é subentendido ao longo do texto mas nunca punctuado. Para o leitor BPSR é uma virada que merece sinalização explícita.

---

## P-V8-5: Tradução pedagógica de INUS para PO via exemplo do incêndio (Mackie / Sposito et al.)

**Status**: RESOLVIDO em 2026-05-09 (uncommitted; texto + figura TikZ inseridos em §3.1, @Mackie_1965 adicionado ao .bib, @jacobs_2022 removido do paper, PDF compila)
**Identificado em**: 2026-05-09
**Prioridade**: média-alta — ganho pedagógico significativo, risco zero
**Depende de / reforça**: P-V8-2 (mesmo argumento aplicado), P-V8-3 (usa exemplo de Sposito como evidência, não opositor)

### Contexto

A §3.1 atual reduz INUS/SUIN a uma frase asseverativa:

> "Vale notar, para a comunicação com a metodologia qualitativa, que noções de causalidade comumente mobilizadas em desenhos de $n$ pequeno — condições necessárias, suficientes, e configurações set-teoréticas (INUS, SUIN) — podem ser expressas em notação de resultados potenciais sem perda de generalidade [@jacobs_2022], de modo que a discussão a seguir se aplica igualmente a desenhos que adotem esse vocabulário."

Isso é asserção, não demonstração. O leitor que valoriza INUS como vocabulário próprio do quali não tem motivo para aceitar a equivalência.

### Movimento proposto

Substituir o one-liner por uma tradução concreta usando o exemplo clássico do incêndio domiciliar (Mackie 1965), exemplo que **Sposito et al. 2022 reproduzem em seu próprio texto** quando explicam INUS para audiência BR. Usar o exemplo deles transforma o texto de Sposito em evidência para o argumento da nota, não em opositor abstrato.

A demonstração:

1. Define cada fator candidato como variável binária ($F$ = fósforo, $L$ = folhas secas, $O$ = oxigênio, $R$ = raio, $T$ = telhado inflamável, $C$ = curto-circuito, $W$ = fiação inflamável)
2. Escreve a configuração INUS como função estrutural disjuntiva-conjuntiva: $Y = (F \land L \land O) \lor (R \land T) \lor (C \land W)$
3. Mostra que isso é exatamente um DAG com nós $F, L, O, R, T, C, W$ apontando para $Y$ e essa equação como mecanismo
4. Explica que "condição INUS" é, em PO, o status de cada $X_i$ numa decomposição disjuntiva-conjuntiva da função estrutural — particular, mas não exótica nem fora do paradigma PO
5. Causas SUIN admitem tradução análoga (mencionar em uma frase)
6. Liga ao argumento central: mesmo com a estrutura INUS conhecida, a análise profunda do caso pode confundir uma das causas (digamos $C$) com uma variável omitida $U$ não-considerada (vandalismo, defeito de fábrica). A profundidade *per se* não protege; o que protege é considerar "foi $U$, não $C$" como rival explícita sob comparação Bayesiana. Isso é a ponte conceitual para §4.

### Texto rascunhado (~270 palavras, pronto para inserção em §3.1, substituindo o one-liner atual)

> "Considere o exemplo do incêndio domiciliar [@Mackie_1965; @Sposito_etal_2022]. As causas candidatas são variáveis binárias — chama de fósforo $F$, folhas secas $L$, oxigênio $O$, raio $R$, telhado inflamável $T$, curto-circuito $C$, fiação inflamável $W$. A configuração 'INUS' afirma que o incêndio $Y=1$ se, e somente se, uma das conjunções $(F \land L \land O)$, $(R \land T)$, ou $(C \land W)$ for verdadeira. No vocabulário PO, isso é simplesmente a função estrutural $Y = (F \land L \land O) \lor (R \land T) \lor (C \land W)$ — um DAG com $F, L, O, R, T, C, W$ apontando para $Y$ e essa equação como mecanismo. O que o vocabulário set-teorético chama de 'condição INUS' é, em PO, o status de cada $X_i$ numa decomposição disjuntiva-conjuntiva da função estrutural: particular, mas não exótica nem fora do paradigma PO. Causas SUIN admitem tradução análoga.
>
> Mais importante: a tradução expõe um ponto que o vocabulário set-teorético tende a obscurecer. Mesmo com a estrutura conhecida — inclusive a forma INUS bem identificada —, a análise profunda do caso pode confundir uma das causas com uma variável omitida $U$. Se a fiação parece danificada, a pesquisadora pode atribuir o incêndio a $C$ quando o causador foi $U$ não-considerado (vandalismo, defeito de fábrica do material). A análise profunda *per se* não protege contra esse erro; o que protege é considerar 'foi $U$, não $C$' como hipótese rival explícita e submetê-la à comparação Bayesiana — que é a infraestrutura de credibilidade desenvolvida no restante deste trabalho."

### Implementação — checklist

- [x] Adicionar `@Mackie_1965` ao `Quali-credibilidade.bib` (referência canônica: Mackie, J. L. 1965. "Causes and Conditions." *American Philosophical Quarterly* 2(4): 245-264). Verificar formato preferido. — **Feito 2026-05-09**
- [x] Substituir o one-liner atual em §3.1 (linha aprox. 84 do estado atual) pelo texto acima. — **Feito 2026-05-09** (linha 72 do estado atual antes da edição)
- [x] Verificar se `@jacobs_2022` (atualmente referenciado no one-liner) cabe em outro lugar ou pode ser removido daquela frase. — **Resolvido 2026-05-09**: removido do paper (decisão do autor; não cabia em outro lugar sem forçar)
- [x] Avaliar se vale uma figura DAG simples em TikZ (nodes $F, L, O, R, T, C, W \to Y$, com a equação estrutural na legenda). Custa pouco; ancora visualmente; melhora fluência. — **Feito 2026-05-09**: figura inserida; `\usepackage{tikz}` + `\usetikzlibrary{arrows.meta}` adicionados ao header-includes; compila com pdflatex (engine atual do projeto, não xelatex — questão pré-existente, fora do escopo de P-V8-5)
- [x] Recompilar e verificar fluência da §3.1 com o novo conteúdo. — **Feito 2026-05-09**: PDF (343K) gerado sem erros; só warning benigno `[h] → [ht]`

### Observação sobre conexão com outras pendências

- Reforça **P-V8-2**: o "vocabulário set-teorético tende a obscurecer" no parágrafo 2 do rascunho é instância concreta do ataque ao folclore deterministic-vs-probabilistic.
- Reforça **P-V8-3**: usar Sposito et al. como exemplo dentro da nota (não como referência distante) torna a recontextualização deles em §2 mais natural; a §3 mostra a tradução, a §2 diagnostica que eles não fizeram essa tradução.
- Sinergias: implementar P-V8-5 antes de revisar §2 (P-V8-3) facilita a referência cruzada.

---

## P-V8-6: Seleção de casos — decompor a fusão KKV em três problemas distintos; reconhecer que IBE NÃO resolve colisor

**Status**: RESOLVIDO em 2026-05-09 (uncommitted; tese final v3 implementada em §2, nova subseção §4.4 "A seleção de casos, decomposta", e parágrafo em §Considerações Finais; @Card_Krueger_1994, @abadie_etal2015, @Pearl_Bareinboim_2011, @Sposito_etal_2022, @King_etal_1994, @koivuQualitativeVariationsSources2015 todos no .bib; PDF compila via xelatex). Ajuste correlato em P-V8-2 §4 também aplicado: a frase "solução para validade interna se dá pela comparação Bayesiana" foi calibrada para "solução para o problema do confundidor não-observado postulado abstratamente se dá pela comparação Bayesiana; problemas estruturais do desenho — viés de colisor — exigem solução de desenho".
**Identificado em**: 2026-05-09
**Prioridade**: alta — fecha arco com P-V8-2, P-V8-3, P-V8-4
**Depende de / reforça**: P-V8-3 (Sposito gasta tempo enorme em seleção de casos), P-V8-4 (n grande/pequeno), P-V8-2 (folclore quali)

### Histórico de reformulações (2026-05-09)

- **v1 descartada**: "anxiedade quali = herança histórica do paradigma cross-country regression dos 1980-90". Hipótese plausível mas não verificada; lit-review (Mahoney-Goertz 2006, Brady-Collier 2010, Sposito 2022, Lijphart 1971, Eckstein 1975) sustentou apenas suporte parcial e indireto.
- **v2 descartada**: decomposição em **dois** problemas (colisor vs representatividade) com IBE resolvendo o colisor. Errado: IBE não resolve colisor.
- **v3 (final)**: decomposição em **três** problemas; IBE tem limites — resolve "U postulado abstratamente" mas não viés de colisor estrutural.

### Tese final (v3)

1. **Identificação causal é uma só lógica.** Opera ao nível populacional, requer suposições críveis sobre o desenho, e é independente de $N$. A mesma lógica vale para $N$ grande (regressões padrão), $N=2$ (Card-Krueger), $N=1$ (Abadie SCM da reunificação alemã), e quali pequeno-$n$. Não há "lógica causal própria do quali" nem "lógica causal própria do pequeno-$n$".

2. **Card-Krueger 1994 e Abadie SCM ilustram esse ponto:**
   - Escolhem casos pelo **tratamento** (NJ teve aumento do salário mínimo; Alemanha Ocidental sofreu a reunificação) e adicionam contrafactual (PA sem aumento; Alemanha Ocidental sintética via donor pool).
   - **Não selecionam pela VD**: a reunificação é evento histórico que aconteceu, não condicionamos em "houve reunificação?"; $Y$ é PIB/produto, e o estudo identifica o efeito sobre $Y$ dado que houve reunificação.
   - Ninguém critica esses estudos por "viés de seleção" embora a seleção seja radicalmente não-aleatória, porque o que importa é credibilidade das suposições de identificação (tendências paralelas, comparabilidade do donor pool) para os casos escolhidos.

3. **KKV mistura três coisas distintas** que não deveriam ser misturadas:
   - **(a) Selection on Y como viés de colisor**: condicionar em $Y$ pode abrir caminho não-causal entre $X$ e variáveis omitidas. Problema **estrutural do desenho** [@Pearl_Bareinboim_2011]. KKV/Geddes acertam ao apontá-lo.
   - **(b) Não-representatividade amostral**: implica que a amostra não representa uma população definida. Problema de **inferência amostra → população** (validade externa), separado de identificação.
   - **(c) $N$ pequeno**: afeta precisão da inferência estatística condicional à identificação. Irrelevante para identificação em si.
   
   KKV opera com "lógica única de inferência" — não separa identificação (populacional, do desenho) de inferência estatística (amostra → população) — e funde (a)(b)(c) sob "selection bias". A literatura quali pós-KKV (Sposito et al. 2022, Koivu-Damman 2015) absorve a fusão e a reproduz em tipologias de "lógicas próprias de seleção" (EQ/PE/TC).

4. **Decomposição correta**:
   - **(a)** é problema **estrutural do desenho**. **NÃO é resolvido por IBE/comparação Bayesiana**: se $Y$ é colisor entre $X$ e variáveis omitidas, condicionar em $Y$ abre dependência espúria, e a evidência condicionada deixa de discriminar entre DAGs alternativos — todos os DAGs comparados ficam igualmente compatíveis com a evidência. **IBE quebra sob colisor estrutural**. Solução: desenhar para não condicionar em $Y$, ou justificar via análise estrutural que $Y$ não é colisor relevante.
   - **(b)** é separado. Resolvido (quando o é) por teorização explícita de escopo de aplicação.
   - **(c)** é separado ainda. Absorvido pela inferência Bayesiana — a comparação de posteriors funciona em qualquer regime de $N$, incluindo $N$ pequeno.

5. **Limites de IBE como contribuição da nota.** A Camada 3 desta nota propõe IBE/comparação Bayesiana de rivais como substituto funcional da CR em quali pequeno-$n$ para o problema **"e se houver um $U$ postulado abstratamente?"** — IBE força articulação dos rivais com implicações empíricas próprias, transformando "$U$s inventáveis" em "explicações comparáveis". É o que IBE faz bem. **IBE não trata viés de colisor estrutural**: se o desenho condiciona em $Y$, IBE quebra independentemente de quão Bayesiana é a comparação. IBE é solução para problemas de **enumeração de explicações dado um desenho que identifica**, não cura para falhas estruturais do desenho.

### Compatibilidade com Spirling-Stewart 2025

A formulação de IBE como "comparação de DAGs alternativos sob a evidência empírica" — usada na decomposição acima — vai além de SS, embora seja compatível.
- SS define "explanation" via tradição causal-mecânica (Salmon, Woodward com counterfactuals); DAG é formalização natural mas SS **não menciona DAG explicitamente** no PDF.
- SS evita Bayes deliberadamente (footnote 1); a operacionalização Bayesiana / Bayes-factor / decibéis vem de F&C 2022 e H&J 2015/2023, não de SS.
- SS **não menciona viés de colisor / selection bias** como categoria nem discute limites estruturais de IBE.
- O ponto "viés de colisor quebra IBE" é insight da nota, não derivado de SS.

Para citação no paper, manter linha SS-CR já estabelecida na §4.3: SS preserva CR onde funciona; a nota propõe IBE como substituto funcional em quali pequeno-$n$ para o problema do $U$ inventável. **NÃO atribuir a SS a formulação "IBE como comparação de DAGs"** — essa síntese é da nota, ancorada em F&C 2022, H&J 2015/2023, Pearl, Lipton 2004.

### Esboços por seção (versão final — pendente aprovação)

**§2 (Recepção BR)** — recontextualizar Sposito como instância da fusão KKVeana:

> Esboço: "Sob a separação aqui adotada — identificação causal como propriedade populacional do desenho, inferência estatística como problema de amostra → população —, o problema da seleção de casos se decompõe em três subproblemas distintos que KKV (1994) deixaram unidos: (i) selecionar pelo valor da variável dependente pode induzir viés de colisor [@Pearl_Bareinboim_2011], problema **estrutural do desenho** que demanda evitar condicionar em $Y$ ou justificar que $Y$ não é colisor relevante; (ii) selecionar de modo não-aleatório limita o que se pode concluir sobre populações alheias aos casos selecionados, problema de validade externa, separado de identificação; (iii) tamanho pequeno de amostra afeta a precisão da inferência estatística condicional à identificação, problema separado dos dois primeiros. KKV opera com 'lógica única de inferência' que não separa identificação de inferência estatística e funde (i)(ii)(iii) sob a rubrica de *selection bias*; @Sposito_etal_2022, seguindo a tipologia de Koivu & Damman (2015), absorve essa fusão e a reorganiza em famílias de 'lógicas próprias de seleção' (EQ/PE/TC). Decompostos, os três têm respostas distintas: (i) é problema de desenho — nem IBE nem nenhuma comparação de rivais o resolve, porque o caminho não-causal aberto faz com que a evidência condicionada deixe de discriminar entre os mecanismos alternativos; (ii) e (iii) são logicamente independentes da identificação, e a sua relevância depende do que se pretende concluir, não da seleção em si."

**§4 (centro do paper)** — destrinchar com Card-Krueger e Abadie SCM:

> Esboço (subseção curta, posicionar antes ou depois de §4.3 conforme melhor flow): "A separação entre identificação e inferência estatística permite tratar a 'seleção de casos' sem confundir três problemas que KKV (1994) deixaram unidos. **Selecionar pelo valor da variável dependente** pode induzir viés de colisor [@Pearl_Bareinboim_2011]: condicionar em $Y$ abre um caminho não-causal entre $X$ e variáveis omitidas. É problema **estrutural do desenho** que não muda com tamanho de amostra: em estudo de $N=1$ ou $N=1000$, sob colisor aberto a evidência condicionada deixa de discriminar entre os mecanismos causais alternativos, e nenhuma comparação Bayesiana de explicações rivais resolve o problema. A solução é desenhar para não condicionar em $Y$, ou justificar via análise estrutural (DAG, modelo causal) que $Y$ não é colisor entre $X$ e as variáveis omitidas relevantes. **Selecionar de modo não-aleatório**, por sua vez, é problema de inferência amostra → população: limita o que se pode concluir sobre populações alheias aos casos selecionados — questão de validade externa, separada de identificação. **Tamanho pequeno de amostra** afeta a precisão da inferência estatística condicional à identificação, problema absorvido pela inferência Bayesiana desenvolvida adiante. A lógica causal é a mesma nos três regimes. @Card_Krueger_1994 compararam New Jersey e Pensilvânia sem pretensão de representatividade do conjunto de estados americanos; @abadie_etal2015 estudam o efeito da reunificação sobre o produto da Alemanha Ocidental usando uma única unidade tratada e um donor pool de outros países da OECD. Nem Card-Krueger nem o estudo da reunificação são acusados de 'selecionar pela variável dependente': escolhem casos pelo **tratamento** (aumento do salário mínimo; ocorrência da reunificação) e adicionam contrafactual (estado vizinho; donor pool sintético); $Y$ é o salário/produto, não presença/ausência do tratamento. A credibilidade desses estudos repousa exclusivamente sobre a defensabilidade das suposições de identificação — tendências paralelas para o par NJ-PA, comparabilidade do donor pool para a Alemanha — para os casos escolhidos. A mesma lógica organiza desenhos qualitativos pequeno-$n$: tamanho de amostra ou ausência de aleatoriedade não introduzem epistemologia adicional."

**§10 (Considerações Finais)** — fechamento sintético:

> Esboço: "A consequência prática mais imediata é a separação clara entre três problemas que KKV (1994) deixaram unidos sob 'lógica única de inferência'. Selecionar pelo valor da variável dependente pode induzir viés de colisor (problema estrutural do desenho); selecionar de modo não-aleatório limita o que se conclui sobre populações alheias aos casos selecionados (problema de validade externa); tamanho pequeno de amostra afeta a precisão da inferência estatística (problema separado). A literatura qualitativa pós-KKV — brasileira inclusive (@Sposito_etal_2022) — herdou a fusão e a reproduz em tipologias de 'lógicas próprias de seleção'. Decompostos, os três têm respostas distintas: o primeiro é tratado por desenho que evita condicionar em $Y$, não por enumeração de rivais (a comparação Bayesiana desenvolvida nesta nota se aplica ao problema do $U$ inventável abstratamente, não ao colisor estrutural do desenho); o segundo é resolvido — quando o é — pela teorização explícita do escopo; o terceiro é absorvido pela inferência Bayesiana. Estudos quantitativos modernos com seleção radicalmente não-aleatória — @Card_Krueger_1994 com dois estados, @abadie_etal2015 com uma única unidade tratada — ilustram que a lógica causal é a mesma em qualquer regime de $N$ e independentemente de aleatoriedade na seleção: a pergunta sobre seleção de casos não é 'a amostra representa o universo?' mas 'as suposições de identificação são defensáveis para os casos escolhidos?'."

### Refs alvo

- `@Card_Krueger_1994` — já no `.bib`
- `@abadie_etal2015` — verificar se está no `.bib`; verificar chave correta
- `@Pearl_Bareinboim_2011` — já no `.bib`
- `@Sposito_etal_2022` — já no `.bib`
- `@Koivu_Damman_2015` — verificar se está no `.bib`; se não, adicionar

### NOTA DE REVISÃO — P-V8-2 §4 abertura precisa ajuste de precisão

Identificado durante a discussão de P-V8-6: o parágrafo inserido em §4 abertura por P-V8-2 termina com "A solução do quali de $n$ pequeno para validade interna se dá pela comparação Bayesiana de explicações rivais sob critério de inferência à melhor explicação, conforme desenvolvido a seguir."

Sob a clarificação do autor sobre os limites de IBE (não resolve colisor), essa frase pode ser lida como overclaim — sugere que IBE resolve "validade interna" em geral, quando na verdade IBE resolve apenas o problema do $U$ postulado abstratamente; viés de colisor estrutural exige solução de desenho. Reformular para algo como:

> "A solução do quali de $n$ pequeno para o problema do confundidor não-observado postulado abstratamente — '$U$ inventável' — se dá pela comparação Bayesiana de explicações rivais sob critério de inferência à melhor explicação, conforme desenvolvido a seguir. Problemas estruturais do desenho — viés de colisor em estudos que condicionam em $Y$, por exemplo — exigem solução de desenho, não de inferência, e são tratados separadamente."

Implementar simultaneamente com P-V8-6 §4 (mesma seção, mesmo arco argumentativo).

### Conexão com outras pendências (arco completo)

Este item fecha o arco do folclore quali junto com P-V8-2, P-V8-3, P-V8-4 e P-V8-5:

- P-V8-2: heterogeneidade + IBE substituem causalidade-determinística como ontologia
- P-V8-3: tipologia EQ/PE/TC colapsa sob PO+IBE (cinco confusões em Sposito)
- P-V8-4: $n$ grande/pequeno é eixo mal-enquadrado para causalidade
- P-V8-5: INUS é função estrutural disjuntiva-conjuntiva sobre binárias
- **P-V8-6 (este)**: seleção de casos é parte da defesa das suposições de identificação, não tem lógica própria

Os cinco pontos têm uma estrutura comum: **o quali contemporâneo (incluindo a literatura BR) opera sob convenções herdadas de um paradigma quanti pré-credibility-revolution; o framework PO+IBE da v8 dissolve essas convenções uma a uma**. Implementadas em conjunto, fecham um argumento sistematicamente coerente.

### Recomendação de ordem de implementação

1. P-V8-5 primeiro (INUS via PO) — texto rascunhado, risco zero, ancora a lógica
2. P-V8-2 (heterogeneidade + IBE) — refinamento de tese, afeta §3.1, §4, §6, §10
3. P-V8-6 (este — seleção de casos) — adiciona Card-Krueger como contraste em §2, §4, §10
4. P-V8-3 (recontextualização de Sposito) — síntese das cinco confusões em §2, depende dos anteriores
5. P-V8-4 (pontuar $n$ irrelevante) — frase em intro e §10
6. P-V8-1 (jargão $U$) — última passada de redação na intro

---

## Como usar este arquivo

- Adicionar pendências aqui à medida que aparecem em sessões de edição.
- **Ser detalhado, não sintético**: o próximo agente precisa de contexto suficiente para não reproduzir os lugares-comuns que o autor está atacando. Sumários sintéticos perdem a textura do diagnóstico.
- Ao começar nova sessão de revisão, ler este arquivo antes do manuscrito.
- Resolver uma pendência: marcar como resolvida com data e commit hash, mas não deletar (manter histórico).
- Bloqueio submissão: nenhuma pendência aberta deve estar em "P-V8-X" no momento da submissão à BPSR.
