# Glauco Silva 2023 (Desenho de Pesquisa, ENAP) — Partials das 6 leituras paralelas

**Data**: 2026-05-08
**Livro**: Silva, Glauco Peres da. *Desenho de Pesquisa*. Brasília: ENAP, 2023 (ed. revisada). 119 pp. ISBN 978-65-87791-31-9.
**Pipeline**: 6 agentes paralelos, cada um lendo uma fatia (PDF: `Livro_desenho_de_pesquisa (2).pdf`)
**Consolidação**: ver `2026-05-08_glauco-silva-2023-consolidado.md`

---

## Fatia 1 — §1 Introdução + §2.1 Produção de conhecimento + §2.2 Teorias e explicação (pp. 7-33)

### Vocabulário e moldura

Glauco Silva opera **predominantemente com vocabulário da filosofia da ciência das ciências sociais**, não com o vocabulário da credibility revolution. Termos centrais: ontologia, epistemologia, metateoria, naturalismo vs construtivismo, realismo vs pragmatismo, indução, dedução, adução, leis universais vs probabilísticas, explicação pragmática/semântica/sintática (Abbott), teorias e operacionalização. Moldura organizadora: filosofia da ciência social (Della Porta-Keating, Moses-Knutsen, Jackson) — não Pearl, não Rubin, não Angrist-Pischke, não Imbens-Rubin.

**Não aparecem nesta fatia**: KKV, DAGs, "potential outcomes", "identification strategy", "credibility revolution", Pearl, Rubin, Angrist, Imbens, Hernán, Heckman. A palavra "identificação" aparece uma vez (p. 22) no sentido genérico de "encontrar padrões".

### Identificação vs inferência

**Não faz, nem tematiza, o split**. A divisão é "dimensão ampla" (filosófica/teórica/ontológica/epistemológica) vs "dimensão prática" (escolha de tema, coleta, análise) — versão do par filosofia/empiria, não identificação/estimação. Causalidade aparece como problema de filosofia da ciência social (qual concepção de causa: Neo-Humeana, contrafactual, manipulação, mecanismos via Brady 2008), não como problema de design separado do estimador.

**Classificação preliminar**: Tipo A (pós-KKV unificado, com flexão pragmatista). Vocabulário é o de *valid inference* à la KKV/Brady-Collier, não o de *credibly identified estimand*.

### Trechos-chave

> "A fim de estabelecer um desenho de pesquisa que produza resultados válidos, deve-se explorar as conexões e os limites do relacionamento dessas duas dimensões." (p. 9)

> "A clareza sobre a questão de pesquisa requer a especificação dos conceitos que envolvem o problema de interesse. (...) Uma vez conhecidos os conceitos e suas relações, a metodologia adotada deve cuidar de transformar aqueles conceitos em variáveis, etapa denominada operacionalização." (p. 12)

> "[A] prática nas Ciências Sociais lida com quatro perspectivas diferentes com relação à inferência causal: (1) Neo-Humeana; (2) contrafactual; (3) manipulação via experimentação; (4) mecanismos." (p. 34, abertura de §2.3)

### Veredito da fatia

Tipo A — pós-KKV unificado com base em filosofia da ciência social (Della Porta-Keating, Moses-Knutsen, Jackson, Brady), KKV nem é citado. Eixo organizador: teoria → operacionalização → método. Causalidade como questão filosófica plural via Brady 2008, não como problema de design separado da estimação. Descontextualizada da credibility revolution: nem o vocabulário, nem as referências, nem a estrutura conceitual a invocam.

---

## Fatia 2 — §2.3 Causalidade (pp. 33-43) — SEÇÃO CRÍTICA

A seção começa em p. 33 (não 34) e termina em p. 43. **"A referência básica utilizada nesta seção é Brady (2008)"** (n. 23, p. 33) — determinante para tudo que se segue.

### Definição de causalidade

Silva NÃO adota uma definição única — apresenta **quatro perspectivas** filosófico-metodológicas como menu, todas extraídas de Brady (2008):
1. Neo-Humeana (correlacional, conjunção constante)
2. Contrafactual (Lewis-Weber)
3. Manipulação (experimentação)
4. Mecanismos e capacidades

Tabela 1 (p. 40), "Adaptado de Brady (2008, p. 219)", com autores associados: Hume, Mill, Hempel, Beauchamp, Rosenberg (Neo-Humeana); Weber, Lewis (Contrafactual); Gasking, Von Wright, Menzies, Price (Manipulação); Hartre [sic] e Madden, Cartwright, Machamber [sic — Machamer], Darden e Craver, Glennan (Mecanismos).

A perspectiva contrafactual é Lewisiana ("mundo mais similar"), com Weber como exemplo — **não** Rubin Causal Model. Linguagem: "se X fosse ocorrer, então Y ocorreria" (p. 35), tratada como par de proposições contrafactuais para verificação, **não** como par (Y(1), Y(0)) de variáveis aleatórias.

**Ausentes**: Pearl, SCM, DAG, do-calculus, Mahoney (necessary/sufficient/INUS/SUIN), KKV, Fairfield-Charman, Humphreys-Jacobs.

### Identificação aparece como conceito separado?

**Não — não na acepção pós-credibility-revolution.** O termo "identificação" aparece, mas em sentido genérico:
- p. 33: "identificar a causa dos eventos" (uso coloquial)
- p. 34: "identificação da causa via experimentação" (descrição de uma das quatro perspectivas)
- p. 38: "A identificação de uma causa está em estabelecer a maneira como as partes de um processo se conectam" (mecanismos)
- p. 41: "identifica-se que..." (uso reflexivo)

**Não há nenhuma ocorrência** de "estratégia de identificação" como conceito técnico, "problema de identificação", "condições de identificação", nem distinção entre "identificação" e "estimação". Confounders, backdoor, frontdoor, omitted variable bias: ausentes nesta seção.

A única distinção operacional que aproxima de algo pós-Rubin é "**causa de um efeito vs efeito de uma causa**" (pp. 39, 41) — distinção inspirada em Holland 1986/Gelman-Imbens, mas Silva não cita nenhum dos dois e a apresenta como divisão didática própria.

### DAGs / Potential outcomes / Counterfactuals

- DAGs: ausentes. A única figura é Figura 2 (p. 42) — fluxograma metateórico, não DAG causal.
- Potential outcomes: ausentes na notação Y(0), Y(1).
- Counterfactuals: presentes em prosa Lewisiana, não Rubin.
- Mantra "correlação não é causalidade" mencionado em nota (n. 24, p. 34).

### Credibility revolution

**Não há menção alguma.** Ausentes: Angrist, Pischke, Imbens, Card, Athey, Rubin, Neyman, Pearl, Hernán, Robins, Holland, Rosenbaum, Heckman, Manski, Lundberg-Johnson-Stewart. Não há "identification strategy", nem "natural experiment", nem "as-if random". A única nota com referência metodológica empírica (n. 25, p. 37) é Morton & Williams (2010) sobre experimentos.

### KKV vs Pearl/Rubin

**Nenhum dos dois.** A moldura é **Brady (2008)** — pluralismo filosófico em quatro abordagens, herdeiro do debate Brady-Collier-Mahoney sobre KKV mas em registro filosófico-metodológico.

### Quali nesta seção

Silva **não** distingue causalidade quanti vs quali aqui. As quatro abordagens são pluralismo dentro das CS, sem clivagem disciplinar. Ausentes: Bennett-Checkel, F&C, H&J, Mahoney metodológico, Goertz pós-2010, Beach-Pedersen, process tracing.

### Trechos-chave

> "A referência básica utilizada nesta seção é Brady (2008)." (n. 23, p. 33)

> "A prática nas Ciências Sociais lida com quatro perspectivas diferentes com relação à inferência causal: (1) Neo-Humeana; (2) contrafactual; (3) manipulação via experimentação; (4) mecanismos." (p. 34)

> "Sobre a perspectiva contrafactual, a sua formulação básica sobre causalidade depende da verificação se duas afirmações sobre dois eventos distintos X e Y são verdadeiras. A primeira afirmação a ser verificada se é verdadeira é a de que 'se X fosse ocorrer, então Y ocorreria'. A segunda afirmação, chamada de contrafactual, é a de que 'se X não fosse ocorrer, então Y não ocorreria também'. Se ambas as frases forem entendidas como verdadeiras, então X causa Y." (p. 35)

> "A pergunta de pesquisa pode ser tal que procura identificar a causa de um efeito ou o efeito de uma causa." (p. 39)

> "A ideia de causa contrafactual permeia praticamente todas as demais [abordagens], em maior ou menor grau." (p. 41)

> "É diante desse amplo cenário de debates filosóficos sobre a produção de conhecimento científico e seus desdobramentos que o desenho de pesquisa está inserido." (p. 43)

### Veredito da seção crítica

**Tipo A — pré-credibility-revolution, sem split.** Silva 2023 §2.3 NÃO faz o split entre identificação causal (design step) e inferência estatística (estimation step). A moldura é taxonomia filosófico-pluralista derivada de Brady (2008). A literatura da credibility revolution está totalmente ausente; DAGs, potential outcomes formais e linguagem de confounding também. A literatura quali-causal contemporânea (F&C, H&J, Bennett-Checkel, Beach-Pedersen, Mahoney/Goertz pós-2010) também ausente.

**Implicação**: Silva 2023 **não** é caso BR de Tipo C, nem mesmo de Tipo B. É livro-texto BR pós-2020 que opera dentro de moldura pluralista-filosófica pré-credibility-revolution, com Brady 2008 como referência básica única. **Reforça — não enfraquece — o claim do gap BR**.

---

## Fatia 3 — §3 + §3.1 + §3.1.1 Experimentos (pp. 45-70)

### Identificação como conceito (sim/não, como)

**Sim, e de forma surpreendentemente explícita** — sobretudo na transição de §3.1.1 para §3.1.2 (pp. 66-67). O termo "identificação"/"estratégias de identificação" aparece com sentido técnico:

> "Em uma situação desse tipo, é necessário que se busque criar **estratégias de identificação**. Isso implica em buscar isolar o efeito de Z, tanto em X quanto em Y..." (p. 66)

> "As estratégias de identificação para isolar o efeito de Z são as mais diversas. É importante notar que **a identificação é um passo central na busca pela mensuração da relação entre X e Y**. (...) A identificação passa por isolar os efeitos endógenos e simultâneos que ocorrem diante daquilo que se deseja estudar" (p. 67)

Potential outcomes notation aparece formalmente:
- δ_i = (Y_i|X=1) − (Y_i|X=0) (eq. 3, p. 63) — *"o efeito observado para cada observação i"*
- E[δ] = E[(Y|X=1) − (Y|X=0)] (eq. 4, p. 64) — *"efeito médio do tratamento (ATE)"* (p. 65)

Random assignment como o que **garante** que os grupos sejam idênticos em média em Z (p. 64). Nível técnico razoavelmente alto para manual em PT — chega à fronteira do Rubin causal model, mas apresentado de forma coloquial (sem "potential outcomes", "SUTVA", "ignorabilidade").

### Vocabulário Rubin/Imbens

**Uma única referência direta a Rubin** em nota (n. 44, p. 60): *"A referência principal para a análise experimental nas Ciências Sociais é o modelo de Rubin que, com as contribuições posteriores de Neyman e Holland, também é chamado modelo de Neyman-Rubin ou de Neyman-Rubin-Holland. Vide Sekhon (2007). Deve-se mencionar também a importância recente dos trabalhos de J. Pearl..."*

Holland 1986 não citado nominalmente; Imbens, Athey, Angrist-Pischke como dupla também não. Pearl en passant na mesma nota.

Angrist aparece duas vezes — p. 69 e n. 47. Heckman duas vezes (p. 63 contrafactual; Heckman & Hotz 1989, p. 66). Vernon Smith em bloco (p. 61). Morton & Williams 2010 como base do capítulo (notas 34, 38).

**Experimento como gold standard**: sim, ainda que com qualificações.

> "Em um experimento, o problema de se isolar efeitos de X e Z sobre Y está resolvido. Por definição, o experimento se dá em uma situação em que as variáveis de contexto, que podem interferir na relação que se deseja estudar, estão controladas." (p. 61)

### Confounding / variáveis omitidas

**Sim, com nome técnico**. Apresenta o problema em três níveis:
1. Caso "X→Y←Z" (eq. 2, p. 59) como problema básico de superestimação/subestimação
2. Distingue Z observável de Z não-observável (pp. 60-61)
3. Formaliza na p. 66: *"De acordo com a expressão (5) [X→Y←Z com Z→X], Z não só interfere em Y, como também interfere em X. Nesse caso, Z é conhecida como **confounder**. Em uma situação desse tipo, é necessário que se busque criar estratégias de identificação."* + *"Quando isso é calculado, diz-se que as estimativas da relação entre X e Y sofrem de **viés de variável omitida**"*.

Endogeneidade discutida pp. 67-68 com exemplos (sorvete/tubarões; crimes/policiais). Nota n. 46 cita Clarke 2005, 2009.

Tratado como problema de **identificação**, não apenas de estimação — "estratégias de identificação" aparecem precisamente neste contexto.

### Veredito da fatia 3

**Tipo A com qualificação — opera dentro do vocabulário da credibility revolution, mas em registro mais antigo (Sekhon 2007 / Morton-Williams 2010), não na fronteira atual.** Silva usa "estratégias de identificação", confounder, viés de variável omitida, ATE, potential outcomes (em forma de equação), atribuição aleatória como mecanismo. Mas a sofisticação fica abaixo de Cunningham/Huntington-Klein/Hernán-Robins: não usa "ignorabilidade", "DAG", "backdoor", "SUTVA", e a separação identificação ≠ estimação não estrutura o capítulo (aparece tópico-a-tópico, não como princípio organizador).

**O autor opera dentro da credibility revolution — mas como tradutor para iniciantes, não como evangelista do split conceitual.** A v8 pode citar Silva 2023 como evidência **a favor** do diagnóstico de que o vocabulário existe na BR; ao mesmo tempo, a organização do livro sugere que mesmo um manual recente em português não trata identificação como o conceito fundador da pesquisa N-grande — ele aparece tarde, depois de tipologias kuhnianas e schmitterianas.

---

## Fatia 4 — §3.1.2 Métodos quase-experimentais (pp. 71-88)

### Vocabulário identification strategy (geral)

O autor **não usa explicitamente o vocabulário "estratégia de identificação"** de forma sistemática, mas opera com proxy conceitual: **"exogeneidade"** da atribuição do tratamento. Aparece "tratamento exógeno" para RD (p. 73, p. 75); **autosseleção** como problema central em PSM (pp. 77-78); "início exógeno" para DiD (p. 83, n. 52). "Identificado" aparece duas vezes — "parâmetros identificados com o efeito líquido de X sobre Y" (p. 72); "A maneira de **identificar** o efeito do tratamento está em comparar a diferença entre o grupo de tratamento e de controle..." (p. 83). Mas **a palavra "identificação" não é tematizada como passo separado de "estimação"**. Não há "aqui está a suposição identificadora" em nenhuma das cinco subseções. O leitor sai com a noção correta de que cada técnica tem uma condição-chave (descontinuidade, observáveis, controle sintético plausível, tendências paralelas), mas o autor não entrega o **rótulo conceitual unificador** que Angrist-Pischke ou Cunningham usariam.

### Regressão múltipla

Silva é **explícito** em recusar regressão múltipla como ferramenta de identificação causal:

> "uma regressão múltipla por si só não se refere à causalidade entre X e Y, mas sim deve ser interpretada como uma correlação. Porém, ela não é idêntica ao simples cálculo de correlação, porque na regressão estão considerados os efeitos dos controles sobre a variável dependente, o que não ocorre no cálculo de uma correlação. Assim, apesar de ser mais sofisticada na avaliação da relação entre X e Y, não se refere à relação causal entre elas." (p. 72)

> "a estimação dos parâmetros, usando o modelo de regressão, é importante para entender os efeitos entre X e Y, mas não nos permite falar de causalidade." (p. 73)

**Não há discussão de viés de variável omitida formalmente, nem de "conditional ignorability"/"selection on observables" no sentido moderno**. A regressão é tratada como técnica de descrição/correlação parcial, e a causalidade é deslocada para os desenhos quase-experimentais.

### RDD

Razoavelmente alinhada com o moderno, mas **incompleta tecnicamente**. Há "as if random" assignment local: *"a RD é associada a uma aleatorização local (Lee, 2008)"* (p. 74). Cita Hahn, Todd & Van der Klaauw (1999). **Não há menção a Cattaneo, McCrary, nem ao manipulation test** (McCrary density test). **Não há "continuity assumption" formalizada**. Trade-off de bandwidth informalmente (p. 76). Polinômios de 4º grau como especificação usual (p. 76) — referência **datada** dado o consenso pós-Gelman-Imbens (2019) contra polinômios globais altos. Exemplo 13 (Avelino-Biderman-Barone 2012) e Izumi 2016 são exemplos BR de RD aplicada.

### PSM

**Chega muito próximo** do vocabulário moderno de identificação, sem nomeá-lo. P. 78: selection-on-observables verbalizado, CIA verbalizado:

> "O argumento central do PSM é o de que se os potenciais resultados do tratamento não dependem do grupo dos participantes condicionalmente às variáveis observáveis, X, os potenciais resultados também serão independentes do tratamento condicional. Portanto, é possível substituir X pelo *propensity score*" (p. 78)

Cita Rosenbaum & Rubin (1983) (n. 49, p. 78) — referência canônica. Mas **não nomeia "CIA", "unconfoundedness", "selection on observables"** como termos técnicos. Reconhece a limitação:

> "O viés ainda se manterá caso características não observáveis influenciarem a participação em um dos grupos e os resultados condicionais a X" (p. 80)

Exemplo 14 sobre Bolsa Família (Duarte et al. 2009) é exemplo BR adequado.

### Synth control

Cita Abadie, Diamond & Hainmueller (2010) logo de saída (p. 80) — atribuição correta. Apresenta como mecanismo de identificação contrafactual:

> "no caso em questão, tem-se apenas uma única observação sujeita ao tratamento... Não se conhece (Y_1|X=0), ou seja, não se sabe o valor de Y quando o tratamento não ocorreu. A tarefa está, então, em encontrar uma maneira de estimar esse valor." (p. 80)

Menciona placebo in space (p. 82). **Não cita Abadie 2021 (JEL survey)**, **não menciona pre-treatment fit como diagnóstico**, nem **inferência via permutação**. Exemplo 15 (Corseuil et al. 2015) é BR.

### DiD

A apresentação é **a mais fraca das cinco em termos de explicitação da suposição identificadora**. **Tendências paralelas (parallel trends/common trends) NÃO é nomeada**. Intuição aparece (p. 83-84). **Não cita Bertrand-Duflo-Mullainathan (2004)** sobre serial correlation. **Não menciona staggered DiD**, Goodman-Bacon, Callaway-Sant'Anna, de Chaisemartin-D'Haultfoeuille — toda a revolução pós-2018 ausente. Exemplo 16 (Nishijima et al. 2011) é BR.

### DAGs / esquemas

**Não há nenhum DAG**. **Não há diagrama de potential outcomes**. Quatro figuras puramente gráficas/visuais convencionais (estilo Angrist-Pischke), mas nenhuma é causal-graphical no sentido Pearl. Coerente com tradição econométrica brasileira (FEA-USP).

### Veredito da fatia 4

**Tipo B com inclinações para C — predominantemente B**. Apresentação **competente, didática, e razoavelmente alinhada com o espírito da credibility revolution** (regressão é correlação parcial, não causalidade; quase-experimentos exploram exogeneidade da atribuição; PSM verbaliza CIA), mas **opera dentro do framework econométrico tradicional**, sem o aparato moderno de identification-design-as-distinct-step.

Falta o vocabulário técnico unificador (não há "estratégia de identificação" como termo central, não há "identifying assumption", não há "parallel trends" nomeado, não há "continuity assumption"); falta DAGs e potential outcomes diagrams; falta a literatura pós-2010 (Cattaneo, Goodman-Bacon, Callaway-Sant'Anna, Abadie 2021); falta Angrist-Pischke / Cunningham / Huntington-Klein nas referências.

---

## Fatia 5 — §3.2 N-pequeno (pp. 89-108) — CRÍTICA QUALI

### §3.2 introdução

Pesquisa N-pequeno enquadrada como **permeada pela tensão entre flexibilidade/descoberta e rigor científico** (p. 90). Padget (2017) como referência central. Pesquisa qualitativa: *"as hipóteses são substituídas por **premissas** que não indicam uma relação de causa e efeito, mas uma tendência em seu lugar"* (p. 93). Menção a **equifinalidade** (p. 93) e referência a QCA/álgebra booleana. Vinculação metateórica com **pragmatismo** (Padget; Moses-Knutsen) e construtivismo. Tom didático-descritivo, **sem mobilização de identificação causal nem de inferência Bayesiana**.

### Estudo de caso — definição e função

Adota definição de **Pascal Vennesson (2008, p. 226)**: *"um caso é um fenômeno, ou um evento, escolhido, conceitualizado e analisado empiricamente como uma manifestação de uma classe mais ampla de fenômenos"* (p. 95). Daí extrai quatro características (p. 95): caso ≠ unidade de observação, mas categoria teórica.

Três funções (p. 96, via Vennesson): desenvolver/avaliar teorias (Skocpol); formular hipóteses (indutivo); explicar fenômenos particulares.

**Não há menção a Gerring, George & Bennett 2005, Bennett & Checkel 2015**. Yin não aparece. Única referência tratada como autoridade: **Vennesson 2008**. Complementada por Rueschemeyer 2003 e Seawright & Gerring 2008. **A bibliografia da fronteira metodológica anglófona contemporânea está inteiramente ausente**.

### Process tracing

**Ausente como termo**. Expressão "process tracing" / "rastreamento de processos" / "análise de processos" **não aparece em nenhuma das 20 páginas**. Beach-Pedersen 2013/2019 não citado. **Não há tratamento da lógica de testes hoop/smoking-gun/straw-in-the-wind**. Não há discussão de tipos de evidência diagnóstica nem de força probatória. **A literatura de process tracing — peça central da metodologia qualitativa contemporânea — é inteiramente ausente**.

### Inferência Bayesiana

**Inteiramente ausente**. "Bayes" / "Bayesiano" / "atualização de crenças" / "prior" / "likelihood ratio" / "posterior" **não aparecem nenhuma vez** nas pp. 89-108. **F&C 2017/2022 não citado**. **H&J 2015/2023 não citado**. Não há discussão de razão de verossimilhança, Teorema de Bayes aplicado a evidências qualitativas, nem de probabilidades epistêmicas. **A virada Bayesiana na metodologia qualitativa simplesmente não está no horizonte do livro**.

### Hipóteses rivais

A enumeração de **hipóteses rivais como passo metodológico estruturado é ausente**. Termo "hipóteses rivais" / "explicações alternativas" / "explicações concorrentes" **não aparece**. Apenas duas menções tangenciais: viés de explicação desejada (p. 100) e sobredeterminação histórica (Rueschemeyer 2003, pp. 106-107). Mas em nenhum momento o livro **prescreve** que o pesquisador deva enumerar e comparar sistematicamente hipóteses rivais. A lógica IBE está ausente. **A noção de "credibilidade quali = robustez da comparação entre rivais" — peça central da nossa tese v8 — não é o quadro do livro**.

### DAGs em quali / U não-observado

**Inteiramente ausente**. DAGs não aparecem. "Variável omitida" / "confundidor" / "viés de variável omitida" não aparecem na fatia. A linguagem de identificação causal pearliana ou econométrica está fora do horizonte qualitativo no livro. **A própria pergunta "como o quali responde à objeção do U" não é colocada**.

### Análise histórica comparada (§3.2.2)

Subseção pp. 105-108. Referência principal **Rueschemeyer 2003**. Análise histórica como permitindo "desenvolvimento de novas teorias, teste de teorias existentes e uso das teorias resultantes" (p. 105). Exemplos: E. P. Thompson e Robert Michels.

Mill aparece duas vezes (n. 58, p. 101 e p. 101). Skocpol como exemplo (Exemplo 17, p. 101; p. 96). Mahoney apenas em nota (Mahoney-Goertz 2004 sobre princípio da possibilidade) e Mahoney 2000 sobre **path dependence** (p. 107). Goertz só na mesma nota. **Ragin/QCA aparece de relance** (p. 93). **Thelen 2003** em path dependence.

A moldura predominante é **Rueschemeyer + path dependence (Mahoney/Thelen)**, com Mill como pano de fundo histórico e Skocpol como exemplo, sem mobilização da literatura QCA/Ragin propriamente dita nem de Mahoney metodológico (Logic of Social Science 2021).

### Identificação vs inferência em quali

**O autor não distingue identificação causal de inferência estatística no quali — nem como problema separado nem como crítica à confusão entre os dois**. Vocabulário do livro: "testar teoria", "avaliar teoria", "desenvolver teoria", "explicar fenômenos", "estabelecer inferência causal" (p. 101, ao remeter Mill), "confirmar mecanismo causal" (p. 101, sobre caso típico). **Não há sinal do split conceitual identificação ≠ inferência**.

A discussão sobre seleção pela variável dependente (pp. 100-104) — onde KKV é mobilizado — é apresentada como **disputa metodológica clássica** (variable-oriented vs case-oriented). Crítica a KKV é por **escopo de objetivo**, não por desalinhamento entre identificação e inferência:

> "King, Keohane e Verba estão preocupados em testar teorias, abordagem típica das pesquisas de N-grande, e não com a formação, elaboração e refinamento de conceitos" (p. 104)

**A revolução da credibilidade não toca o quali do livro**.

### Veredito da fatia 5

**Tipo A — pré-credibility-revolution e pré-Bayesian quali**.

- Process tracing **ausente** como técnica nomeada
- Inferência Bayesiana **inteiramente ausente** (zero menção a Bayes, F&C, H&J, Bennett-Checkel, Beach-Pedersen)
- Enumeração de hipóteses rivais como passo metodológico estruturado **não tematizada**
- DAGs e a manobra "U não-observado" **não mobilizados** nem para apontar limite nem para discutir transferibilidade
- Identificação ≠ inferência **não distinguido**

**Implicação para a v8**: esta fatia **confirma fortemente** o claim da Camada 2. Silva 2023 é exatamente o **adversário-pedagógico** que a v8 precisa: livro-texto sério, recente, em português, que ensina estudo de caso e análise histórica comparada **sem** mobilizar nenhum dos elementos da fronteira contemporânea quali-Bayesiana. **A contribuição operacional própria da v8 (DAG-mas-rivais como substituto operacional para U não-observado em pequeno-n) não é antecipada pelo livro — o problema sequer é colocado nesses termos**.

---

## Fatia 6 — §3.3 Métodos mistos + §4 Considerações finais + Referências (pp. 109-119)

### §3.3 Métodos mistos

Organizada em torno de **Creswell (2009)** (tipologia descritiva de seis estratégias) e **Seawright (2016)** (crítica conceitual). Silva endossa Seawright contra triangulação:

> "as abordagens mais contemporâneas a respeito dos métodos mistos substituem a ideia de triangulação (presente na tipologia utilizada por Creswell apresentada anteriormente) pela ideia de integração entre os métodos" (p. 111)

> "O problema fundamental é que os métodos qualitativos e quantitativos não estão realmente fazendo a mesma pergunta, mesmo que foquem no mesmo tópico." (Seawright 2016, p. 7, citado p. 112)

Solução proposta: desenho integrativo multimétodo **"nos quais dois ou mais métodos são combinados cuidadosamente para sustentar uma única e unificada inferência causal"** (p. 112).

**Crítico para o diagnóstico**: a moldura é **inferência causal unificada**, não split identificação/inferência. Silva não distingue função quanti (estimar efeito sob suposições de identificação) da função quali (estabelecer mecanismo, processo, plausibilidade da identificação). Não cita Lieberman (nested analysis), Goertz (multimethod research), Tarrow, Humphreys-Jacobs. Seawright 2016 é o único pivô — e mesmo ele de forma redutora. **Sem Bayes, sem teste de hipóteses concorrentes, sem process tracing como teste, sem IBE**.

### §4 Considerações finais

Reafirma mensagem central como **gestão de escolhas e validade**. Validade tem três dimensões: **construto, interna, externa** (vocabulário psicométrico/Campbell-Stanley). Replicabilidade como tendência crescente.

**Não aparece**: split identificação ≠ inferência. **Não aparece**: Bayes. **Não aparece**: "design" no sentido forte da Credibility Revolution. Vocabulário de validade é Campbell-Stanley/Shadish, não Pearl/Imbens.

> "A validade se refere às três dimensões do trabalho empírico: a validade de construto, a validade interna e a validade externa" (p. 114)

### Lista de referências (categorizada)

**(I) KKV / pós-KKV BR**:
- King, Keohane, Verba (1994)
- Kellstedt, Whitten (2015) — *Fundamentos da Pesquisa em Ciência Política* [tradução BR]
- Brady, Henry (2008) — Causation and Explanation (Oxford Handbook)
- Sekhon, Jasjeet (2008) — Neyman-Rubin model (Oxford Handbook)
- Morton, Williams (2010) — *Experimental Political Science*
- Plümper, Troeger, Neumayer (2010)

**(II) Pearl / DAG / SCM**: **NENHUMA referência explícita.** Sem Pearl, Bareinboim, Hernán, Morgan-Winship, Robins, Greenland, Spirtes/Glymour. **Ausência total do cluster gráfico-estrutural.**

**(III) Credibility revolution / econometria moderna**:
- Abadie, Diamond, Hainmueller (2010) — Synthetic control
- Bertrand, Duflo, Mullainathan (2004) — DiD
- Hahn, Todd, Van der Klaauw (1999) — RDD (NBER)
- Heckman, Hotz (1989); Heckman, Ichimura, Todd (1997)
- Lee (2008) — RDD
- Rosenbaum, Rubin (1983) — Propensity score
- Smith, Vernon L. (2002)
- Clarke (2005, 2009) — omitted variable bias

**Comentário crucial**: aparece o "primeiro andar" da credibility revolution (DiD, RDD, matching, propensity score, synthetic control) — **mas não Angrist-Pischke, Imbens, Cunningham, Athey, Card, Huntington-Klein, Lundberg-Johnson-Stewart**. É um cluster pré-2010, herdado da econometria de avaliação de programas, sem o aparato pedagógico contemporâneo.

**(IV) Quali metodológico moderno (F&C, H&J, Beach-Pedersen, Bennett-Checkel, Slater-Ziblatt)**:
- Seawright (2016)
- Seawright, Gerring (2008)
- Goertz, Starr (2003)
- Vennesson, Pascal (2008)

**Comentário**: **Fairfield-Charman ausente. Humphreys-Jacobs ausente. Beach-Pedersen ausente. Bennett-Checkel ausente. Slater-Ziblatt ausente. Mahoney 2021 ausente.** O quali "moderno" mobilizado é meados-2000 a 2016, **sem o giro Bayesiano explícito**.

**(V) Quali clássico**:
- KKV (1994), Mahoney 2000 (path dependence), Gerring 2004, Skocpol 1979, Thelen 2003, Tilly 2001, Rueschemeyer, Della Porta-Keating 2008, Padget 2017

**(VI) Outros**: filosofia da ciência (Popper, Kuhn, Lakatos, Carnap, Chalmers, Salmon, Bevir, Jackson, Moses-Knutsen), sociologia clássica (Almond-Verba, Durkheim, Weber, Marx Anthony), métodos mistos (Creswell, Feilzer, Johnson, Maxcy, Maxwell, Newman, Small, Reichertz, Abbott, Gorard, Corbetta), política BR substantiva (Aguilar, Avelino-Biderman-Barone, Bueno-Dunning, Corseuil, Figueiredo Argelina, Izumi, Kölln, Nishijima), e auto-citação Silva (2015) RBCS sobre métodos mistos.

### Veredito da fatia 6

**Tipo A (clássico pré-credibility-revolution, com verniz modernizante seletivo)**.

A bibliografia confirma o diagnóstico:
- **Ausência total do cluster (II) Pearl/DAG/SCM**
- Cluster (III) credibility revolution **truncado**: presente em sua camada econométrica pré-2010, ausente em sua camada pedagógico-conceitual contemporânea
- Cluster (IV) quali moderno **seletivo e datado**: Seawright 2016, Goertz/Starr 2003, mas **sem F&C, H&J, Beach-Pedersen, Bennett-Checkel, Mahoney 2021**

A moldura conceitual em §3.3 é "inferência causal unificada" (Seawright), não split identificação/inferência; em §4 é validade tripartite Campbell-Stanley + replicabilidade, sem Bayes e sem design-based identification.

**Para a v8**: Silva 2023 fornece evidência **forte e diretamente citável** do gap BR — manual contemporâneo (2023), de editora oficial (ENAP), com aparato bibliográfico cuidadoso, que **não mobiliza o split em nenhum momento** e cuja síntese de fecho permanece presa ao vocabulário Campbell-Stanley + replicabilidade-como-transparência.
