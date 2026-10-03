# Adjudicação do parecer do ChatGPT — 3 de outubro de 2026

## Identidade do material

O baseline completo foi lido e preservado em `artifacts/baseline_source.Rmd`. Seu SHA-256 é `3d1b06bdb1e35c3ea85baed3a1611741fceebf8e2e8a656afc27378e80e3383a`, idêntico ao conteúdo de `4af3f60:paper_dados_format_quali.Rmd`. A verificação não usa o manuscrito posteriormente editado. O parecer original permanece integral e sem alteração em `quality_reports/parecer_paper_qualitativo_bayesiano.md`, SHA-256 `a662894c0003a6b8340471ed3d5c189d269e034997e8a84d4c691333fd9961d8`.

A cópia exata do upload originalmente lido pelo ChatGPT não foi preservada. As referências convertidas daquele parecer não correspondem à numeração do Rmd. Todas as passagens substantivas criticadas foram reencontradas no baseline; não se presume equivalência de paginação, nem se pode auditar eventual truncamento do upload. O artefato local desta adjudicação é íntegro.

Não se exigiu novo contrato de argumento: trata-se da adjudicação de um parecer existente, sob decisões autorais já documentadas, e não de nova revisão multi-reader. Foram lidos AGENTS.md, handoff de 2026-10-03 e notas completas de releitura. O split identificação/inferência e a posição ontológica do Problema 1 são decisões vinculantes.

## Encaminhamento executivo

Há **5 achados confirmados**, **6 parciais** e **1 não resolvido**. Nenhum achado recebeu REFUTED como status primário; limites e contraprovas de críticas mais amplas estão registrados nos PARTIAL.

O record global é **BLOCKED** por R1-F002, uma decisão substantiva sobre exaustividade. Isso conserva a pendência sem interromper correções independentes. O componente `adjudication_safe_component.json` é **READY_FOR_IMPLEMENTATION** para correções locais e para a apresentação delimitada da regra sequencial, preservando a matriz didática. READY é um veredicto técnico; a autorização vem do pedido atual do autor.

**Tabela 1. Status dos doze achados adjudicados, com distinção entre defeito e solução proposta.**

| ID | Status | Decisão vinculante | Correção proposta |
|---|---|---|---|
| R1-F001 | PARTIAL | sim | safe |
| R1-F002 | UNRESOLVED | não | owner_decision |
| R1-F003 | CONFIRMED | não | safe |
| R1-F004 | PARTIAL | não | needs_design |
| R1-F005 | PARTIAL | não | needs_design |
| R1-F006 | CONFIRMED | não | safe |
| R1-F007 | CONFIRMED | não | safe |
| R1-F008 | CONFIRMED | não | safe |
| R1-F009 | PARTIAL | sim | owner_decision |
| R1-F010 | CONFIRMED | sim | safe |
| R1-F011 | PARTIAL | não | safe |
| R1-F012 | PARTIAL | não | safe |

## Evidência e raciocínio por achado

### R1-F001 — PARTIAL

**Crítica preservada:** Item 1: o paper às vezes desliza entre identificação causal e adjudicação entre explicações; definir validade interna pela enumeração, verossimilhanças e posterior odds não implica identificação do estimando.

**Tipo / dimensão / gravidade proposta:** scope_or_consistency; identificação e escopo; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:121`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:125`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:137`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:367`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:371`.

**Decisão autoral vinculante:** sim; ler os limites abaixo.

**Evidência do defeito:**

- L371 atribui validade interna ao procedimento de comparação e explicitamente a contrapõe ao desenho; L367 define a mesma expressão como propriedade do desenho que torna um estimando recuperável na população.

**Evidência que limita ou refuta parte da crítica:**

- L121 exige solução de desenho para problemas estruturais; L125 desloca o alvo para discriminar explicações; L137 afirma que a comparação não elimina formalmente variável omitida.

**Verificação e raciocínio:** O problema local é uma incompatibilidade terminológica comprovada. A leitura segundo a qual o paper inteiro confunde os dois objetos excede o texto. A decisão do autor de separar identificação e inferência, mesmo com suposições comuns, permanece vinculante.

**Correção proposta:** `safe`. Alinhar L371 e referências locais com a definição L367: credibilidade da explicação comparada depende do procedimento, enquanto identificação do estimando depende do desenho/modelo. Não reestruturar a tese nem qualificar o split decidido pelo autor.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

### R1-F002 — UNRESOLVED

**Crítica preservada:** Item 2: exaustividade seria forte demais; substituir enumeração exaustiva por adequação e robustez do conjunto de rivais.

**Tipo / dimensão / gravidade proposta:** method; escopo do espaço de modelos; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:129`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:139`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:225`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:409`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:411`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L129, L225 e L409 dão centralidade à exaustividade; L139 e L411 a tratam como suposição substantiva indemonstrável.

**Evidência que limita ou refuta parte da crítica:**

Nenhuma.

**Verificação e raciocínio:** A fonte comprova que o autor defende um compromisso de fechamento do conjunto comparado. O parecer propõe abandonar ou enfraquecer esse compromisso, mas não oferece contraprova lógica que o torne inválido. Se a analogia entre suposições de identificação e fechamento do espaço explicativo é adequada é parte substantiva do argumento. O fato de robustez finita não demonstrar exaustividade deve ser explicitado sem converter automaticamente exaustividade em adequação.

**Correção proposta:** `owner_decision`. Preservar a centralidade da exaustividade nesta rodada. Reservar ao autor a decisão sobre enfraquecer ou manter o critério. Corrigir separadamente promessas falsas dos diagnósticos F003.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

### R1-F003 — CONFIRMED

**Crítica preservada:** Item 3: mantendo prioris e verossimilhanças fixas, retirar uma rival só renormaliza; não pode alterar o ranking das hipóteses restantes, salvo retirar a própria vencedora.

**Tipo / dimensão / gravidade proposta:** logic_or_proof; inferência e diagnóstico de robustez; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:413`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L413 promete que retirar cada rival e observar mudança de top-1 identifica rival decisiva e que estabilidade a todas as remoções seria robustez local. Para pesos w_i=P(H_i)P(E|H_i), a razão após retirar k é w_i/w_j para i,j diferentes de k.

**Evidência que limita ou refuta parte da crítica:**

Nenhuma.

**Verificação e raciocínio:** O denominador comum cancela nas odds. A checagem com as seis hipóteses confirma invariância de todas as odds e do ranking restrito; H6 sai da liderança apenas quando é removida. A impossibilidade independe dos valores particulares do impeachment. Inserir uma rival também não reordena as antigas: a nova hipótese pode superar a vencedora, e esse é o diagnóstico legítimo de uma adição.

**Correção proposta:** `safe`. Retirar a interpretação impossível do leave-one-rival-out. Se substituir por leave-one-evidence-out, descrevê-lo como sensibilidade à evidência, separado da cobertura do conjunto de rivais. Manter enumeração incremental/adversarial sem afirmar que estabilidade prova exaustividade.

**Verificações mecânicas:** `checks.R`, execução PASS registrada em `checks_output.txt`.

### R1-F004 — PARTIAL

**Crítica preservada:** Item 4, primeira parte: H6 composta recebe vantagem de flexibilidade frente a rivais estreitas; prioris iguais deixam a complexidade sem tratamento e seria necessário um Occam penalty.

**Tipo / dimensão / gravidade proposta:** method; especificação de hipóteses e prioris; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:283`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:297`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:299-306`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:326-342`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:348-357`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L297 classifica cinco hipóteses como monocausais/parciais e a sexta como composta; L336 fixa prioris iguais. Nenhum parâmetro de flexibilidade ou elicitação de custo de complexidade é modelado.

**Evidência que limita ou refuta parte da crítica:**

- L283 declara números didáticos, não estimados empiricamente; L342 condiciona a conclusão à parametrização. A matriz contém probabilidades fixas, não uma família paramétrica explicitamente ajustada aos dados.

**Verificação e raciocínio:** A preocupação com ad hoc e complexidade é sustentada pela fonte primária de Fairfield–Charman (2017), §3.4 p.11 e App.A pp.22–23: a priori requer background e o tratamento de flexibilidade depende da família de hipóteses. Isso não fornece uma penalidade numérica obrigatória para H6 nem demonstra intenção de fazê-la vencer. Aritmética e transparência didática do baseline estão corretas. A comparação substantiva continua subdeterminada. [Fairfield–Charman (2017), §3.4 e App.A](https://cpd.berkeley.edu/wp-content/uploads/2018/02/CPC_Fairfield.pdf).

**Correção proposta:** `needs_design`. Não impor desconto arbitrário a H6 nem remodelar rivais sem decisão substantiva. É seguro explicitar a prior uniforme como cenário didático e mostrar limiar de prior odds; uma penalização ou comparação empírica exige definir família, background e parâmetros.

**Verificações mecânicas:** `checks.R`, execução PASS registrada em `checks_output.txt`.

### R1-F005 — PARTIAL

**Crítica preservada:** Item 4, segunda parte: E4–E7 dificilmente seriam condicionalmente independentes; multiplicar probabilidades marginais pode repetir informação. Usar a regra sequencial de probabilidades.

**Tipo / dimensão / gravidade proposta:** method; dependência entre evidências; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:315-318`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:322`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:336`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:338-340`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- E4–E6 incluem partidos, fraturas, Cunha, Lava Jato e Temer; o produto de probabilidades marginais em L336 requer independência condicional que não é justificada substantivamente.

**Evidência que limita ou refuta parte da crítica:**

- L336 explicita a independência como suposição simplificadora. A descrição de eventos relacionados não prova, sozinha, dependência estatística condicional dada cada hipótese. Não há dados/modelo conjunto que permita medir a dupla contagem.

**Verificação e raciocínio:** A crítica confirma uma limitação relevante, não uma demonstração empírica de dependência. A regra P(E|H)=P(E1|H) produto P(Ek|E anteriores,H) é exata. Reutilizar a mesma matriz como se os números já fossem incrementais apenas mudaria rótulos e não justificaria os valores. Fairfield–Charman (2017), Eq.4 p.8 e Eq.5 p.13 usam condicionamento sequencial. [Fairfield–Charman (2017), Eq.4 e Eq.5](https://cpd.berkeley.edu/wp-content/uploads/2018/02/CPC_Fairfield.pdf).

**Correção proposta:** `needs_design`. É seguro adicionar a regra geral e delimitar o produto atual ao modelo independente didático. Para remover a suposição ou reportar novos números, elicitar probabilidades condicionais ou definir modelo conjunto. Não relabelar os números marginais como incrementais.

**Verificações mecânicas:** `checks.R`, execução PASS registrada em `checks_output.txt`.

### R1-F006 — CONFIRMED

**Crítica preservada:** Item 5: duas hipóteses rivais não implicam P(Hi)+P(Hj)=1; para usar essa identidade na comparação binária é preciso estabelecer exclusividade mútua e exaustividade conjunta.

**Tipo / dimensão / gravidade proposta:** false_statement; especificação do espaço de hipóteses; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:215`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:219`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:306`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L215 enuncia soma 1 como significado matemático de rivais. Eventos que omitem alternativas ou se sobrepõem não formam uma partição binária.

**Evidência que limita ou refuta parte da crítica:**

Nenhuma.

**Verificação e raciocínio:** As condições de partição bastam para derivar a soma 1; rivais substantivas não as garantem. Contraexemplos aritméticos registrados. A definição didática de índice único de modelo comparado pode ser preservada se assumida explicitamente, mas não transforma mecanismos coexistentes em eventos excludentes por si só. Soma numericamente igual a 1 também não caracteriza uma partição sem informação adicional.

**Correção proposta:** `safe`. Qualificar a identidade em L215 por exclusividade e exaustividade no conjunto comparado; distinguir mecanismos que coexistem de alternativas completas de modelo. Preservar a estipulação didática de L306 com seu limite. Não abandonar automaticamente posteriors normalizados.

**Verificações mecânicas:** `checks.R`, execução PASS registrada em `checks_output.txt`.

### R1-F007 — CONFIRMED

**Crítica preservada:** Item 6, primeira parte: Humphreys–Jacobs não exigem que todas as variáveis sejam binárias; o binário é o caso simples e há generalização a variáveis não binárias.

**Tipo / dimensão / gravidade proposta:** false_statement; fidelidade à fonte e mensuração; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:231`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:235`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L231 diz que é necessário para fins práticos tratar todas as variáveis qualitativas como binárias e atribui inviabilidade a contínuas/pequeno n.

**Evidência que limita ou refuta parte da crítica:**

Nenhuma.

**Verificação e raciocínio:** Integrated Inferences, §2.1.2, admite variáveis discretas com mais de dois valores e explica o crescimento do número de tipos. O enunciado de necessidade binária é diretamente refutado pela fonte. A leitura feita não estabelece que qualquer análise contínua seja viável; a correção deve restringir-se ao que o livro permite. [Integrated Inferences, §2.1.2](https://integrated-inferences.github.io/book/02-causal-models.html).

**Correção proposta:** `safe`. Apresentar binariedade como simplificação de exposição/implementação, não requisito universal. Mencionar generalização não binária discreta e custo computacional. Não prometer estimabilidade genérica de variáveis contínuas.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

### R1-F008 — CONFIRMED

**Crítica preservada:** Item 6, segunda parte: a query causal de um caso pergunta por seu tipo; proporção de tipos na população é outra quantidade, usada para queries populacionais.

**Tipo / dimensão / gravidade proposta:** false_statement; definição do alvo inferencial; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:243`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:245`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:247`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L245 começa com tipo causal do Brasil sob Lula III e termina definindo seu estimando como proporção de crônicos em população comparável.

**Evidência que limita ou refuta parte da crítica:**

Nenhuma.

**Verificação e raciocínio:** Integrated Inferences distingue §4.1 (probabilidade do tipo do caso) e §4.3 (proporções lambda populacionais; ATE como proporção de positivos menos negativos). Essa diferença de alvo é explícita. Um modelo hierárquico pode ligar theta e lambda, mas não torna as duas perguntas idênticas. [Integrated Inferences, §4.1 e §4.3](https://integrated-inferences.github.io/book/04-causal-questions.html).

**Correção proposta:** `safe`. Definir a query do caso pela probabilidade posterior de seu tipo, condicional à evidência/modelo. Se preservar a referência a proporções populacionais, apresentá-la como alvo distinto. Manter o exemplo de Lula e a codificação do outcome.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

### R1-F009 — PARTIAL

**Crítica preservada:** Item 7, primeira parte: representabilidade de INUS/SUIN em função estrutural não demonstra mesma lógica inferencial/estimando; a metáfora vestidos da mesma máquina é mais ampla.

**Tipo / dimensão / gravidade proposta:** scope_or_consistency; causalidade versus inferência; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:45`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:61`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:79`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:149`.

**Decisão autoral vinculante:** sim; ler os limites abaixo.

**Evidência do defeito:**

- L61 chama as três famílias de vocabulários de uma mesma estrutura inferencial; a igualdade de representação causal em L79 não deriva essa unidade de inferência.

**Evidência que limita ou refuta parte da crítica:**

- L45 distingue explicitamente lógica causal comum de estimação e inferência por caminhos distintos. A decisão registrada no Problema 1 é precisamente negar diferença ontológica necessária, preservando perguntas distintas.

**Verificação e raciocínio:** O excesso local existe na expressão estrutura inferencial e na metáfora, mas o parecer não autoriza enfraquecer a tese do autor sobre uma lógica causal comum. Sua correção é dependente da implementação do Problema 1 e deve seguir aquela decisão.

**Correção proposta:** `owner_decision`. Encaminhar apenas o descompasso local para a edição já autorizada do Problema 1. Preservar causalidade comum e inferências distintas; não substituir isso por pluralismo ontológico nem por lógica inferencial única.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

### R1-F010 — CONFIRMED

**Crítica preservada:** Item 7, segunda parte: tudo é crença e a distinção entre propriedades do mundo e estado epistêmico perde tração é uma tese filosófica mais ampla que representar incerteza sobre suposições.

**Tipo / dimensão / gravidade proposta:** scope_or_consistency; interpretação epistêmica da probabilidade; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:113`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:115`.

**Decisão autoral vinculante:** sim; ler os limites abaixo.

**Evidência do defeito:**

- L115 diz que suposições deixam de ser propriedades verificáveis da realidade externa e dissolve a distinção realidade/crença; L113 havia mantido parâmetros fixos desconhecidos e possibilidade de processos físicos estocásticos.

**Evidência que limita ou refuta parte da crítica:**

Nenhuma.

**Verificação e raciocínio:** A probabilidade subjetivista representa crença em proposições sobre o mundo; disso não se segue que as proposições deixem de descrever propriedades do mundo ou que fatos e estados epistêmicos sejam iguais. A inconsistência é local e não exige resolver a ontologia. A escolha autoral de permitir choques estocásticos no mundo em model-based deve ser preservada.

**Correção proposta:** `safe`. Trocar o passo inferencial indevido por crenças sobre suposições estruturais e sua incerteza. Não declarar inexistência de aleatoriedade física, não apagar a escolha ontológica do Problema 1. Integrar com a edição dessa seção pelo agente principal.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

### R1-F011 — PARTIAL

**Crítica preservada:** Seção sobre prioris: uniformidade 1/K não é neutra e depende da partição do espaço de hipóteses; retirar recomendação geral de prioris não-informativas.

**Tipo / dimensão / gravidade proposta:** method; elicitação de prioris e sensibilidade; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:187`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:191`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:195`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:269`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:336`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L191 identifica não-informatividade com hipóteses equiprováveis; L195 recomenda seu uso como forma de evitar inflação/viés. Uniformizar depois de refinar uma explicação redistribui massa de probabilidade por decisão taxonômica.

**Evidência que limita ou refuta parte da crítica:**

- L193 admite conhecimento substantivo; L195 recomenda robustez com outras prioris. O paper não elimina prioris informativas nem nega sensibilidade.

**Verificação e raciocínio:** A neutralidade/invariância de uma prior discreta uniforme é matematicamente falsa. O contraexemplo preserva likelihoods e muda a posterior agregada de A=0,6 para 0,857 ao subdividir A em quatro variantes e uniformizar cinco modelos. Preservando a massa original, recupera 0,6. A fonte Fairfield–Charman 2017 §3.2 p.6 condiciona prioris a background; a leitura completa do artigo recente citado no parecer não foi obtida, e não fundamenta esta adjudicação. [Fairfield–Charman (2017), §3.2](https://cpd.berkeley.edu/wp-content/uploads/2018/02/CPC_Fairfield.pdf).

**Correção proposta:** `safe`. Distinguir prioris uniformes de não-informatividade e apresentar uniformidade como cenário explicitado. Manter elicitação e sensibilidade, sem eleger faixa razoável de prior odds sem background substantivo.

**Verificações mecânicas:** `checks.R`, execução PASS registrada em `checks_output.txt`.

### R1-F012 — PARTIAL

**Crítica preservada:** Passagens sobre contribuição e estrutura: Spirling–Stewart já admitem IBE com parâmetros não identificados e evidência qualitativa; a distinção não pode ser eles fazem IBE apenas depois da identificação.

**Tipo / dimensão / gravidade proposta:** scope_or_consistency; fidelidade à literatura e contribuição; major.

**Localização:** `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:20`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:45`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:131`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:143`; `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd:145`.

**Decisão autoral vinculante:** não indicada para este finding.

**Evidência do defeito:**

- L143 caracteriza IBE deles como passo seguinte da inferência identificada para explicação; L145 contrapõe organizar a adjudicação empírica à complementação apenas teórica.

**Evidência que limita ou refuta parte da crítica:**

- L145 reconhece escopo aberto para qualitativo e foco deles em regressão. L45 já reivindica operacionalização Bayesiana/restrições qualitativas, uma diferença possível de escopo.

**Verificação e raciocínio:** O texto primário de Spirling–Stewart, versão de 2/7/2024, pp.2 e 5, afirma que estimar parâmetro identificado não é necessário para produzir evidência de alegação causal e inclui evidência qualitativa de mecanismo; pp.21–23 trata evidência imperfeita. A atribuição estreita de L143 está confirmadamente equivocada. Isso não prova a inexistência de contribuição operacional do paper nem autoriza uma nova reivindicação de originalidade sem revisão própria. [Spirling–Stewart, versão de 2/7/2024, pp.2 e 5](https://arthurspirling.org/documents/whatgood.pdf).

**Correção proposta:** `safe`. Corrigir a descrição da posição deles em resumo/seção específica. Enunciar a operacionalização qualitativa como objeto desta nota e não como extensão de uma exclusão que eles não fazem. Preservar a tese e evitar promessa de novidade demonstrada.

**Verificações mecânicas:** Comparação textual da fonte e do contexto, sem teste estatístico empírico.

## Verificações reproduzidas

O script `checks.R` usa uma cópia do script canônico do impeachment, preservada em `artifacts/impeachment_bayes_example_baseline.R` (SHA-256 `a9b1e3ce6e2555336631e8ce334e9e8236c017a80fd4a78fcf6f71e7e5b06e65`). O script canônico não foi editado. Execução da raiz do repositório:

```bash
Rscript quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/checks.R
```

Os posteriors das duas tabelas e os decibéis foram reproduzidos com o arredondamento publicado. Remover cada rival preservou ranking restrito e todas as odds das restantes; o erro numérico máximo foi de 5,7 × 10⁻¹⁴. A preferência por H6 só desapareceu ao retirar H6.

Se os pesos não normalizados são \(w_i=P(H_i)P(E\mid H_i)\), para quaisquer \(i,j\ne k\):

\[
\frac{P(H_i\mid E,H_k\text{ removida})}{P(H_j\mid E,H_k\text{ removida})}=\frac{w_i}{w_j}.
\]

O leave-one-evidence-out do modelo independente manteve H6 em primeiro nas sete remoções. Sua posterior variou de 0,568 a 0,652. Isso é sensibilidade interna ao cenário e não verificação da independência nem validação substantiva de Limongi. O Bayes factor H6/H4 foi 3,6886; a comparação par a par muda a favor de H4 se prior odds H6/H4 forem menores que 0,2711, mantendo likelihoods fixas.

No contraexemplo de partição, A/B com prioris iguais e likelihoods 0,6/0,4 produz P(A|E)=0,6. Subdividir A em quatro variantes com a mesma likelihood e uniformizar as cinco alternativas produz posterior agregada 0,857. Distribuir entre as quatro variantes a massa original de A recupera 0,6. O objetivo é demonstrar dependência da partição, não escolher prioris para o impeachment.

A regra geral é:

\[
P(E\mid H)=P(E_1\mid H)\prod_{k=2}^{K}P(E_k\mid E_1,\ldots,E_{k-1},H).
\]

A matriz de base contém marginais \(P(E_k\mid H)\). Para uma aplicação sequencial, as condicionais incrementais precisam ser reavaliadas ou derivadas de um modelo conjunto. Trocar apenas o rótulo das colunas não trata dependência. Nenhum número da matriz foi reinterpretado nesta adjudicação.

## Decisões autorais e correções perigosas

- **R1-F002:** abandonar exaustividade ou reduzir sua centralidade muda o argumento principal. A crítica permanece aberta para decisão do autor.
- **R1-F004:** descontar a priori de H6 sem definir background, família e flexibilidade acrescenta uma arbitrariedade. A recomendação de considerar complexidade é pertinente; o desconto concreto não foi estabelecido.
- **R1-F005:** reapresentar os números marginais como condicionais incrementais seria uma mudança silenciosa de significado. Apenas a orientação geral está pronta.
- **R1-F009/R1-F010:** preservar causalidade comum e distinção entre conhecimento e aleatoriedade física. A implementação desses trechos pertence ao Problema 1 já autorizado; sua adjudicação não reabre decisões.
- **R1-F012:** corrigir a atribuição a Spirling–Stewart não demonstra por si só originalidade ou falta de originalidade da contribuição operacional.

## Limites desta adjudicação

Não se refez o estudo substantivo do impeachment, não se elicitaram likelihoods ou prioris empíricas, não se mediu dependência condicional e não se leu integralmente o livro Fairfield–Charman. As seções necessárias das fontes primárias consultadas estão documentadas em `primary_sources.json`, com URLs e data de acesso. O artigo recente sobre replicação teve acesso insuficiente para checar sua afirmação específica sobre prior odds; a crítica das prioris foi verificada matematicamente e pelo preprint primário de 2017.

## Veredicto

**Global: BLOCKED**, com decisões abertas preservadas. **Componente seguro: READY_FOR_IMPLEMENTATION**. O JSON é o record autoritativo. A implementação deve renderizar toda prosa nova em amarelo no PDF, conforme pedido do autor, e depois passar por conferência independente dos trechos alterados.

