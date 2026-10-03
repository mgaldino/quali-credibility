# Encaminhamento seguro delimitado — 3 de outubro de 2026

Este componente deriva de `adjudication_round1.json`, que permanece **BLOCKED**. O baseline e os achados mantêm os IDs e o hash originais. Nenhum diagnóstico desfavorável foi apagado do record global.

## Escopo e independência

- As alterações locais não dependem de abandonar exaustividade: o argumento permanece condicionado ao conjunto comparado.
- Corrigir renormalização e a interpretação do protocolo não prova nem nega exaustividade.
- Qualificar binário, query de caso e soma de rivais preserva o alvo qualitativo e a tese Limongi.
- Orientação sobre condicionamento sequencial não reinterpreta nem altera matriz didática de marginais.
- Sensibilidade com prior odds usa a mesma matriz e explicita condicionamentos, sem impor penalização de complexidade.
- A descrição de Spirling–Stewart é corrigida pela fonte; isso não declara inexistência da contribuição autoral.

## Ações excluídas

- Reformular a tese de exaustividade.
- Aplicar penalização arbitrária a H6 ou construir nova família de modelos.
- Reelicitar ou relabelar marginais da tabela como condicionais incrementais.
- Reestruturar o manuscrito em torno de nova contribuição sem decisão autoral.

**Tabela 2. Achados encaminhados para implementação delimitada.**

| ID | Status | Decisão vinculante | Correção proposta |
|---|---|---|---|
| R1-F001 | PARTIAL | sim | safe |
| R1-F003 | CONFIRMED | não | safe |
| R1-F005 | PARTIAL | não | safe |
| R1-F006 | CONFIRMED | não | safe |
| R1-F007 | CONFIRMED | não | safe |
| R1-F008 | CONFIRMED | não | safe |
| R1-F010 | CONFIRMED | sim | safe |
| R1-F011 | PARTIAL | não | safe |
| R1-F012 | PARTIAL | não | safe |

## Limites por achado

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

**Correção proposta:** `safe`. Parte segura delimitada: acrescentar a regra sequencial e explicitar que o produto atual é uma simplificação sob independência. Em aplicação substantiva, reelicitar P(Ek|E anteriores,H) ou definir likelihood conjunta. A matriz permanece de marginais e todos os números de base ficam preservados; nenhum número deve ser tratado silenciosamente como incremental.

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


## Veredicto

**READY_FOR_IMPLEMENTATION** apenas para as intervenções descritas. As decisões sobre exaustividade e a comparação substantiva de complexidade/dependência continuam no record global. Este componente não autoriza descarregar números de marginais como condicionais incrementais, nem alterar a matriz do impeachment. A prosa nova deve aparecer em amarelo no PDF.

