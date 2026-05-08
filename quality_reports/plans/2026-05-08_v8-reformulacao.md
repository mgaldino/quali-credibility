# Plano: Reformulação v8 do paper "Quali Credibilidade"

**Status**: DRAFT
**Data**: 2026-05-08
**Próxima sessão**: dedicada, em `quali-credibility/` (não na pasta-pai)

## Contexto

- **Manuscrito atual**: `paper_dados_format_quali_v7.Rmd` (Jun/2025)
- **Status no journal**: v7 rejeitada na BPSR; editor convidou ressubmissão
- **Diagnóstico Edmans (sessão 2026-05-08)**: `quality_reports/2026-05-08_edmans-review.md` — score global 4.7/10
- **Pareceres BPSR**: avaliados pelo autor como "ruins ou coincidentes com Edmans"; **não serão usados** como input substantivo da v8
- **Sessão atual** apenas plano; execução em sessão dedicada (4-8 semanas)

## Tese central (definida em conversa 2026-05-08, recalibrada após lit-review intl/BR + Silva 2023 + Spirling-Stewart 2025)

**Camada 1 — premissa (não vender como descoberta):**
Identificação causal e inferência estatística são problemas distintos. **Doutrina consolidada na metodologia quantitativa internacional** (Angrist-Pischke 2009; Lundberg-Johnson-Stewart 2021; Imbens 2020, 2022; recentemente reafirmado em Spirling-Stewart 2025 forthcoming JoP) e **em consolidação crescente na metodologia qualitativa via virada Bayesiana** (Fairfield-Charman 2017, 2022; Humphreys-Jacobs 2015, 2023) e inferência à melhor explicação (Lipton 2004), embora ainda conviva com tradições que tratam o problema como integrado (Brady-Collier 2010; Mahoney 2010, 2021).

**Camada 2 — contribuição-de-tradução para a comunidade BR:**
A literatura metodológica brasileira começa a importar termos isolados do design-based identification (Silva 2023 ENAP nomeia "estratégias de identificação" em §3.1.1 e apresenta RDD/PSM/synth/DiD em §3.1.2 com intuição moderna correta) **mas sem incorporá-lo como princípio organizador**: a §2.3 Causalidade do mesmo manual é construída sobre Brady (2008) — pluralismo filosófico pré-credibility-revolution — e a §3.2 N-pequeno opera em vocabulário Vennesson-Rueschemeyer-Mill-Skocpol-path dependence sem citar uma única vez Bayes/F&C/H&J/Bennett-Checkel/Beach-Pedersen/process tracing. **O gap não é de vocabulário (alguns termos chegaram), mas de arquitetura conceitual** — o split como princípio organizador, e a virada Bayesiana qualitativa, simplesmente não estão no horizonte da pedagogia metodológica BR. Substanciação: 8 refs Tipo A (Rezende 2017, 2019; Mesquita 2017; Paula 2018; Leite & Rocha 2019; Figueiredo Filho et al. 2021; Bachini & Chicarino 2018; Amorim Neto & Rodriguez 2016) + 4 Tipo B (Rezende 2011; Sposito et al. 2022; Vick & Lavalle 2020; Perissinotto 2024) + 0 Tipo C confirmadas em periódico CP/RI BR + Silva 2023 ENAP como peça central.

**Camada 3 — contribuição operacional própria (afiada após reflexão sobre SS 2025):**

A revolução da credibilidade construiu um critério de identificação ancorado em **tecnologias de desenho** — atribuição aleatória, instrumentos, descontinuidades, tendências paralelas, doadores sintéticos — todas operando sobre múltiplas observações para tornar plausível a suposição de **ignorabilidade** (ausência de U relevante condicional ao desenho). Em desenhos qualitativos pequeno-n (N=1, N=2, N=3), essas tecnologias **perdem operacionalização**: não há grau de liberdade para "controlar U" via design. A objeção padrão da CR ("e se houver um U não-observado?") admite suprimento infinito de variáveis inventáveis sem critério para falseá-las.

A v8 propõe que **IBE em quali small-n é alternativa funcional à CR, não seu mero complemento**. O threat relevante desloca-se: deixa de ser "U omitido" e passa a ser "explicação rival não considerada". O critério de credibilidade desloca-se: deixa de ser "plausibilidade de ignorabilidade via desenho" e passa a ser **enumeração exaustiva do conjunto de rivais + comparação Bayesiana de seus posteriors sob critério IBE** (Lipton 2004).

**Diferenciação relativa a Spirling-Stewart (2025):** SS argumentam que IBE é o framework do **passo teórico** (mover-se do parâmetro identificado para a explicação) em **toda** pesquisa empírica, inclusive aquelas com identificação CR rigorosa. SS preserva CR onde funciona; IBE em SS é complemento, não substituto. **A v8 vai além**: argumenta que em quali pequeno-n, onde a CR proper não tem operacionalização, IBE não complementa mas **substitui funcionalmente** o critério de credibilidade. SS, focados em regressão, deixam esse caso aberto (footnote 2: *"a version of what we argue applies much more generally (e.g. to qualitative evidence)"*) — é exatamente o espaço onde a v8 contribui.

**Implicações operacionais:**
- A objeção de credibilidade contra estudo qualitativo causal não é "e se houver um U?" — vira "sua enumeração de rivais foi exaustiva o suficiente?"
- O critério de credibilidade quali é a **robustez da comparação de rivais**, não o controle de confundidores via design.
- Process tracing Bayesiano (F&C) e causal queries (H&J) são **duas implementações** dessa lógica — não a contribuição.
- A recomendação F&C de redefinir escopo continuamente é problemática **porque subverte a finitude do conjunto de rivais**, que é o que dá força ao critério.

## Decisões já tomadas — NÃO REABRIR

| Alternativa | Decisão | Razão |
|---|---|---|
| Vender "KKV errou" como meta-tese | REJEITADO | Overclaim — fronteira internacional já sabe; só BR não absorveu. Sintetizar para BR, não anunciar descoberta global |
| Posicionar como síntese pedagógica OU contribuição original (parecer Edmans #1) | REJEITADO em forma de OU; ADOTADO em forma de camada-dupla | Síntese para BR + ponto operacional sobre rivais. Honesto, calibrado |
| Crítica F&C escopo como contribuição central | REJEITADO | Vira corolário/aplicação da tese sobre finitude de rivais |
| Ler pareceres BPSR antes da v8 | REJEITADO | Autor avaliou: ruins ou coincidem com Edmans |
| Worked example apenas como ilustração técnica de Bayes | REJEITADO | Função decisiva: demonstrar enumeração de rivais e mostrar que objeção "U" vira "rival adicional" |
| INUS/SUIN como subseção (linhas 75-77 v7) | CORTAR ou reduzir a uma frase | Não serve à tese; tem erros lógicos no exemplo |
| Bayes 101 como subseção (linhas 95-107 v7) | CORTAR 90% | Audiência BPSR não precisa; quem precisa não terá condição de avaliar PT depois |

## Abordagem

Reescrita estrutural, não revisão linha-a-linha. A v8 é nova arquitetura argumentativa, não v7 com cosmética.

Ordem da execução: **estrutura argumentativa → preencher conteúdo → mecânica (proofread + bib)**. Inverter essa ordem perde tempo arrumando texto que será cortado.

## Prioridades em ordem de impacto

### P1 — Reescrita do abstract (camada-dupla explícita)

Primeiras frases devem trazer:
1. Tese central operacional (rivais como substituto de DAG-U)
2. Audiência declarada (comunidade BR; gap da fronteira)
3. Sem "novo enquadramento" / "aplicação crítica no ensino e pesquisa" (vagos)

Sugestão de abertura:
> "Esta nota tem dois objetivos. Primeiro, sistematizar para a ciência política brasileira a separação — já consolidada na fronteira metodológica internacional — entre identificação causal e inferência estatística, e suas implicações para o debate qualitativo-quantitativo. Segundo, especificar o que substitui, em desenhos qualitativos, a manobra de controle de variáveis omitidas que está disponível para desenhos quantitativos: a comparação Bayesiana de hipóteses rivais sob o critério de inferência à melhor explicação disponível."

### P2 — Reestruturação por arco argumentativo (não tópico)

| Seção atual (v7) | Função na v8 |
|---|---|
| Introdução | Tese-erro pós-KKV no debate metodológico BR + roadmap |
| Recepção qualitativa | Como a literatura BR aceitou a moldura inferencial errada (precisa cit BR) |
| Revolução da credibilidade | Como o quanti separou identificação de inferência |
| Identificação causal | Identificação é propriedade do desenho, não do n |
| ~~INUS/SUIN~~ | **Cortar** ou reduzir a uma frase |
| Inferência Bayesiana | Por que pequeno-n não é problema inferencial |
| ~~Fundamentos Bayes~~ | **Cortar 90%** |
| **NOVO: O que substitui DAG+U no quali** | Enumeração de rivais + IBE como operacionalização. **Centro do paper.** |
| PT Bayesiano + queries causais | Duas implementações concretas da operacionalização |
| Comparando F&C e H&J | F&C escopo movel como **violação** da finitude de rivais (aqui entra a crítica como corolário) |
| **NOVO: Trade-offs honestos** | Elicitação, replicabilidade, teatro Bayesiano, treinamento, quando NÃO usar |
| **NOVO: Worked example brasileiro** | Caso BR completo: rivais → priori → verossimilhança em decibéis → posterior → sensibilidade |
| Transportabilidade | O que sobra: validade externa, igualmente limitada |
| Considerações finais | Reset: o debate pós-KKV foi mal-enquadrado para a comunidade BR |

### P3 — Subseção explícita "DAG + U vs. enumeração de rivais"

Centro operacional do paper. Deve cobrir:
- Por que em quanti a manobra "vou apontar um U" funciona como crítica padrão (sempre disponível, mas há respostas: IV, design, sensitivity analysis)
- Por que em quali essa manobra perde sentido (não há grau de liberdade do desenho para fechar backdoor com U inventado)
- Reformulação: a ameaça em quali é "explicação rival não-considerada", não "U não-observado"
- Critério de credibilidade quali: enumeração exaustiva + comparação Bayesiana
- Conexão com Slater-Ziblatt, Spirling-Stewart, Lipton (IBE), F&C posterior odds
- **Resolve a contradição linhas 165 vs 203 do v7** (rivais não eliminam OVB no sentido quanti — reformulam o problema)

### P4 — Cobertura de literatura brasileira

Argumento da camada 2 (BR não absorveu) precisa ser **substanciado**, não retórico. v7 cita apenas Amorim & Rodriguez 2016. Para v8:

- Pesquisar literatura metodológica BR dos últimos 10-15 anos (DADOS, BPSR, RBCS, Opinião Pública, Lua Nova). Skill `lit-review`.
- Identificar 5-8 trabalhos que **operam dentro da moldura inferencial pós-KKV** sem o split identificação/inferência. Citar como evidência do gap.
- Se houver trabalhos BR que já fazem o movimento, citar como avanços parciais. Se não houver, declarar.
- Limongi, Marenco, Oliveira-Vieira, manuais de metodologia em CP no Brasil — onde quer que o argumento esteja.
- **Sem essa cobertura, o claim do gap fica retórico** e é vulnerável a parecerista que pede "quem confunde?".

### P5 — Worked example brasileiro completo

Função: demonstrar a tese operacional. Não é "aplicação técnica de Bayes ao Brasil".

Estrutura sugerida:
1. Caso (sugestões: impeachment Dilma 2016, transição democrática 1985, ascensão Bolsonaro 2018, escolha de algum estudo qualitativo BR existente para reanálise)
2. Lista explícita de hipóteses rivais (3-5) consideradas pela literatura
3. Prioris (não-informativas como default; informativas se houver justificativa)
4. 2-3 evidências centrais; verossimilhanças em decibéis (à la F&C)
5. Posterior odds entre rivais
6. Análise de sensibilidade (alternar prioris)
7. **Discussão**: a objeção "e se houver um U?" não é a objeção certa contra esse estudo. A objeção certa é "qual rival adicional não foi considerado?" e a resposta vai para a expansão do conjunto

### P6 — Seção de trade-offs honesta

Subseção explícita "Limitações Práticas das Abordagens Bayesianas Qualitativas":
- Custo de elicitação de prioris
- Replicabilidade entre pesquisadoras
- Curva de aprendizado e treinamento (especialmente em programas BR sem fundamentação estatística forte)
- Risco de "teatro Bayesiano" — formalismo sem disciplina
- Quando NÃO usar (ex: estudos descritivos; quando o conjunto de rivais não é finito ou bem definido; etc.)
- Comparação com PT não-Bayesiano (Bennett, Collier): o que se ganha, o que se perde

### P7 — Conserto de inconsistências conceituais (Execution)

Pontos do parecer que **persistem** independentemente da nova tese:

- **Validade interna em UM sentido apenas**: adotar definição Pearl/Rubin (identificação) ou Campbell/McDermott (adequação amostral) e ser consistente. Sugestão: adotar Pearl/Rubin para o argumento técnico; usar "validade interna" no sentido Campbell apenas quando explicitamente referenciar essa tradição.
- **Substituir "ortogonal"** por "logicamente distinto" ou "separável" (linhas 113, 243).
- **Substanciar a acusação da linha 209** ("a literatura qualitativa metodológica tem feito confusão") com citação específica, ou reformular.
- **Tautologia condicional linhas 81 e 211-213**: a tese operacional sobre rivais resolve isso — quali identifica via comparação de rivais (não por suposição). Mas a redação precisa fazer esse passo aparecer.
- **Tratar F&C com caridade** na crítica de escopo: distinguir o que F&C recomendam do que é consequência operacional indesejável.
- **Skocpol como exemplo retórico** (linhas 162-165, 234): substituir por exemplo construído pelo próprio autor (idealmente o worked example).
- **Mahoney-Goertz / controle sintético** (linha 57): contraponto frágil; rever ou cortar.

### P8 — Mecânica (Exposition) — fazer **antes** de finalizar

Skills: `proofread` + `validate-bib`. Ordem: depois da reestruturação argumentativa, antes da compilação final.

Mínimo:
- [ ] YAML linha 4: `Manoel Galino` → `Manoel Galdino`
- [ ] Equação linha 121: `\frac{P(H_jP(E|H_j)}` → `\frac{P(H_j)P(E|H_j)}`
- [ ] Exemplo INUS linha 77 (se mantiver subseção): corrigir Y(0,0,1)=0 para Y(0,0,0)=0; remover parêntese sobrando em Y(1,1,1)=1)
- [ ] Typos: "Por outro outro lado" (45), "Potanto" (211), "acabouço" (222), "A pos / conslidou" (241), "comunicais" (224), "qual a causal" (224)
- [ ] Concordância: argumenta→argumentam (57); das critica sao a→das criticas e a (59); esta organizada→esta organizado (51)
- [ ] Citações: padronizar `;` em listas; corrigir `]]` duplo (209); converter `(@autor)` em `[@autor]` (129); padronizar `Bennet (2015)` para citação formal
- [ ] Verificar autores possivelmente errados: `Forozish_2024` (Furszyfer?), `Goldsmith_2024` (Goldsmith-Pinkham?), `Card_2022`, `Bennet_2015`, `spirling_stewart2025`
- [ ] Padronizar chaves `fairfield_charman_2022` / `_2023` / `_2025` (umas com underscore antes do ano, outras sem)
- [ ] Padronizar terminologia "process tracing" / "rastreio de processo" / "rastreamento de processos"

## Arquivos a modificar

- [ ] **Manuscrito**: usar `paper-version` skill para criar v8 a partir de v7 via git tag (NÃO criar `paper_dados_format_quali_v8.Rmd` ad hoc — versionamento por git)
- [ ] **`Quali-credibilidade.bib`**: adicionar 5-8 entradas BR; corrigir nomes de autores duvidosos (Forozish, Goldsmith, Bennet, Spirling-Stewart); padronizar chaves Fairfield-Charman; verificar todas as chaves citadas existem
- [ ] **README.md**: corrigir typo "qualitativie" → "qualitative"
- [ ] **Sem mexer**: arquivos `.pages`, `paper_revision.docx`, `Carta a editora`, anexos institucionais — ver CLAUDE.md

## Verificação (gates antes de declarar v8 pronta)

- [ ] **Score Edmans esperado**: rodar `edmans-review` na v8. Gate: ≥ 6.5 global; ≥ 6 em cada dimensão
- [ ] **Tese aparece nas 2 primeiras frases do abstract** (teste do leitor cego)
- [ ] **DAG+U vs rivais é subseção autônoma** com mais de 1 página
- [ ] **Linhas 165 vs 203** do v7: na v8 a posição deve ser inequívoca (rivais reformulam, não eliminam, OVB)
- [ ] **Pelo menos 5 referências BR** citadas com função argumentativa, não decorativa
- [ ] **Worked example completo** (priori → verossimilhança em decibéis → posterior → sensibilidade), com discussão da reformulação da objeção "U" para "rival adicional"
- [ ] **Subseção trade-offs explícita** com pelo menos: elicitação, replicabilidade, treinamento, teatro Bayesiano, quando NÃO usar
- [ ] **Validade interna** usada em UM sentido consistente; "ortogonal" substituído
- [ ] **`validate-bib` PASS** (todas as citações têm entrada; todas as entradas são citadas; nomes verificados)
- [ ] **`proofread` PASS** (sem typos, concordância, formatação)
- [ ] **Compila PDF + Word sem erro**
- [ ] **Carta de respostas a pareceristas BPSR**: documento separado mapeando crítica → mudança textual (mesmo que pareceres tenham sido fracos, o editor espera essa carta na ressubmissão)

## Score esperado pós-revisão

| Dimensão | v7 | v8 (target) |
|---|---|---|
| Contribution | 4.5 | 6.5 — translation-gap real + ponto operacional próprio (rivais vs DAG-U) |
| Execution | 5.5 | 6.5 — tautologia resolvida; conceitos consistentes; F&C com caridade |
| Exposition | 4.0 | 7.0 — proofread + abstract reescrito + estrutura argumentativa |
| **Global** | **4.7** | **~6.7 — passa o gate publicável; calibrado para BPSR (não top intl)** |

## Próximos passos imediatos (sessão dedicada)

1. Abrir sessão em `quali-credibility/` (não na pasta-pai)
2. Ler este plano + `quality_reports/2026-05-08_edmans-review.md`
3. Rodar `paper-version` skill para criar v8 (git tag)
4. Rodar `lit-review` sobre literatura metodológica BR (P4)
5. Esboçar estrutura argumentativa nova em outline antes de escrever prosa (P2)
6. Identificar caso para worked example (P5) — decidir antes de escrever a seção operacional
7. Reescrever abstract (P1) — gate antes de prosseguir
8. Reescrever introdução (camada-dupla; ancoragem no gap BR)
9. Reescrever seção "Recepção qualitativa" para BR (com refs novas)
10. Escrever subseção operacional "DAG+U vs rivais" (P3) — centro do paper
11. Comprimir Bayes 101 e INUS/SUIN; reposicionar PT/queries como implementações
12. Reescrever crítica F&C como corolário sobre finitude de rivais
13. Adicionar trade-offs (P6)
14. Adicionar worked example (P5)
15. Reescrever transportabilidade e conclusão como reset do debate
16. Conserto conceitual (P7) — pode ser intercalado
17. Mecânica final (P8): proofread + validate-bib
18. Compilar; rodar `edmans-review` na v8 como gate de saída
19. Escrever carta de respostas
20. Submeter

## Observações sobre escopo

- **Sem mexer no `_targets.R`**: pipeline não é central ao argumento; manter como ilustração no apêndice se útil
- **`synth-trade-china.bib`**: untracked no v7; provavelmente colateral de outro paper — confirmar com autor antes de mexer
- **Versão Apple Pages (`paper_quali_v7.pages`, `paper_revision.pages/.docx`)**: NÃO TOCAR. São backup/correspondência editorial em formato fechado
