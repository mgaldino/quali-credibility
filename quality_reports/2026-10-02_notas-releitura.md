# Notas de releitura — v8 (PDF compilado em 2026-05-09)

**Data**: 2026-10-02
**Status**: notas abertas. Problemas levantados pelo autor na releitura, para discutir antes de decidir como endereçar. Nenhuma edição no manuscrito.

Números de linha referem-se a `paper_dados_format_quali.Rmd` no estado de 2026-10-02 (working tree, com mudanças não commitadas).

---

## Problema 1 — Justificativas da inferência (design / model / sampling) e a dicotomia ontológica determinístico × probabilístico

### Argumento do autor (ditado na releitura)

- O artigo cita *design-based* e *model-based*; falta o *sampling-based*, e a distinção entre os três está mal feita. A explicação de como se justifica a inferência causal está em parte errada, em parte incompleta.
- As três justificativas, classificadas pela fonte da aleatoriedade:
  - **Design-based**: a aleatoriedade está no mecanismo de alocação entre tratamento e controle (ex.: experimento). Os resultados potenciais podem ser fixos e a relação causal, determinística.
  - **Model-based**: a aleatoriedade está nos próprios resultados potenciais, que têm componente estocástico. Vale mesmo com mecanismo de alocação determinístico e mesmo quando se observa a população inteira.
  - **Sampling-based**: os dados são uma amostra aleatória de uma população maior (finita, ou superpopulação / processo gerador infinito).
- Função no artigo: questionar a distinção ontológica que a literatura qualitativa usa para se diferenciar da quantitativa — quanti seria "probabilístico", quali "determinístico" (relações necessárias e suficientes).
- O que os qualitativos têm em mente: regressão com termo de erro estimando efeito médio → "na média é assim; para casos individuais pode ser diferente" → "probabilístico". Essa imagem vem do paradigma pré-revolução da credibilidade (pré-resultados potenciais e DAGs), em que não se distinguia:
  - identificação causal de inferência estatística;
  - modelo estrutural de modelo estatístico;
  - β como parâmetro estrutural (causal) de β como parâmetro descritivo (correlação parcial).
  - Os pressupostos vinham misturados: E[ε | X] = 0 (ausência de viés, no fundo um pressuposto causal) listado ao lado de homocedasticidade (pressuposto para o erro-padrão).
  - Referência: revisão de Pearl de livros-texto de econometria, que documenta exatamente essa confusão.
- Conexão das três: no design-based a relação causal pode ser determinística e a inferência se justifica pela alocação aleatória; no model-based o resultado potencial é estocástico. Nada intrínseco à pesquisa quantitativa obriga a usar uma dessas justificativas em particular. Logo, não há diferença ontológica *necessária* entre quanti e quali.
- **Referência-base para as três definições**: de Chaisemartin & D'Haultfœuille, *Credible Answers to Hard Questions: Differences-in-Differences for Natural Experiments*, §2.4 "Framework for statistical inference", pp. 20–22 (rascunho de 25/09/2024, sob contrato com a Princeton UP). PDF local: `~/Documents/DCP/Cursos/Causalidade/cópia de BOOK CREDIBLE ANSWERS.pdf`. Não está no .bib; a forma de citação depende do status de publicação (ver lit-check).

### Onde o manuscrito está hoje (diagnóstico)

1. **Design/model-based definidos por aplicação.** §"Variantes da identificação: desenho vs. modelo" (l. 101–107) classifica *desenhos*: RDD/IV seriam design-based; DiD/controle sintético, model-based. O erro está nessa classificação, já que design/model-based classificam a *justificativa da inferência* (onde está a aleatoriedade), e o mesmo desenho admite as duas (ver ponto f abaixo). l. 111 usa o sentido correto (fonte de aleatoriedade), de modo que o texto parece ter dois sentidos sem marcação. Usos de "design-based" a revisar: l. 139, 145, 173, 285, 289, 361, 369, 411, 415.
2. **Sampling-based aparece em uma única frase** (l. 111), sem referência e sem função no argumento. `abadie_etal_2020` ("Sampling-Based versus Design-Based Uncertainty in Regression Analysis", *Econometrica*) está no .bib e não é citado.
3. **O manuscrito declara a questão ontológica fora de escopo.** l. 75: "sem tomar partido sobre a fonte ontológica da aleatoriedade subjacente, questão filosófica que não discrimina entre os argumentos desta nota"; l. 113: "questão deixada em aberto em §3.1". O argumento do Problema 1 usa as três justificativas justamente para desmontar a dicotomia, o que exige entrar na questão.
4. **l. 61** (crítica a Sposito et al. 2022) declara a dicotomia determinístico × probabilístico "dispensável sob resultados potenciais", sem argumentar. É o ponto onde o argumento do Problema 1 pagaria a afirmação.
5. **l. 233** (Inferências Integradas): "a relação é determinística, mas o nosso conhecimento sobre a relação causal é probabilístico". Compatível com o argumento: é a probabilidade Bayesiana/epistêmica, um sentido de "probabilístico" que não envolve aleatoriedade no mundo.
6. **Alvos não citados**: `mahoney_goertz_2006` ("A Tale of Two Cultures"), `mahoney_2008` ("Toward a Unified Theory of Causality", *CPS*) e `Mahoney_2010` estão no .bib e nenhum é citado no corpo. Mahoney & Goertz (2006) é a formulação canônica da dicotomia; Mahoney (2008) trata diretamente de causalidade probabilística (nível populacional) × necessária/suficiente (nível do caso).
7. **Chen & Pearl** (revisão de livros-texto de econometria) não está no .bib.

### Pontos discutidos com o autor (2026-10-02)

a. **Terminologia — resolvido.** Os termos são *design-based*, *model-based* e *sampling-based*, como em dC&DH §2.4 ("three common perspectives on statistical inference"). Abadie, Athey, Imbens & Wooldridge (2020) usam *sampling-based* e *design-based uncertainty*.

b. **Fronteira model-based × sampling-based — distinção conceitual clara; cuidado de redação no caso do censo.**
- Autor: os dois diferem pela fonte da aleatoriedade. No sampling-based, a incerteza vem de a amostra ser um subconjunto aleatório da população: a estimativa pode coincidir ou não com o efeito populacional. No model-based, a incerteza persiste com a população inteira (censo, dados administrativos de todas as escolas ou de todas as votações), porque o resultado potencial é estocástico: chuva ou enchente no dia mudam o resultado. Ter amostra ou população é irrelevante para o model-based.
- Critério formal de dC&DH (p. 20): no model-based, o desenho D é fixo (ou condicionado) e só os resultados potenciais são aleatórios. No sampling-based, D e os resultados potenciais são ambos aleatórios, porque amostras diferentes geram desenhos e resultados potenciais diferentes.
- Proximidade que o próprio livro registra (pp. 20–21): quando a amostra inclui todas as unidades ("their study sample often includes all the states or municipalities of a country"), o sampling-based só se sustenta por um experimento mental, com uma superpopulação infinita hipotética. Nesse caso, "the two remaining perspectives on statistical inference do not greatly differ, but we favor the model-based one, for pedagogical reasons". Implicação para o paper: definir as três pela fonte da aleatoriedade, como o livro faz, sem afirmar que levam sempre a procedimentos distintos. A distinção conceitual do autor fica intacta.

c. **Estocasticidade dos resultados potenciais — leitura ontológica (posição do autor).**
- Autor, seguindo dC&DH: no model-based, a estocasticidade é inerente ao resultado potencial; algum componente do mundo é aleatório.
- Exemplo para o paper: efeito das operações policiais no segundo turno de 2022 sobre o comparecimento. O efeito difere entre lugares onde choveu e onde não choveu (onde choveu, talvez não haja efeito, porque as pessoas já deixariam de votar por causa da chuva). Chover ou não é aleatório para todos os efeitos. A chuva entra no modelo como fenômeno aleatório. *Conferir ao redigir: as operações de 30/10/2022 foram da PRF (Polícia Rodoviária Federal).*
- **Leitura descartada**: estocasticidade como resumo instrumental de causas não modeladas (sugestão anterior do agente). Descartada pelo autor: no model-based, o componente estocástico é do mundo.
- Nota de redação: dC&DH descrevem o model-based como "a thought experiment, where one imagines that nature draws some shocks affecting potential outcomes", que exige "a judgment call on the joint distribution of the shocks" (p. 20). Formular no paper como suposição explícita do pesquisador ("supõe-se que choques estocásticos afetam os resultados potenciais"), o que se alinha ao ponto d.
- VanderWeele & Robins (2012, contrafactuais estocásticos e causas suficientes estocásticas) seguem como ponte possível com o exemplo INUS do incêndio. Verificação a cargo do lit-check.

d. **Termo de erro e explicitação da ontologia — formulação do autor (núcleo do argumento).**
- Na regressão pré-CR, ε pode ser justificado como causas determinísticas omitidas ou como componente aleatório do mundo. Como o resultado potencial não é modelado explicitamente, a escolha fica mal definida.
- Ao modelar explicitamente o resultado potencial, o pesquisador precisa responder onde está a aleatoriedade. Há termo estocástico na equação do resultado potencial? Se não há, a análise não é model-based. Está na alocação do tratamento? Na amostragem? É preciso dizer.
- Tese: a exigência de explicitar o contrafactual força o pesquisador a explicitar a ontologia. Antes, ela ficava implícita, o que abriu espaço para a confusão compartilhada por qualitativos e quantitativos.
- A decomposição de ε proposta antes pelo agente (heterogeneidade de efeitos, causas omitidas, erro de medida, estocasticidade) fica subsumida: é o inventário do que ε podia significar enquanto nada obrigava a escolher.

e. **Bayes como quarto sentido — não discutido.** A probabilidade subjetivista (l. 113) é epistêmica e se aplica igualmente a relações determinísticas (l. 233). Organização possível: as três justificativas frequentistas, classificadas pela fonte de aleatoriedade (alocação, resultados potenciais, amostragem), mais a probabilidade como grau de crença. Nenhuma das quatro é exclusiva do quanti.

f. **Um único sentido de design/model-based — resolvido pelo autor.**
- Design/model/sampling-based classificam a justificativa da inferência. O erro do manuscrito (l. 101–107) é definir design-based por aplicações.
- Um mesmo desenho admite justificativas diferentes; a ambiguidade é da prática aplicada (qual justificativa o pesquisador adota), e os conceitos têm definição única. Exemplo com DiD: quem modela os resultados potenciais como tendências com componente aleatório está no model-based; quem invoca um choque aleatório que tratou algumas unidades e outras não está no design-based.
- Apoio em dC&DH: adotam model-based para DiD porque as implicações testáveis do timing aleatório são violadas na maioria dos experimentos naturais que revisitam (p. 21); citam Athey & Imbens (2022) para DiD design-based.
- Consequência a pensar: o paralelo CR/IBE de l. 107, 139 e 411 (não-manipulação, exclusão, tendências paralelas ↔ exaustividade da enumeração; "modelo correto" ↔ "enumeração exaustiva") trata de *suposições de identificação*. Ele precisa ser reancorado nessas suposições, sem o rótulo design/model-based.
- Checagem de literatura em andamento (o autor pediu): `quality_reports/2026-10-02_lit-check-design-model-sampling.md`. Inclui a variante da estatística de surveys, em que "design-based" designa a aleatoriedade do desenho *amostral*.

g. **Localização — não discutido.** O §"Framework Bayesiano subjetivista" já abre com as três fontes (l. 111) e é candidato natural para o argumento expandido. Alternativa: subseção própria antes dele, que l. 61 e l. 75 passariam a referenciar.

h. **Pendências bibliográficas.** Adicionar dC&DH ao .bib (forma de citação conforme o status de publicação); Chen & Pearl (2013); citar Mahoney & Goertz (2006) e Mahoney (2008) como formulações da dicotomia.

### Resultado da checagem de literatura (2026-10-02)

Relatório integral: `quality_reports/2026-10-02_lit-check-design-model-sampling.md` (37 fontes, com status de verificação e BibTeX das verificadas).

**Conferido pelo agente principal nos PDFs locais**: dC&DH versão 27/02/2026 (§2.4; definições idênticas às do rascunho de 2024, inclusive "do not greatly differ"); KKV pp. 59–60; Mahoney & Goertz pp. 229, 233–234, 239 n. 12; Keele (2015) §4 e nota de rodapé. **Não conferido pelo agente principal** (o subagente marcou como VERIFICADO via arXiv/DOI): Athey & Imbens 2017, Abadie et al. 2023, Rambachan & Roth 2026, Aronow, Jang & Offer-Westort 2026, data de publicação pela PUP.

1. **As definições estão corretas e são o uso dominante.** A tripartição aparece em Abadie et al. (2023, *QJE*), Rambachan & Roth (2026, *JASA*), Roth et al. (2023) e Arkhangelsky & Imbens (2024). A classificação pela justificativa, com o mesmo desenho admitindo análises diferentes, está documentada para DiD, RD, IV, shift-share e controle sintético. A correção do autor sobre l. 101–107 se sustenta.
2. **"Um único sentido" não descreve o uso.** Coexistem três: (i) inferencial (dC&DH, Abadie et al., Athey & Imbens); (ii) amostragem de surveys, em que *design-based* = aleatoriedade do plano amostral (Särndal et al. 1992; Little 2004); (iii) identificação ou primado do desenho (Dunning 2012, Sekhon 2009, Keele 2015, Borusyak, Hull & Jaravel 2025). Keele (2015, §4): "the phrase 'design-based approach' does not have universal definition"; em nota, ele evita "design-based inference" para não confundir com o uso de surveys. O §Variantes (l. 101–107) segue o sentido (iii), corrente em ciência política, e a l. 111 usa o sentido (i). **Decisão para o autor**: adotar o sentido inferencial explicitamente, com nota de rodapé que reconheça os outros dois (precedentes: Keele 2015; Aronow, Jang & Offer-Westort 2026, n. 1).
3. **Rótulos de model-based × sampling-based instáveis.** Segundo o relatório, Athey & Imbens (2017) chamam de *sampling-based* o arranjo "atribuição fixa, resultados aleatórios", que dC&DH chamam de *model-based*. Imbens (2024) agrupa "model- or sampling-based" contra *design-based*. Para o argumento do paper, o corte decisivo é entre aleatoriedade na atribuição e aleatoriedade nos resultados ou nas unidades.
4. **DiD: a justificativa muda a hipótese identificadora e o estimando.** O estimador é o mesmo. Em Athey & Imbens (2022), o ponto de partida é a data de adoção aleatória; tendências paralelas são consequência, e o estimando é uma média ponderada específica de efeitos. Em dC&DH, tendências paralelas recaem sobre E[Y(0)] condicional ao desenho. Implicação para a Camada 1: no design-based, a mesma suposição (atribuição aleatória) identifica e fundamenta a inferência. O split identificação/inferência se mantém conceitualmente, mas o texto precisa reconhecer esse caso.
5. **Risco de espantalho na atribuição da dicotomia.** Mahoney & Goertz (p. 234) rejeitam explicitamente que a ausência de termo de erro na equação qualitativa implique suposições determinísticas e citam procedimentos para causas necessárias/suficientes probabilísticas. "Untenable deterministic assumptions" (p. 233) aparece como objeção que os pesquisadores estatísticos fazem ao quali. A Tabela 1 (p. 229) opõe "necessary and sufficient causes; mathematical logic" a "correlational causes; probability/statistical theory". Alvos defensáveis: a Tabela 1 como formulação da dicotomia e Sposito et al. (2022), já criticados na l. 61 pela "lógica determinista".
6. **Evidência para o ponto d (termo de erro).** Mahoney & Goertz (p. 239, n. 12): "the error term of a typical statistical model may contain a number of variables that qualitative researchers regard as crucial causes in individual cases". É a leitura de ε como causas determinísticas omitidas, típica do enquadramento pré-CR.
7. **KKV (pp. 59–60) como aliado.** A "Perspective 1: A Probabilistic World" usa o argumento do censo ("Even if we [...] collected a census [...] our analyses would still never generate perfect predictions"), o mesmo do exemplo model-based do autor. A Perspective 2 é determinística. As duas são "observationally equivalent", e a escolha "depends on faith or belief rather than on empirical verification"; o argumento "applies with equal force to qualitative and quantitative researchers". Na n. 12, economistas ficam mais perto da Perspectiva 1 e estatísticos da 2. A equivalência observacional apoia a redação do ponto c (componente estocástico como suposição explícita).
8. **N pequeno no design-based** (observação do subagente, sem fonte): a distribuição de aleatorização fica pobre. Com uma unidade tratada e uma de controle sob probabilidades iguais, o menor p-valor exato é 0,5; com N = 1 não há comparação. Conecta com a Camada 3.
9. **Citação de dC&DH.** Sai pela Princeton UP com novo título, *Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions* (previsto para 8/12/2026, copyright 2027; preprint SSRN 10.2139/ssrn.4487202). A versão de 27/02/2026 já está no repo (`DiD_deChaisemartin_dHaultfoeuille.pdf`, ignorada pelo .gitignore); nela, §2.4 ocupa pp. 28–32. Citar por seção. Chen & Pearl (2013): *real-world economics review* 65, pp. 2–20, sem DOI. O anexo do Zotero rotulado Imbens & Rubin (2015) é um syllabus de outro curso.

### Respostas do autor à checagem (2026-10-02)

- **Três sentidos de "design-based" (item 2)**: de acordo. Adotar o sentido inferencial e reconhecer os outros dois em nota de rodapé.
- **Alvo da dicotomia (item 5)**: a leitura "quanti probabilístico / quali determinístico" é da literatura metodológica **brasileira**. A fronteira qualitativa internacional (Mahoney; Seawright — segundo o autor, hoje com outro prenome; confirmar a forma atual do nome para citação) já sabe disso. O ponto é novidade para a pedagogia brasileira e se encaixa na Camada 2. Mahoney & Goertz (p. 234) passam a servir de evidência de que a fronteira já superou a dicotomia; o alvo da crítica é a recepção brasileira (Sposito et al. 2022 e afins).
- **KKV**: aliado no ponto ontológico (pp. 59–60, equivalência observacional das duas perspectivas) e alvo no ponto da fusão entre identificação e inferência. Os dois usos convivem.

### Decisão: suposição que identifica e também fundamenta a inferência (item 4)
- **Escolha (autor)**: identificação e inferência são perguntas distintas, mesmo quando a mesma suposição entra nas duas. Identificação responde ao que acontece com amostra infinita (a população): o estimando é pontualmente identificado? Por construção, ali não há problema de inferência. Que a suposição identificadora tenha implicações para a amostra finita (ex.: dC&DH derivam da perspectiva model-based a recomendação de clusterizar no nível mais desagregado que ainda forma painel, com custos em relação a outras escolhas) não mistura as duas perguntas. Não misturá-las é um dos pontos do paper, em linha com a literatura internacional; a fusão de KKV é a perspectiva menos produtiva e com menos clareza analítica.
- **Alternativas descartadas**:
  - "No design-based, a atribuição aleatória faz o duplo trabalho e o split precisa ser qualificado" (sugestão do agente após o lit-check): descartada. Uma suposição servir de premissa a duas perguntas não funde as perguntas.
- **Detalhe para a redação** (compatível com a escolha): no mesmo trecho, dC&DH (versão 2026, §2.4) observam que clusterizar no nível do estado torna a suposição de tendências paralelas ligeiramente mais fraca, pois ela passa a valer incondicionalmente, e não condicional aos choques realizados. A escolha do que é aleatório e do que é condicionado vem antes das duas perguntas: fixa o estimando e a forma da suposição. Dada essa escolha, identificação e inferência seguem separadas.

---

## Problema 2 — (a preencher)
