# Parecer de Exposition (Framework Edmans) — paper_dados_format_quali.Rmd (v8 em desenvolvimento)

**Data**: 2026-05-09
**Avaliador**: Editor (simulado) de top journal de CP
**Manuscrito**: As implicacoes metodologicas da Revolucao da Credibilidade e Inferencia Bayesiana para a pesquisa qualitativa causal (v8 em desenvolvimento)
**Autor**: Manoel Galdino (USP)
**Genero**: Nota de pesquisa (~10.700 palavras totais Rmd; corpo principal ~7.500-8.000 palavras), submetida a BPSR
**Comparacao**: parecer de Exposition v7 (2026-05-08, score 4/10)

---

# Parecer de Exposition (Framework Edmans)

## Score: 6.5/10

Justificativa: A v8 representa uma melhora substancial e mensuravel sobre a v7. O nome do autor foi corrigido, a equacao Bayesiana central foi reparada, os typos lexicais grosseiros (`Galino`, `Potanto`, `acabouco`, `conslidou-se`, `causal/causa`, `comunicais`) foram eliminados, e a tese tripla esta articulada com clareza incomum num primeiro paragrafo. A introducao agora cumpre a funcao Edmans de **vender em duas paginas**. O abstract, embora ainda enxuto-empolado, contem uma "marca" memoravel — a migracao da objecao de `e se houver um U?` para `sua enumeracao foi exaustiva?`.

A nota perde ponto pelos seguintes motivos: (i) **citacoes nao-canonicas persistem no paragrafo de abertura da Secao 3** (Forozish_2024 — SSRN paper de autor obscuro — alinhado com Goldsmith-Pinkham e Angrist-Pischke como se fossem do mesmo peso); (ii) **camadas de redundancia entre intro / Secao 4 / conclusao** — o split conceitual e a migracao da objecao de credibilidade aparecem em pelo menos quatro momentos do texto, perdendo forca cumulativa; (iii) **a Secao 5 (`Solucoes Praticas`)** mistura material de livro-texto Bayesiano com a tese sobre sinal-ruido (linhas 137-149) — o leitor pos-graduado vai pular, e o leitor que precisa do material nao tem condicao de avaliar a aplicacao posterior; (iv) **a Secao 6 (`Novos Desenhos Causais Qualitativos`)** ainda carrega tracos de manual ("a definicao de prioris pode ser feita por meio da elicitacao de experts...") que diluem a contribuicao; (v) **trechos residuais da v7 nao reescritos** sobreviveram no fim da Secao 9 e na conclusao (linhas 351-367) — convivem com a prosa nova v8 e produzem inconsistencia de registro; (vi) **tres formas concorrentes** de "process tracing / rastreio de processo / rastreamento de processos" oscilam ao longo do texto.

O score 6.5 reflete: a v8 ja **passa o gate "passo zero" do Edmans** (nao tem typos que sinalizam descuido total; tese e legivel em duas paginas), mas ainda nao atinge o standard do top journal porque a redundancia interna e os residuos da v7 fazem com que o paper leia com **35-40% mais palavras do que precisa**. Em revisao linha-a-linha de 1-2 dias o score sobe para 7.5/8.0.

---

## Avaliacao por dimensao

### Clareza — **Boa** (era Fraca em v7)

#### Qualidade da escrita

**Melhorias mensuraveis vs v7:**

- **Linha 4**: `author: "Manoel Galdino"` — corrigido (era "Manoel Galino"). Sinal de cuidado restaurado.
- **Linha 163** (era linha 121 v7): equacao `\frac{P(H_i|E)}{P(H_j|E)} = \frac{P(H_i)P(E|H_i)}{P(H_j)P(E|H_j)}` — parenteses balanceados. Reparada.
- Typos de palavras-chave da v7 eliminados: nao ha "Potanto", "acabouco", "conslidou-se", "comunicais", "qual a causal" sobreviventes.
- `Por outro outro lado` (duplicacao da v7) eliminada.
- **Linha 197**: o agradecimento a Elizabeth Balbachevsky permanece em rodape `^[...]` Rmd-style, e nao mais em `\footnote{}` quebrada. Apenas 1 nota de rodape Rmd-style + 1 LaTeX-style — total 2, dentro do limite Edmans (1/pagina).

**Problemas residuais de escrita encontrados na v8:**

1. **Linha 39**: `O restante da nota está organizado como segue. A próxima seção mapeia a recepção brasileira...` — a frase `como segue` é construcao academica padrao mas pesada; `está organizado da seguinte forma` ou simplesmente `Esta nota tem a seguinte estrutura.` lê melhor.

2. **Linha 245**: `inclusive com alunos que viram o conteúdo em aula e já disseram que estão usando em seus trabalhos (ainda não publicados)` — registro **coloquial**, claim **anedotico**, e parentese `(ainda não publicados)` que reconhece nao ter como referenciar. Em paper academico a frase **inteira** sai. Mantenha apenas a citacao `[@fairfield_charman2025; @rabbia_2023]` como evidencia de aplicacao.

3. **Linha 215**: `como é costumeiro, é mais fácil modelar as relações causais como determinísticas` — `como e costumeiro` e tique de fala. Cortar.

4. **Linha 217**: `Esse modelo pode ser facilmente generalizado para múltiplas causas, com uma pequena modificação na notação, a qual o leitor interessado pode encontrar no livro de Humphreys e Jacobs.` — passagem inteira em registro de aula, nao de paper. Reduzir a `O modelo se generaliza para multiplas causas (cf. @Humphreys_Jacobs_2023, capitulo X).`

5. **Linha 327**: `Similarmente, as duas metodologias são igualmente limitadas (a princípio) em sua capacidade de possuírem validade externa. E essas duas conclusões seguem do fato de que não há relação entre validade interna e externa e tamanho amostral, se a pesquisa é em profundidade ou não, ou quaisquer das possíveis distinções empregadas para diferenciar métodos qualitativo de quantitativo.` — frase **muito longa e mal pontuada**, com `E` iniciando a segunda frase, `de quantitativo` (sg/pl errado — qualitativos/quantitativos), e `(a princípio)` parenteses-hedge. Reescrever em duas frases curtas.

6. **Linha 348**: `Como esse exemplo mostra, a generalização é difícil de ser realizada e as soluções propostas na literatura estão longe de serem ponto pacífico. Contudo, tampouco é algo resolvido na abordagem influenciada pela revolução da credibilidade. Se algo, por hora parece mais fácil endereçar essa questão na pesquisa qualitativa do que na quantitativa, como mostramos acima. Mas é a prática aplicada que irá dizer em que medida cada abordagem irá lidar com esse problema.` — passagem inteira **mal escrita** e em **contradicao com o resto do paper**: o paper sustentou que validade externa e problema **igualmente dificil em ambas**, mas aqui aparece como sendo `mais fácil endereçar essa questão na pesquisa qualitativa`. Inconsistencia logica + prosa coloquial (`por hora`, `Se algo`). Reescrever ou cortar.

7. **Linha 365**: `Por fim, abordamos como a literatura baseada na ideia de transportabilidade pode permitir que a pesquisa qualitativa supere suas limitações quanto à validade externa.` — **roteiro reverso vazio**. Anuncia que abordamos algo que ja foi abordado, sem dizer o que descobrimos. Cortar inteira ou reescrever como conclusao substantiva (ex: `A transportabilidade trata escopo como objeto de teorizacao explicita, nao como variavel de ajuste — a finitude do conjunto de rivais que sustenta a comparacao Bayesiana exige que a redefinicao de escopo venha acompanhada de mecanismo independente.`).

8. **Linha 367**: `Esperamos que este estudo inspire uma reavaliação das abordagens metodológicas nas ciências sociais, promovendo um debate mais equilibrado e integrador.` — **fechamento generico** classico ("hopes and dreams"). Edmans recomenda fechar com a tese, nao com `esperamos que inspire`. Cortar.

9. **Inconsistencia de terminologia "process tracing"**: `process tracing` (linhas 37, 41, 115, 153, 245, 261, 363), `rastreamento de processos` (159, 241), `rastreio de processo` (191, 201), `rastreio de mecanismo` (59) — quatro variantes. Padronizar em **uma forma** (sugiro `process tracing` em italico, com glosa em portugues `(rastreamento de processos)` na primeira ocorrencia).

10. **Linha 213**: `Um primeiro aspecto que a metodologia de @Humphreys_Jacobs_2023 deixa claro é que é necessário, para fins práticos, considerar todas as variáveis da pesquisa quali como binárias.` — `é que é necessário` tem `e que e` em sequencia. Reformular.

#### Significancia substantiva

**O abstract da v8** (linhas 20-24) é dramaticamente melhor que o da v7. A frase-marca está claramente **identificavel**:

> A objeção de credibilidade contra estudo qualitativo causal deixa de ser 'e se houver um confundidor não considerado?' e passa a ser 'sua enumeração de rivais foi exaustiva?'; o critério de credibilidade migra de plausibilidade de ignorabilidade via desenho para robustez da comparação entre rivais.

Isto e ideia-marca. **Mas** o abstract gasta as primeiras 4 linhas no movimento `Esta nota tem dois objetivos. Primeiro... Segundo...` antes de chegar na frase-marca. **Recomendacao Edmans**: invertir — abrir com a frase-marca, e depois desdobrar nos dois objetivos. A versao reordenada seria:

> Em desenhos qualitativos de n pequeno, a objeção de credibilidade contra inferência causal deixa de ser 'e se houver um confundidor não considerado?' e passa a ser 'sua enumeração de rivais foi exaustiva?': o critério migra de plausibilidade de ignorabilidade via desenho para robustez da comparação entre explicações rivais. Esta nota explora duas implicações dessa migração para a ciência política brasileira. Primeiro, sistematiza a separação entre identificação causal e inferência estatística...

Numero memoravel: o paper nao tem nenhum (e nao precisaria, sendo conceitual), mas o **caso ilustrativo** do impeachment de 2016 fornece uma frase candidata: `posterior odds H_2/H_3 ≈ +0,6 dB — distante do limiar de evidencia saliente`. Esse numero poderia entrar no abstract para dar concretude ao que o exemplo entrega.

**A introducao da v8** (linhas 35-49) é **substancialmente** melhor que a v7:
- Paragrafo 1 (linha 37): contexto claro com 4 citacoes-chave (KKV, Sekhon, Seawright, Brady-Collier, Collier, Bennett-Checkel, Rihoux-Ragin, Ragin) — talvez 1-2 a mais do que necessario, mas funcional.
- Paragrafo 2 (linha 39): tese declarativa direta. Boa.
- Paragrafo 3 (linha 41): primeira camada (split conceitual) com substanciacao via Angrist-Pischke + Lundberg + Imbens + Card.
- Paragrafo 4 (linha 43): segunda camada (manobra do U abstrato). Forte.
- Paragrafo 5 (linha 45): distincao com Spirling-Stewart. **Crucial e clara**.
- Paragrafo 6 (linha 47): metaclaim sobre `n grande vs n pequeno`. Boa amarracao.
- Paragrafo 7 (linha 49): roadmap.

A intro ocupa 7 paragrafos em ~700 palavras, ~2.5 paginas double-spaced. Dentro do limite Edmans de 6 paginas (folgadamente). **Aprovada**.

**A frase-tese aparece quantas vezes?** Conto:
- linha 22 (abstract): a frase-marca explicita.
- linha 39 (intro paragrafo 2): articulacao do split.
- linha 47 (intro paragrafo 6): re-articulacao como metaclaim.
- linha 65 (Secao 2 fim): re-articulacao como gap brasileiro.
- linha 109-119 (Secao 4): re-articulacao como mudanca de criterio.
- linha 317 (fim da Secao 8): re-articulacao no fechamento da ilustracao.
- linha 355-361 (conclusao): re-articulacao tres vezes em paragrafos seguidos.

**Nove ocorrencias da mesma tese**. Em paper conceitual, repeticao deliberada e tecnica didatica valida **quando** introduz nuance nova a cada repeticao. Aqui, a Secao 4 e a conclusao **repetem o mesmo conteudo em palavras quase identicas**. Recomendacao: cortar ou variar. A versao da linha 47 (no metaclaim sobre n grande vs n pequeno) e a melhor; sugiro mante-la e cortar as redundantes (notavelmente linha 361, que repete o metaclaim da linha 47 quase literalmente).

#### Precisao da linguagem

A v8 tem prosa **muito mais precisa** que a v7. Exemplos de melhoria:

- **Linha 79**: `A mais típica é conhecida como "ignorability" forte, cuja exigência central é que a atribuição do tratamento seja independente dos resultados potenciais` — definicao crisp, com hedge correto (`tipica`, `central`).
- **Linha 105**: `sob SUTVA e adesão completa, sustenta a ignorabilidade do tratamento recebido` — qualificacao metodologica precisa (atende a regra global do CLAUDE.md sobre nao-overclaim em causalidade).
- **Linha 121**: `ela não elimina, no sentido formal quantitativo, o problema do viés de variável omitida... O que a comparação de rivais faz é *reformular* o problema` — **distincao fundamental** comunicada com precisao. Esta e a contribuicao tecnica do paper, e o paragrafo a entrega bem.

**Imprecisoes residuais:**

1. **Linha 109**: `é sempre possível conjeturar mais um $U$ plausível em princípio. Nessa forma — postulado abstratamente, sem articulação como explicação rival concreta com implicações empíricas próprias — qualquer $U$ é universalmente aplicável a qualquer estudo e por isso não discrimina entre estudos bons e ruins.` — A passagem é precisa mas **tecnicamente delicada**. A regra global do CLAUDE.md alerta: `Variavel U nunca refutavel — falso, U articulada como rival concreta com implicacoes empiricas e refutavel caso a caso.` O paragrafo **distingue corretamente** o U abstrato do U articulado, mas a frase resumo `qualquer U e universalmente aplicavel` e forte demais sem o qualificador imediato. Sugiro: `qualquer U **postulado nessa forma abstrata** e universalmente aplicavel...`. A precisao ja existe duas linhas acima — mas a frase-resumo perde a qualificacao.

2. **Linha 213-217**: a passagem sobre "tipos causais" (adverso, benefico, cronico, destinado) **mantem o problema da v7** — esta e traducao improvisada de `adverse, beneficial, chronic, destined` de Humphreys-Jacobs. A traducao "destinado" para `destined` (que significa "improves regardless of treatment") e contraintuitiva em portugues. Sugiro nota de rodape ou parentese `(adverse, beneficial, chronic, destined no original)` na primeira ocorrencia.

3. **Linha 215**: `o nosso conhecimento sobre a relação causal é probabilístico, o que é compatível com boa parte das ontologias sociais` — claim ontologico forte sem citacao. Ou cita (Mahoney 2008? Brady?), ou hedge mais forte (`o que e compatível com a maior parte das ontologias adotadas em ciencia politica empirica`).

4. **Linha 327**: `(a princípio)` — parenteses-hedge sem qualificacao do que muda fora do "principio". Cortar ou substanciar.

5. **Linha 144**: `estudos qualitativos bem desenhados geralmente se concentram em contextos onde o sinal é forte e claramente observável` — claim empirico forte sem citacao (mesmo problema da v7, linha 89). O autor proprio reconhece em outra parte que ha pesquisa qualitativa mal desenhada; aqui assume que `bem desenhado` coincide com `alto sinal-ruido`, o que e reificacao.

6. **Linha 143**: `Esse tipo de estratégia visa resolver problemas de inferência, mas nada dizem sobre a validade interna, que estão relacionadas às condições formais de identificação.` — concordancia: `nada **diz**` (nao plural); `relacionada` (nao plural). Erro residual da v7.

---

### Extensao — **Adequado, com inflacao localizada**

#### Introducao (linhas 35-49)

~700 palavras, 7 paragrafos, ~2.5 paginas double-spaced. **Dentro do limite Edmans**. Estrutura:
- Contexto (1) — adequado.
- Tese (1) — direta.
- Camada 1 (1) — substancia bem.
- Camada 2 (1) — forte.
- Distincao SS (1) — crucial.
- Metaclaim sobre n (1) — boa amarracao.
- Roadmap (1) — funcional.

**Diferente da v7**, a intro nao mais intercala mini-survey. A camada 1 traz suas citacoes na propria proposicao, sem digressao. **Aprovada**.

Uma observacao Edmans: a intro NAO tem **estrutura de problema** classica (`Existing literature claims X. We show Y.`). Em vez disso, tem **estrutura de afirmacao em camadas**. Para nota de pesquisa em portugues e funcional, mas para um JoP/AJPS o gancho da contribuicao precisaria ser mais sharp na primeira pagina. Para BPSR esta calibrada.

#### Notas de rodape

**2 notas no manuscrito** (linha 197 — agradecimento; linha 265 — disclaimer da ilustracao do impeachment). Ambas apropriadas. Total ~10-15 paginas → 2/15 = bem dentro do limite Edmans (1/pagina). **Aprovado**.

A nota da linha 265 é **substantiva e necessaria** — explica que os numeros da ilustracao sao didaticos. Caso classico de nota Edmans-aprovada.

#### Secoes / extensoes — analise por secao

**Secao 1 (Introducao, linhas 35-49)** — adequada, 7 paragrafos. Aprovada.

**Secao 2 (Recepcao brasileira, linhas 51-65)** — 6 paragrafos densos. **A secao funciona porque substancia o gap brasileiro com 12+ refs novas**, mas paragrafo da linha 61-63 (decomposicao da selecao de casos e critica EQ/PE/TC) **antecipa material que aparece de novo na Secao 4 (linha 129-131) com texto quase identico**. O leitor recebe a mesma critica duas vezes. **Recomendacao**: deixar a critica Sposito et al na Secao 4 e cortar do paragrafo de linha 61 (mantendo apenas o registro factual `Sposito et al organizam selecao de casos em torno de tipos de alegacao causal — probabilistica, mecanistica, conjuntista — sem o vocabulario formal Pearl/Rubin nem comparacao Bayesiana`).

**Secao 3 (Revolucao da Credibilidade, linhas 67-101)** — recapitulacao basica. **Subsecao "Identificacao Causal" (linha 73)** preserva o material INUS/SUIN da v7 (linhas 81-101) — porem o autor alegou no plano da v8 que `INUS/SUIN cortado`. Verificacao: o material foi **reduzido em ~50%** mas **nao cortado**. A apresentacao DAG do incendio (linhas 83-99 com TikZ) e nova e bem feita; a moral da Linha 101 (`a analise profunda *per se* nao protege contra esse erro; o que protege e considerar U como hipotese rival explicita`) é forte e amarra com a tese central. **Veredicto**: a secao **se justifica agora**, mas o plano da v8 falou em corte, e o que se tem é reducao + reorganizacao. Anotar como inconsistencia plano-execucao.

**Secao 4 (De variavel omitida a explicacao rival, linhas 103-131)** — **centro do argumento, e a melhor secao do paper**. A subsecao 4.3 (`Distincao a Spirling & Stewart`, linhas 123-127) e clara e cumpre a funcao critica de diferenciar a v8 do trabalho mais proximo. Aprovada.

**Subsecao 4.4 (`A selecao de casos, decomposta`, linhas 129-131)** — discutida acima, repete material da Secao 2.

**Secao 5 (Solucoes Praticas para Inferencia em Amostras Pequenas, linhas 133-149)** — **secao mais problematica da v8**. 17 linhas, divididas em prosa que mistura:
- Bayes 101 (linhas 137-139): `a quantificacao da incerteza depende de uma distribuicao de probabilidade a priori...` — texto de manual.
- Tese sinal-ruido (linhas 141-145): `a questao critica nao e o tamanho da amostra, mas a relacao sinal-ruido` — substantivo, mas claim sem citacao + reificacao.
- Sumario do teorema de Bayes (linha 149): `$p(H | E) \\propto p(H) \\, p(E | H)$` com glosa — adequado.

O resultado: a secao **nao decide** se introduz Bayes ou se faz argumento sobre sinal-ruido. **Recomendacao**: dividir. Levar Bayes 101 (linhas 137-139) para uma transicao de **2 frases** entrando na Secao 6, e fazer a tese sinal-ruido **um paragrafo da Secao 4** ou dela mesma como Secao 5 reduzida (`Inferencia Bayesiana em pequeno-n: sinal-ruido em vez de tamanho`). A formulacao atual e residuo da v7 nao reescrito a forca.

**Secao 6 (Novos Desenhos Causais Qualitativos, linhas 151-245)** — **a secao mais longa do paper, ~95 linhas**. Subdividida em:

- **6.1 Process Tracing Bayesiano** (157-207): bem estruturada, mas:
  - 6.1.1 `Definicao de Prioris` (linhas 167-177): material de manual (`tres possibilidades: elicitacao, nao-informativa, informativa`). Cortar 60%, deixar uma frase em 6.1.
  - 6.1.2 `Verossimilhancas` (linhas 179-187): material substantivo (decibeis), preservar.
  - 6.1.2.1 `Evidencias` (linhas 189-193): subsubsecao com 5 frases — mau uso de hierarquia. Funde no paragrafo anterior.
  - 6.1.3 `Hipoteses Rivais` (linhas 195-207): central para o argumento, manter.

- **6.2 Inferencias Integradas** (linhas 209-237): apresenta queries causais. Subsubsecoes 6.2.1, 6.2.2, 6.2.3 (Efeito ao Nivel do Caso, Atribuicao, Caminhos Causais). **Mas a lista da linha 219-223 lista 4 tipos** (case-level, atribuicao, ATE, caminhos) e **so 3 sao desenvolvidos** — ATE e omitido. **Inconsistencia estrutural herdada da v7**.

- **6.3 Comparando as Abordagens** (linhas 239-245): boa secao, sintetiza. Aprovada exceto pelo paragrafo final coloquial discutido (linha 245).

**Veredicto Secao 6**: a secao tem ~3000 palavras e podia ter 1800. **Cortar 30-40%** removendo material de manual e a subsubsecao Evidencias. ATE precisa ou ser desenvolvido ou removido da lista.

**Secao 7 (Limitacoes, linhas 247-261)** — **secao bem feita**. Lista 6 limitacoes em formato de paragrafos curtos. Edmans-aprovado: explicitar limitacoes e `pre-empting concerns`. Mantenha como esta.

**Secao 8 (Ilustracao do impeachment, linhas 263-317)** — **secao nova da v8 e excelente do ponto de vista expositivo**. Subdivisao clara (Hipoteses, Evidencias, Verossimilhancas, Posteriors, Sensibilidade, Discussao). A tabela de verossimilhancas (linha 291-293) e os calculos em decibeis (linhas 297-299) **concretizam o argumento**. A subsecao `Sensibilidade e enumeracao` (linha 311-313) e particularmente forte — introduz H_4 (erro estrategico do PT) como teste da exaustividade do conjunto de rivais. A discussao final (linha 317) amarra ao argumento central. **Aprovada com louvor**.

**Secao 9 (Transportabilidade, linhas 319-348)** — **secao com mais residuo da v7**. As primeiras tres subsecoes (linhas 321-325) sao novas e fortes. Mas a partir da linha 327 a prosa volta ao registro v7:
- Linha 327: frase muito longa e mal pontuada (discutida acima).
- Linhas 329-336: paragrafos curtos e desconexos, com citacao a Campbell e Mcdermott (definicao classica de validade interna) que ja foi feita em outras partes do paper. O fluxo se quebra.
- Linha 348: paragrafo coloquial e contraditorio (discutido acima).

**Recomendacao**: a Secao 9 precisa de revisao linha-a-linha. Cortar as linhas 327-336 inteiras se possivel; o argumento substantivo (transportabilidade exige teorizacao explicita do escopo, nao redefinicao ad hoc) ja esta nos paragrafos linhas 338-346.

**Secao 10 (Consideracoes Finais, linhas 351-367)** — **mistura prosa nova v8 com residuo v7**:
- Linhas 353-361 (prosa nova): re-articula a tese tripla. Boa.
- Linha 363 (residuo v7): `mostramos que a inferencia Bayesiana oferece solucoes robustas` — texto de v7.
- Linha 365 (residuo v7): roteiro reverso vazio (discutido).
- Linha 367 (residuo v7): fechamento generico (discutido).

**Recomendacao**: cortar linhas 363-367 inteiras. A conclusao deve fechar com a frase-marca (algo como linha 361, parafraseada).

---

### Citacoes — **Algumas problematicas (problemas v7 parcialmente persistem)**

#### Citacoes de substancia

**Persistem da v7:**

- **Linha 69**: `[@Forozish_2024; @Goldsmith_2024; @Angrist_Pischke_2010]` — `Forozish_2024` e SSRN paper de Ali Omar Forozish, autor sem trajectory verificavel. Citado lado a lado com Goldsmith-Pinkham (Yale, conhecido) e Angrist-Pischke (canonico). **Sinal de citacao estrategica para inflar peso**. Recomendacao: cortar Forozish ou substituir por Currie-Kleven-Zwiers (2020), que e o paper de referencia que Goldsmith-Pinkham 2024 atualiza.
- **Linha 245**: `[@fairfield_charman2025; @rabbia_2023]` — chave `fairfield_charman2025` SEM underscore antes do ano, enquanto o resto do paper usa `_2022`, `_2017`. Inconsistencia herdada. Padronizar.
- **Linha 340**: `[@fairfield_charman2023]` — mesmo problema.
- **Linha 45**: `@spirling_stewart2025` — sem underscore antes de 2025. Inconsistencia herdada.

**Verificacao positiva**: as 22 refs novas adicionadas no lit-review de 2026-05-08 (Rezende_2017, Rezende_2019, Mesquita_2017, Paula_2018, Leite_Rocha_2019, Figueiredo_etal_2021, Bachini_Chicarino_2018, Amorim_Rodriguez_2016, Rezende_2011, Sposito_etal_2022, Vick_Lavalle_2020, Perissinotto_2024, Silva_2023, lipton2004ibe, etc.) **estao todas usadas no corpo do texto**, principalmente na Secao 2. Otimo do ponto de vista Edmans (cita o que usa, nao infla por citar).

#### Inconsistencias de formato

- **Linha 57**: `[@Silva_2023, ENAP/DCP-USP]` — usa **virgula** dentro do colchete para qualificar a citacao. O CSL Chicago author-date espera `[@Silva_2023, p. xx]` ou `[@Silva_2023]` apenas. `ENAP/DCP-USP` parece ser glosa do autor; melhor reescrever fora da citacao: `o manual mais recente de desenho de pesquisa para a comunidade brasileira [@Silva_2023], publicado pela ENAP em parceria com DCP-USP, ...`
- **Linha 59**: `@Sposito_etal_2022` — chave correta, formato correto.
- **Linha 69**: `[@Forozish_2024; @Goldsmith_2024; @Angrist_Pischke_2010]` — formato bem (`;`).
- **Linha 69**: `incluindo a ciência política [@Samii_2016; @Keele_2015a; @Keele_2015b; @Grimmer_2015]` — quatro citacoes em sequencia. Para `incluindo a ciencia politica`, basta uma (Samii_2016 ou Keele_2015a). Cortar duas.
- **Linha 71**: `[@Card_2022; @Angrist_Pischke_2010; @Angrist_Pischke_2009; @Angrist_Krueger_1991; @Card_Krueger_1994]` — cinco citacoes. Mesmo problema da v7. Tres bastariam.

#### Mis-citacoes / citacoes estrategicas

- **Linha 69**: `Lundberg_etal_2021; @Libman_2023; @Glied_2021` — para `varias disciplinas que lidam com dados observacionais`. Lundberg-Johnson-Stewart e CP/sociologia, nao `outras disciplinas`. Libman e Glied podem ser direito/saude, mas nao sao mobilizadas em outro lugar do paper. Citacao estrategica para ampliar reach.
- **Linha 41**: `@brady_collier_2010` — usado corretamente como representante da resposta qualitativa pos-KKV.
- **Linha 47**: `@Pearl_Bareinboim_2011` para colisor — uso correto.

#### Citacoes positivamente substanciadas

- @Silva_2023, @Sposito_etal_2022 — sao **pecas centrais da evidencia** da Secao 2, e a substanciacao no paragrafo das linhas 57-65 e detalhada (cita o que importa Silva, cita o que importa Sposito et al, cita os 4-5 outros autores BR como precedentes parciais). Edmans-aprovado.
- @spirling_stewart2025 — referencia central da Secao 4.3, com argumento de distincao explicito. Otimo uso.
- @fairfield_charman_2022, @Humphreys_Jacobs_2023 — referencias centrais da Secao 6, tratadas em profundidade. Adequado.

---

## Comparacao com v7 (Exposition 4.0/10 no review anterior)

A v8 melhorou substancialmente em **5 das 6 dimensoes do Exposition Edmans**:

| Dimensao | v7 | v8 | Delta |
|---|---|---|---|
| Erros mecanicos (typos, equacoes) | Multiplos graves | Eliminados | +2.0 |
| Abstract (clareza/marca) | Generico | Tem frase-marca | +1.0 |
| Introducao (estrutura/extensao) | Inflada com mini-survey | Limpa, 7 paragrafos | +1.5 |
| Argumentacao macro | Repete tese 5x sem nuance | Repete tese 9x mas com nuances v.1-4 | +0.5 |
| Citacoes (substancia) | 3 nomes nao verificados | Mesmos 3 persistem (Forozish, Goldsmith) | 0.0 |
| Citacoes (formato) | `;` vs `,` mistura sistematica | `;` agora consistente; underscore inconsistente | +0.5 |
| Trechos vagos / fechamentos genericos | "esperamos que inspire" | Mantido na conclusao (linhas 365, 367) | 0.0 |

**Total delta**: +5.5 pontos de melhoria distribuida; menos -3.0 de problemas residuais (redundancia + Bayes 101 + residuos v7 na Secao 9 e conclusao). Resultado liquido: 4.0 → 6.5.

**Permanece da v7**:
- Trechos finais residuais (linhas 327-336, 363-367) **nao foram reescritos** e quebram o registro novo.
- Citacoes estrategicas (`Forozish_2024`, lista de 5 nas linhas 69 e 71) ainda estao la.
- Inconsistencia da chave `fairfield_charman2023/2025` e `spirling_stewart2025` **nao corrigida** (verificavel no validate-bib report 2026-05-09).
- Lista de queries causais (linha 219-223) ainda lista 4 e desenvolve 3 — ATE faltando.
- Subsubsecao com 3 frases (linhas 189-193) ainda mau uso de hierarquia.
- Ambivalencia `process tracing / rastreio / rastreamento` ainda presente.

**Novo na v8 (positivo)**:
- Distincao Spirling-Stewart explicita (Secao 4.3) — e o movimento mais inteligente do paper.
- Ilustracao do impeachment (Secao 8) com tabela e calculos em decibeis — concretiza o argumento.
- DAG do incendio em TikZ (linha 83-99) — visual aid bem feito.
- Tese tripla declarada com clareza no abstract e na intro.

**Novo na v8 (negativo)**:
- Redundancia entre Secao 2 (linha 61) e Secao 4 (linha 129) sobre EQ/PE/TC — material identico em dois lugares.
- Camada de articulacao da tese aparece 9 vezes (era 5 na v7) — repeticao didatica que perde forca.

---

## Veredicto geral sobre exposition

A v8 **passa o gate Edmans "passo zero"** (nao tem typos que sinalizam descuido total), o que a v7 nao passava. O abstract e a introducao sao agora dignos do registro de top journal. A Secao 4 (centro do argumento) e a Secao 8 (ilustracao do impeachment) sao **excelentes** do ponto de vista expositivo. A distincao Spirling-Stewart na Secao 4.3 e dimensoes argumentativas novas que a v7 nao tinha.

**O que falta para subir de 6.5 para 7.5/8.0**:

1. **Cortar 35-40% das palavras**: Secao 5 (Solucoes Praticas) reduz a 1 paragrafo de transicao; Secao 6.1.1 (Definicao de Prioris) reduz a 1 paragrafo; Secao 6.1.2.1 (Evidencias) funde no anterior; ATE da lista 6.2 ou e desenvolvido ou removido.

2. **Reescrever os residuos v7**: linhas 327-336 (Secao 9 segunda metade), linha 348 (Secao 9 fim), linhas 363-367 (conclusao final). Estes trechos quebram o registro v8 e contem inconsistencias logicas (linha 348) e fechamento generico (linha 367).

3. **Eliminar redundancia argumentativa**: a tese tripla aparece 9 vezes. Concentrar em 4 momentos: abstract, intro paragrafo 6 (linha 47), fim da Secao 4, conclusao. As outras 5 ocorrencias podem ser omitidas ou parafraseadas.

4. **Corrigir citacoes inconsistentes**: padronizar `fairfield_charman_2022/2023/2025` e `spirling_stewart_2025` com underscore antes do ano. Trocar `Forozish_2024` por Currie-Kleven-Zwiers (2020) ou cortar.

5. **Padronizar terminologia**: escolher uma forma de `process tracing` e usar consistentemente. Idem para "tipos causais" (sugerir glosa do original ingles).

A boa noticia: nada disso e estrutural. Sao todos enderecaveis em revisao linha-a-linha de 1-2 dias. **A v8 ja tem ossatura adequada e tese forte**; falta apenas o ultimo polimento de acabamento.

---

## Top 5 sugestoes de melhoria

1. **Cortar a Secao 5 (`Solucoes Praticas`) a 1 paragrafo de transicao.**
   - Estado atual (linhas 133-149): 3 subparagrafos misturando Bayes 101 + tese sinal-ruido + sumario do teorema.
   - Reescrita sugerida: `# Inferencia Bayesiana: a maquinaria minima\n\nEstabelecida a comparacao Bayesiana de explicacoes rivais como tecnologia de credibilidade do quali de n pequeno, segue a operacionalizacao. A maquinaria e o teorema de Bayes — $p(H \\mid E) \\propto p(H) p(E \\mid H)$ — que combina a probabilidade *a priori* sobre hipoteses rivais com a verossimilhanca da evidencia para produzir a *posterior* sobre as hipoteses. A tese central que importa para o argumento desta nota e que o tamanho amostral nao e a variavel critica: o que governa a precisao da inferencia e a relacao sinal-ruido entre evidencia e hipoteses concorrentes, e estudos qualitativos bem desenhados sao os que selecionam contextos de alto sinal-ruido. Introducoes acessiveis a maquinaria Bayesiana qualitativa em @fairfield_charman_2022.`
   - Espaco economizado: ~150 palavras + clareza.

2. **Reescrever o final da conclusao (linhas 363-367).**
   - Estado atual: tres paragrafos residuais da v7 que quebram registro (`mostramos que a inferencia Bayesiana oferece solucoes robustas...`, `Por fim, abordamos como a literatura...`, `Esperamos que este estudo inspire...`).
   - Reescrita sugerida: substituir por **um paragrafo final** que feche com a frase-marca:
     `O eixo metodologico produtivo na ciencia politica brasileira pos-KKV nao e \"n grande vs n pequeno\". E a comparacao entre infraestruturas de credibilidade calibradas a cada regime: tecnologias de desenho com multiplas observacoes na quanti, comparacao Bayesiana de rivais sob inferencia a melhor explicacao em pequeno-n qualitativo. A objecao que disciplina o quali nao e mais \"e se houver um confundidor nao considerado?\", mas \"sua enumeracao de rivais foi exaustiva?\" — pergunta refutavel, produtiva, e inseparavel da credibilidade da inferencia.`
   - Espaco economizado: ~80 palavras + fechamento Edmans-aprovado.

3. **Eliminar redundancia argumentativa entre Secao 2 e Secao 4.**
   - Linha 61 (Secao 2, segunda metade) repete a critica EQ/PE/TC que aparece de novo em linha 129 (Secao 4.4) com texto quase identico.
   - Recomendacao: na Secao 2 (linha 61), reduzir a uma frase: `@Sposito_etal_2022 organizam a selecao de casos em torno de tipos de alegacao causal — probabilistica, mecanistica, conjuntista — sem mobilizar o vocabulario formal Pearl/Rubin nem a comparacao Bayesiana de rivais. A critica analitica dessa tipologia e desenvolvida na Secao 4.4.`
   - Cortar o paragrafo da linha 63 (analise da `validade interna intrinsica` e `lógica determinista`) e mover seu conteudo essencial para a Secao 4.4 (linha 131, depois da decomposicao).
   - Espaco economizado: ~250 palavras + reducao de uma das nove ocorrencias da tese.

4. **Reescrever o paragrafo final da Secao 9 (`Transportabilidade`, linha 348).**
   - Estado atual: `Como esse exemplo mostra, a generalizacao e dificil de ser realizada e as solucoes propostas na literatura estao longe de serem ponto pacifico. Contudo, tampouco e algo resolvido na abordagem influenciada pela revolucao da credibilidade. Se algo, por hora parece mais facil enderecar essa questao na pesquisa qualitativa do que na quantitativa, como mostramos acima. Mas e a pratica aplicada que ira dizer em que medida cada abordagem ira lidar com esse problema.`
   - Problemas: prosa coloquial (`por hora`, `Se algo`), claim contraditorio com o resto do paper (`mais facil enderecar na quali`), e fechamento generico.
   - Reescrita sugerida: `Validade externa permanece problema metodologico aberto em ambas as abordagens. A perspectiva aqui defendida nao oferece solucao do problema, mas sim disciplina sobre o que conta como solucao: redefinicao de escopo so opera como argumento substantivo quando acompanhada de mecanismo causal independente que distingue o sub-conjunto novo. Sem isso, a finitude do conjunto de rivais que sustenta a comparacao Bayesiana e subvertida pela propria invocacao do escopo.`
   - Espaco economizado: ~50 palavras + coerencia logica restaurada.

5. **Padronizar chaves bibtex inconsistentes e cortar `Forozish_2024`.**
   - Acoes mecanicas:
     - `fairfield_charman2023` → `fairfield_charman_2023` (linha 340)
     - `fairfield_charman2025` → `fairfield_charman_2025` (linha 245)
     - `spirling_stewart2025` → `spirling_stewart_2025` (linhas 45, 115, 125)
     - Atualizar entradas correspondentes no .bib se necessario.
   - Substituir `[@Forozish_2024; @Goldsmith_2024; @Angrist_Pischke_2010]` (linha 69) por `[@Goldsmith_2024; @Angrist_Pischke_2010]` ou por `[@Currie_etal_2020; @Goldsmith_2024]` se existir essa entrada (verificar bib). Forozish e SSRN paper de autor sem trajetoria — citar lado a lado com Angrist-Pischke e custoso para a credibilidade do proprio paper.
   - Custo: 30 minutos de trabalho mecanico (validate-bib + edits localizadas).
   - Beneficio: elimina o sinal de citacao estrategica que persiste da v7.

---

**Fim do parecer.**

---

## Anexo A — Score breakdown

```
Clareza:      Boa  (era Fraca em v7)            +1.5
Extensao:     Adequado, com inflacao localizada +0.5
Citacoes:     Algumas problematicas             +0.5
              (problemas v7 parcialmente persistem)

Score base:   v7 = 4.0
Delta v7→v8:  +2.5
Score v8:     6.5
```

## Anexo B — Estatisticas mecanicas

- Total de palavras Rmd: 10.696 (incluindo YAML, comentarios, metadados)
- Palavras corpo principal estimado: ~7.500-8.000
- Numero de paginas estimado (compilado, double-spaced, 12pt): ~17-19
- Notas de rodape: 2 (Rmd-style + LaTeX-style)
- Equacoes display: 6 (sem typos detectados)
- Figuras: 1 (DAG do incendio em TikZ)
- Tabelas: 1 (verossimilhancas do impeachment)
- Secoes top-level: 11 (incluindo References)
- Subsecoes (`##`): 16
- Subsubsecoes (`###`): 4
- Citacoes unicas estimadas: ~70 chaves distintas
