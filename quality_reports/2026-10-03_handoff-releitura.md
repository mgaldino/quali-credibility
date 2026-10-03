# Passagem de sessão — releitura da v8 (Claude Code → Codex)

**Data**: 2026-10-03
**Origem**: sessão Claude Code de 2026-10-02/03, interrompida por limite de uso.
**Leitura obrigatória antes de continuar**: este arquivo, depois `quality_reports/2026-10-02_notas-releitura.md` inteiro.

---

## 1. O que está em curso

O autor está relendo o PDF compilado da v8 (`paper_dados_format_quali.pdf`) e ditando os problemas que encontra. Nas palavras dele: "por enquanto são só notas dos problemas para a gente pensar e ver como vai endereçar".

Modo de trabalho combinado:

- **Fase de notas.** Registrar e discutir cada problema. Não editar `paper_dados_format_quali.Rmd`, `.bib` ou PDF até o autor decidir como endereçar.
- **Registro único**: `quality_reports/2026-10-02_notas-releitura.md`. Cada problema novo entra como `## Problema N — título`, com a mesma estrutura do Problema 1:
  1. *Argumento do autor (ditado na releitura)*: a posição dele, fiel ao que disse.
  2. *Onde o manuscrito está hoje (diagnóstico)*: com números de linha do `.Rmd`.
  3. *Pontos discutidos com o autor*: itens a, b, c… marcados como resolvido / não discutido.
  4. *Decisões*: formato `### Decisão: tópico` com **Escolha** e **Alternativas descartadas** (e por quê). Quando o autor rejeitar uma sugestão do agente, ela entra como alternativa descartada e não volta a ser proposta.
- **Ditado por voz.** As mensagens chegam transcritas, com erros em nomes próprios. Exemplo desta sessão: "Jason C. Wright" = Seawright. Interpretar pelo contexto e, se o nome importar para citação, confirmar.
- **Respostas**: em português, curtas. Quando discordar, argumentar com fonte e página. Quando o autor já decidiu, registrar e seguir.
- **Literatura**: o autor pede checagem quando quer saber se a definição dele bate com a literatura. Separar sempre o que foi conferido no PDF do que veio só de busca.

Próximo passo imediato: **esperar o autor ditar o Problema 2** (o placeholder já existe no fim do arquivo de notas).

---

## 2. Arquivos desta frente

| Arquivo | Conteúdo |
|---|---|
| `quality_reports/2026-10-02_notas-releitura.md` | Registro principal. Problema 1 completo, Problema 2 vazio. |
| `quality_reports/2026-10-02_lit-check-design-model-sampling.md` | Checagem de literatura (37 fontes, status de verificação, conflitos terminológicos, BibTeX das fontes verificadas). |
| `DiD_deChaisemartin_dHaultfoeuille.pdf` (raiz do repo, ignorado pelo Git) | de Chaisemartin & D'Haultfœuille, versão de 27/02/2026. §2.4 "Discussion of the book's perspective on statistical inference", pp. 28–32. Referência-base das três definições. |
| `~/Documents/DCP/Cursos/Causalidade/cópia de BOOK CREDIBLE ANSWERS.pdf` | Rascunho de 2024 do mesmo livro. §2.4 "Framework for statistical inference", pp. 20–22. |
| `~/Documents/DCP/Cursos/stat_basica/King, Keohane, Verba Designing Social Inquiry.pdf` | KKV, pp. 59–60 (Perspectivas probabilística e determinística). |
| `../mahoney-e-goertz-2006-a-tale-of-two-cultures-contrasting-quantitative-a.pdf` (diretório pai) | Mahoney & Goertz 2006: Tabela 1 p. 229; pp. 233–234; p. 239 n. 12. |
| `~/Zotero/storage/9UFD6N87/` | Keele 2015 (§4 e nota sobre "design-based"). |

Trechos do `.Rmd` mais citados no Problema 1 (numeração do working tree em 2026-10-02):
l. 61 (crítica a Sposito et al.; dicotomia "dispensável"), l. 73–77 (definição de identificação; l. 75 "sem tomar partido sobre a fonte ontológica"), l. 79–99 (exemplo INUS do incêndio + DAG TikZ), l. 101–107 (§"Variantes da identificação: desenho vs. modelo", onde está o erro principal), l. 109–113 (§"Framework Bayesiano subjetivista"; l. 111 menciona as três fontes numa frase só), l. 233 (relação determinística, conhecimento probabilístico).

---

## 3. Problema 1 — estado

Tema: justificativas da inferência (design / model / sampling-based) e a dicotomia ontológica "quanti probabilístico × quali determinístico". Detalhe completo no arquivo de notas.

### Decidido pelo autor

1. **Definições** pela fonte da aleatoriedade, como em dC&DH §2.4:
   - *design-based*: aleatoriedade na alocação do tratamento; resultados potenciais podem ser fixos e a relação causal, determinística;
   - *model-based*: desenho fixo ou condicionado; resultados potenciais com componente estocástico; a incerteza persiste com a população inteira (censo, dados administrativos);
   - *sampling-based*: unidades sorteadas de população maior (finita ou superpopulação); desenho e resultados potenciais ambos aleatórios.
   Cuidado de redação: dC&DH dizem que model-based e sampling-based "do not greatly differ" quando a amostra cobre todas as unidades; definir pela fonte da aleatoriedade sem afirmar que levam sempre a procedimentos distintos.
2. **Erro do manuscrito**: l. 101–107 definem design/model-based por aplicação (RDD/IV = design; DiD/SCM = modelo). Os termos classificam a justificativa da inferência, e o mesmo desenho admite justificativas diferentes (DiD com tendências estocásticas = model-based; DiD com choque aleatório = design-based).
3. **Terminologia**: adotar o sentido inferencial de "design-based" e reconhecer em nota de rodapé os outros dois usos (amostragem de surveys; "primado do desenho" na identificação, como em Dunning, Sekhon, Keele). Precedentes da nota: Keele 2015; Aronow, Jang & Offer-Westort 2026, n. 1.
4. **Leitura ontológica** do componente estocástico no model-based: ele é do mundo. Exemplo do autor: efeito das operações policiais do segundo turno de 2022 sobre o comparecimento varia com a chuva, e a chuva é aleatória. Redigir como suposição explícita do pesquisador, como em dC&DH ("nature draws some shocks"). *Leitura descartada*: estocasticidade como resumo instrumental de causas não modeladas.
5. **Núcleo do argumento (termo de erro)**: na regressão pré-CR, ε podia ser causas determinísticas omitidas ou aleatoriedade do mundo, e a escolha ficava mal definida porque o resultado potencial não era modelado. Modelar o resultado potencial obriga a dizer onde está a aleatoriedade (alocação, resultado potencial, amostragem), e com isso obriga a explicitar a ontologia. Nada intrínseco ao quanti exige uma das três justificativas, logo não há diferença ontológica necessária entre quanti e quali.
6. **Alvo da dicotomia**: a literatura metodológica brasileira (Sposito et al. 2022 e afins); encaixa na Camada 2. A fronteira internacional já superou a dicotomia: Mahoney & Goertz (2006, p. 234) entram como evidência disso. KKV é aliado no ponto ontológico (pp. 59–60: as duas perspectivas são observacionalmente equivalentes e o argumento "applies with equal force to qualitative and quantitative researchers") e alvo no ponto da fusão entre identificação e inferência.
7. **Identificação ≠ inferência mesmo com suposição comum.** Identificação é a pergunta sobre amostra infinita (o estimando é pontualmente identificado?); ali não há problema de inferência por construção. Uma suposição identificadora pode ter implicações para a amostra finita (dC&DH derivam o nível de clusterização da perspectiva model-based) sem que as perguntas se misturem. *Alternativa descartada*: "no design-based a atribuição aleatória faz o duplo trabalho e o split precisa ser qualificado". **Não reabrir.** Detalhe opcional para a redação: em dC&DH, clusterizar no nível do estado faz tendências paralelas valerem incondicionalmente (versão ligeiramente mais fraca); a escolha do que é aleatório vem antes das duas perguntas e fixa o estimando e a forma da suposição.

### Em aberto (não discutido ou pendente)

- **e. Bayes como quarto sentido de "probabilístico"**: a probabilidade subjetivista (l. 113) é epistêmica e vale para relações determinísticas (l. 233). Organização possível: três justificativas frequentistas + probabilidade como grau de crença, nenhuma exclusiva do quanti. Não discutido.
- **g. Localização do argumento expandido**: dentro do §"Framework Bayesiano subjetivista" (l. 109–113) ou subseção própria antes dele, referenciada por l. 61 e l. 75. Não discutido.
- **Reancorar o paralelo CR/IBE** (l. 107, 139, 411) nas suposições de identificação, sem o rótulo design/model-based.
- **Revisar os demais usos de "design-based"**: l. 139, 145, 173, 285, 289, 361, 369, 411, 415.
- **Reescrever l. 75 e l. 113**, que hoje declaram a questão ontológica fora de escopo, e pagar a afirmação de l. 61.
- **Bibliografia** (nada adicionado ainda ao `.bib`):
  - dC&DH: sai pela Princeton UP como *Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions* (previsto para 8/12/2026, copyright 2027; preprint SSRN 10.2139/ssrn.4487202). Citar por seção.
  - Chen & Pearl 2013, *real-world economics review* 65: 2–20 (revisão de livros-texto de econometria).
  - Citar `mahoney_goertz_2006`, `mahoney_2008` e `abadie_etal_2020` (já no `.bib`, hoje sem citação no corpo).
  - VanderWeele & Robins 2012 (causas suficientes estocásticas) como ponte possível com o exemplo INUS: conferir.
  - Seawright: o autor indicou que o prenome mudou. Confirmar a forma atual do nome antes de citar.
- **Conferir ao redigir**: as operações de 30/10/2022 foram da PRF (Polícia Rodoviária Federal).
- **Fontes do lit-check que só o subagente conferiu** (via arXiv/DOI, sem leitura do PDF pelo agente principal): Athey & Imbens 2017, Abadie et al. 2023, Rambachan & Roth 2026, Aronow, Jang & Offer-Westort 2026, data de publicação pela PUP.

---

## 4. Regras que o Codex não vê

As regras abaixo vivem na memória do Claude Code (`~/.claude/projects/.../memory/`) e no `~/.claude/CLAUDE.md` global. Valem para este repo.

**Do autor sobre o conteúdo do paper**

- **Identificação ≠ inferência**: item 7 acima. Não tratar uma suposição comum às duas perguntas como falha do argumento.
- **Seleção de casos**: manter separados (a) selecionar pela VD, que induz viés de colisor e é problema de identificação, e (b) seleção não-aleatória, que é problema de representatividade/validade externa. KKV funde; o paper decompõe. Teste: Card-Krueger 1994 (N=2) e o SCM da reunificação alemã (N=1).
- **Claims genealógicos** ("X é resíduo/herança de Y") exigem lit-review ou hedge explícito. Alternativa defensável: paralelo lógico-funcional.
- **Precisão metodológica**: não simplificar afirmações sobre identificação, suposições de desenho ou estimadores a ponto de perder condições essenciais. Nomear as side conditions (SUTVA, adesão etc.) ou usar "tipicamente"/"em geral".

**Prosa**

- Evitar "não é X, mas Y" quando o X negado é um espantalho. Teste: apagar a metade negada; se o que sobra diz tudo, a frase era só isso. A regra vale para o texto que o agente escreve. `rather than`/"em vez de" ficam quando alguém de fato defenderia o X naquele ponto do texto.
- Paper atemporal: o texto sugerido não menciona versões anteriores, revisões ou "agora".

**Processo**

- Commits e push exigem autorização explícita, mas o agente deve **propor** o commit com mensagem quando houver trabalho pronto. Ficar em silêncio não cumpre a regra.
- Pareceres e reviews são salvos na íntegra em `quality_reports/YYYY-MM-DD_nome.md` antes de qualquer resumo.
- Decisões conceituais são registradas com as alternativas descartadas (formato da seção 1).
- Formulação CR/IBE em pequeno-n: vale a de `AGENTS.md` (recalibrada após o Devil's Advocate de 2026-05-09, commit `faefa26`). O `CLAUDE.md` do diretório pai ainda traz a formulação anterior ("IBE substitui CR").

---

## 5. Estado do Git em 2026-10-03

- Branch `main`; último commit `8fd9f24`.
- `paper_dados_format_quali.Rmd` e `.pdf` já estavam modificados (sem commit) antes desta sessão. A sessão de releitura não os tocou.
- Criados nesta sessão, sem commit: `quality_reports/2026-10-02_notas-releitura.md`, `quality_reports/2026-10-02_lit-check-design-model-sampling.md`, este arquivo e a seção "Trabalho Em Curso" em `AGENTS.md`.
- Outros arquivos sem rastreamento, anteriores à sessão, que não devem ser tocados sem perguntar ao autor: `notas auxiliares.Rmd`, `quality_reports/parecer_paper_qualitativo_bayesiano.md`, `quality_reports/plans/2026-05-09_cut-30-percent-plan.md`, `synth-trade-china.bib`.
