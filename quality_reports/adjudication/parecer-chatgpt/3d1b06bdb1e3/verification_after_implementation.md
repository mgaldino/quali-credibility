# Verificação independente após implementação — 3 de outubro de 2026

**Resultado:** PASS delimitado para a aplicação dos nove IDs encaminhados e para a consistência das decisões do Problema 1. Nenhum defeito material introduzido permanece aberto no snapshot abaixo. A adjudicação substantiva sobre exaustividade e o redesign empírico de complexidade/dependência permanecem pendentes; este PASS não os resolve.

**Escopo:** comparação do Rmd completo com o baseline preservado; inspeção do script R, filtro Lua e referências adicionadas; reprodução independente de números e comparação com três CSV. A verificação foi realizada por adjudicador distinto do implementador e não editou arquivos canônicos. A cor e a disposição visual do PDF são verificadas pelo agente principal.

## Identidade dos arquivos

Baseline: `artifacts/baseline_source.Rmd`, SHA-256 `3d1b06bdb1e35c3ea85baed3a1611741fceebf8e2e8a656afc27378e80e3383a`.

Snapshot final conferido às 10:12 de 3/10/2026, horário de São Paulo:

**Tabela 1. Hashes dos artefatos da implementação conferida.**

| Arquivo | SHA-256 |
|---|---|
| `paper_dados_format_quali.Rmd` | `a0fabc7bca51c28ab9190b266a4c6726527fd3d35003093c98abb3a010d92386` |
| `paper_dados_format_quali.pdf` | `959f7e87ca334925052e01d817ab1d15f3fe5728fc65d10252418d07bb97cc4e` |
| `Quali-credibilidade.bib` | `ffd6e6be2e2bc7657ecb7f497f1be652d6781b439181ed38f90c6b0c0c49d0e5` |
| `scripts/impeachment_bayes_example.R` | `ba0809181d727c27b7947155b212dc19bfaf3fdffa598c8879ebc738f57481c4` |
| `scripts/codex_highlight.lua` | `b33e2b3a915c8003b8aeb6c84fd55901c86eb66ffeb4c4298eebe1aaa6143d7e` |
| `data/derived/impeachment_bayes_example/posteriors.csv` | `a8a5f8ed50a5e91b211d03f7b913d3fe9b8e4a5fe2e77254a6d15deadad4df77` |
| `data/derived/impeachment_bayes_example/leave_one_evidence_out.csv` | `4ad6a0d706358faf7674d1ab14769d22f45dae3035c8695a6d7a454b62832036` |
| `data/derived/impeachment_bayes_example/prior_odds_thresholds.csv` | `5445ebacd4fb1234de7bebb4876c8b9bb37e8206c294b248e820d0e508f3bc86` |

## Aplicação dos nove achados

**Tabela 2. Correspondência entre encaminhamento e implementação.**

| ID | Trecho corrente | Resultado |
|---|---|---|
| R1-F001 | Rmd 449, 452, 456 e 467 | Identificação do efeito distinguida da credibilidade explicativa. A definição ampla de McDermott aparece atribuída como acepção alternativa. |
| R1-F003 | Rmd 504 | Remoção de rival preserva odds; adição de candidata pode superar a vencedora sem reordenar as antigas. LOEO examina evidência e não prova exaustividade. |
| R1-F005 | Rmd 383–387 e 504 | Regra sequencial correta. A matriz continua de marginais e o texto exige novos julgamentos ou modelo conjunto para condicionais incrementais. |
| R1-F006 | Rmd 260 e 365 | Soma 1 qualificada por exclusividade e exaustividade. Índice único de modelo comparado distinguido de mecanismos coexistentes e de cobertura substantiva. |
| R1-F007 | Rmd 278 | Binariedade apresentada como simplificação; generalização limitada a variáveis discretas e custo do espaço de tipos. |
| R1-F008 | Rmd 294 e 298 | Probabilidade posterior do tipo do caso separada de proporção populacional. O caso Lula III e sua codificação foram preservados. |
| R1-F010 | Rmd 140–142 | Crenças sobre suposições causais preservam proposições sobre o mundo e admitem modelos com choques estocásticos. |
| R1-F011 | Rmd 226, 232, 238, 322, 407 e 434 | Prior uniforme é cenário explícito; partição, background e sensibilidade expostos, sem desconto arbitrário a H6. |
| R1-F012 | Resumo, Rmd 58, 176 e 180 | Spirling–Stewart admitem evidência sem parâmetro identificado e mecanismos qualitativos. Contribuição formulada como operacionalização, sem a exclusão indevida antes atribuída a eles. |

## Decisões do Problema 1

As definições são dadas pela fonte da aleatoriedade em Rmd 122–130. O texto admite resultados potenciais fixos em design-based, choques estocásticos do mundo em model-based e variação de unidades sob sampling-based. O censo finito elimina a incerteza de amostragem; a proximidade conceitual do experimento mental de superpopulação e model-based está preservada.

O mesmo desenho admite justificativas diferentes; o exemplo DiD não classifica técnicas em famílias fixas. O texto mantém explicitamente identificação e inferência como perguntas distintas mesmo com suposição comum. O termo de erro, a necessidade de explicitar resultados potenciais e a ausência de dicotomia ontológica necessária aparecem em 132–134. Mahoney–Goertz e KKV servem como evidência da fronteira internacional, preservando o alvo brasileiro da crítica.

O paralelo com as condições de identificação é defendido como justificação substantiva em Rmd 170. A tese de uma lógica causal comum e de inferências distintas foi preservada em Rmd 58; o texto não a substitui por uma lógica inferencial única.

Dois pontos surgiram na conferência inicial e foram corrigidos pelo implementador antes deste snapshot: a troca da tese de lógica causal por simples vocabulário comum e a definição de McDermott apresentada sem marcação de acepção alternativa. Não restam como findings abertos nesta implementação.

## Verificação numérica e dos outputs

Executado da raiz:

```bash
Rscript quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/verification_checks.R
```

O script fonte corrente foi executado em diretório temporário. Não se sobrescreveram os outputs canônicos para obter igualdade. As três matrizes de likelihoods e os três vetores de resultados são idênticos ao baseline por comparação de objetos R; os CSV canônicos foram lidos e comparados com os resultados recalculados, com tolerância 1e-12.

O LOEO foi recalculado diretamente dos produtos de likelihoods de base, independentemente do objeto novo. H6 permanece preferida nas sete remoções; sua posterior mínima é 0,5680042 e a máxima é 0,6522987. O fator de Bayes H6/H4 é 3,6886191 e o limiar das prior odds é 0,2711042. Os arredondamentos 3,689; 0,271; e 0,568–0,652 em Rmd 434 correspondem a esses resultados. A comparação permanece par a par e condicionada às mesmas likelihoods; o texto não a usa como prior elicitada, custo Occam ou tratamento da dependência.

Output completo: `verification_checks_output.txt`. O primeiro rascunho deste teste continha um valor de BF transcrito com precisão excessiva e falhou nessa constante de referência; o teste foi corrigido para confrontar os arredondamentos efetivamente publicados e os objetos/output independentes. A execução final passou. Esse erro estava no teste do adjudicador, não no script ou nos números do manuscrito.

## Cobertura da marcação amarela no fonte

O scan da diferença de linhas encontrou 47 linhas de conteúdo acrescentadas/substituídas marcadas em divisões `.codex-edit` e **zero** linhas novas de corpo sem marcação. As divisões estão fechadas. Os quatro novos citation keys que produzem entradas novas na bibliografia estão no mapa do filtro Lua: Abadie et al. 2020, Chen–Pearl 2013, de Chaisemartin–D’Haultfœuille 2026 e Mahoney–Goertz 2006. Configuração técnica no YAML foi excluída da contagem de prosa; `@inlabel` é parte de uma macro TeX, não citation key.

A inspeção do filtro confirma processamento de citeproc antes da marcação das entradas novas e tratamento das notas de um parágrafo. O texto extraído do PDF contém a tese causal restaurada, a acepção alternativa de validade interna e os números LOEO. Isso verifica presença textual; não comprova cobertura visual amarela ou ausência de cortes. Evidência do scan: `verification_source_coverage.json`.

## Conferência dos ajustes finais de composição

Após o primeiro snapshot, a QA visual levou a três ajustes de composição: largura da caixa de notas reduzida em 1,8 em; bibliografia em 11 pt, alinhada à esquerda, com penalidades contra linhas viúvas e órfãs; separação de dois trechos matemáticos inline para permitir quebra na primeira comparação de decibéis. O parágrafo dessa comparação está agora num bloco amarelo.

A comparação de decibéis é idêntica à do baseline após remover apenas delimitadores de modo matemático e normalizar espaços. Não mudou a fórmula, a evidência, o par de hipóteses, o número ou a conclusão. A inspeção do Lua mostra as mudanças apenas na largura da parbox e na composição local da bibliografia. O hash da bibliografia, do script analítico e dos três outputs numéricos é idêntico ao snapshot anterior. Assim, permanecem válidos os testes numéricos e a verificação dos nove IDs; a versão final não acrescentou mudança substantiva observável nessa etapa.

O PDF final tem 45 páginas segundo pdfinfo. O scan de cobertura foi atualizado para a nova composição. Fontes finais foram preservados em `artifacts/implementation_final_source.Rmd` e `artifacts/implementation_final_highlight.lua`, sem modificar o baseline adjudicado.

## Pendências e limites

Exaustividade continua como compromisso central e pendência substantiva R1-F002. A implementação limita o poder dos diagnósticos sem declarar exaustividade demonstrada ou abandoná-la. Complexidade da hipótese composta e dependência entre evidências continuam exigindo trabalho numa aplicação substantiva; a matriz, os cenários e a hipótese integrada de Limongi foram preservados.

Não houve nova revisão ampla, verificação de originalidade, elicitação empírica ou reconstrução histórica do impeachment. Não se releu o proof BPSR para emitir juízo geral de voz. A prosa foi conferida apenas quanto às intervenções autorizadas e à preservação das teses/documentação vinculantes. A auditoria visual do PDF pertence ao agente principal.

