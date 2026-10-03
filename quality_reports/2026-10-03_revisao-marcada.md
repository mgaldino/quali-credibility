# Revisão marcada do paper — 3 de outubro de 2026

O Problema 1 foi implementado, e os componentes locais verificáveis do parecer do ChatGPT foram corrigidos. O PDF foi recompilado com as intervenções em amarelo e conferido nas suas 45 páginas. As matrizes, as tabelas numéricas e os cenários originais do exemplo de impeachment foram preservados. Esta entrega não resolve a proposta substantiva de abandonar a exaustividade, nem transforma a ilustração didática em adjudicação empírica do impeachment.

## Autorização e referência de voz

O pedido de 3/10 autorizou endereçar os problemas documentados, deixando o Problema 2 de lado. O autor exigiu amarelo para tudo que o agente escrevesse no paper e máxima preservação de sua voz, com leitura do original da BPSR.

Foi lido integralmente `../BPSR-2024-0056.R1_Proof_hi.pdf`, incluindo o texto e as referências. SHA-256: `f54693a5e15e1129eeed551fe4a9651c74b63e1230c35689d19e3de522fec4e0`. A referência de voz foi o encadeamento entre premissa, exemplo e implicação, o plural em primeira pessoa e a explicação didática das distinções. A confusão conceitual do original não foi usada como restrição. A preservação de voz é um julgamento editorial oferecido à revisão do autor, não um resultado que os testes mecânicos possam demonstrar.

As mudanças preservam a tese de uma lógica causal comum com caminhos inferenciais distintos, a separação entre identificação e inferência mesmo com uma suposição comum e a interpretação integrada de Limongi. Não se substituiu essa tese por mera compatibilidade de vocabulários.

## Material e baseline

Foram lidos `AGENTS.md`, o handoff de 3/10, as notas de 2/10 e o parecer integral `quality_reports/parecer_paper_qualitativo_bayesiano.md`. O parecer permanece intacto, SHA-256 `a662894c0003a6b8340471ed3d5c189d269e034997e8a84d4c691333fd9961d8`.

O baseline usado é o Rmd encontrado no início desta implementação, correspondente a `4af3f60:paper_dados_format_quali.Rmd`, SHA-256 `3d1b06bdb1e35c3ea85baed3a1611741fceebf8e2e8a656afc27378e80e3383a`. Ele está preservado em `adjudication/parecer-chatgpt/3d1b06bdb1e3/artifacts/baseline_source.Rmd`. O PDF anterior tinha 41 páginas. O estado antigo descrito no handoff não foi tomado como retrato do Git corrente.

O parecer foi adjudicado por agente distinto do implementador antes das correções. Registros integrais, fontes, verificações e limites estão em `adjudication/parecer-chatgpt/3d1b06bdb1e3/`. A conferência independente posterior está em `verification_after_implementation.md` nessa pasta.

## Alterações e sua finalidade

**Tabela 1. Intervenções implementadas e localização no PDF entregue.**

| Tema | Alteração | Localização |
|---|---|---|
| Problema 1 | Definir design-, model- e sampling-based pela fonte da aleatoriedade; incluir censo, superpopulação, exemplo hipotético da chuva e DiD com justificativas diferentes. | §3.2, pp. 9–11 |
| Ontologia e termo de erro | Explicitar o que pode ser aleatório, distinguir parâmetro causal de associação e negar uma diferença ontológica necessária entre quali e quanti. Preservar KKV e Mahoney–Goertz como aliados nesse ponto e o alvo brasileiro da crítica. | §2, p. 6; §3.1–3.3, pp. 7–11 |
| Bayes e suposições causais | Distinguir probabilidade epistêmica de propriedades do mundo; manter identificação e comparação de explicações como perguntas distintas. | §3.3, p. 11 |
| Condições de identificação | Retirar a classificação rígida RDD/IV versus DiD/SCM e reancorar o paralelo com a enumeração na defesa substantiva das suposições. | §3.2 e §4.2, pp. 9–11 e 15–16 |
| Spirling–Stewart | Reconhecer que admitem evidência causal sem parâmetro identificado, inclusive mecanismos qualitativos; formular a contribuição local como operacionalização da comparação de rivais. | Resumo, p. 1; introdução, p. 3; §4.3, p. 16 |
| Prioris e rivais | Tratar prioris uniformes como cenário, dependente da partição e do conhecimento anterior; qualificar soma 1 por exclusividade e exaustividade. | §6.1, pp. 20–22; §8.3–8.6, pp. 30–34 |
| Inferências integradas | Qualificar binariedade como simplificação; separar posterior do tipo do caso de proporção populacional. | §6.2, pp. 23–25 |
| Verossimilhanças | Acrescentar a regra sequencial exata. A matriz continua de marginais didáticas, com independência condicional explícita; condicionais incrementais exigiriam novos julgamentos ou modelo conjunto. | §8.5, pp. 31–33 |
| Sensibilidade | Acrescentar o limiar das prior odds e retirar uma evidência de cada vez, preservando números e cenários de base. | §8.6, pp. 33–34 |
| Validade interna | Resolver a contradição entre propriedade de identificação e credibilidade da explicação; atribuir a definição ampla de McDermott como acepção alternativa. | §9, pp. 34–36 |
| Protocolos finais | Substituir o diagnóstico impossível de remover uma rival por sensibilidade à evidência. Explicitar que remover ou adicionar uma rival, com pesos fixos, não reordena as antigas; uma rival nova pode superar a vencedora. Estabilidade não demonstra exaustividade. | §10, pp. 40–41 |

Os componentes autônomos encaminhados foram R1-F001, F003, F005, F006, F007, F008, F010, F011 e F012. O descompasso local de F009 foi tratado junto com o Problema 1, preservando a decisão autoral sobre causalidade e inferência. A correção local não implica aceitar as versões mais amplas de todos os achados parciais.

Foi acrescentada a referência à versão preliminar de 27/02/2026 do livro de de Chaisemartin & D’Haultfœuille, citada por §2.4, e Chen & Pearl (2013). Abadie et al. (2020) e Mahoney & Goertz (2006), já presentes no `.bib`, passaram a ser citados. As fontes pertinentes foram conferidas nos PDFs locais; Chen–Pearl e os capítulos pertinentes de Humphreys–Jacobs foram consultados nas fontes primárias online. As pontes bibliográficas opcionais das notas não foram introduzidas nesta intervenção.

## Como funciona o amarelo

O Rmd delimita os trechos alterados por `::: {.codex-edit}`. O filtro `scripts/codex_highlight.lua` converte esses blocos em fundo amarelo, também nas fórmulas e notas, e marca as quatro entradas novas da bibliografia. O processamento das citações ocorre uma vez, antes da marcação.

O amarelo cobre o parágrafo alterado por inteiro, incluindo frases do autor que permaneceram dentro dele. Isso permite conferir unidades legíveis sem deixar intervenções sem marcação. Portanto, a área amarela é maior que a quantidade de palavras efetivamente substituídas. A diferença exata permanece verificável contra o baseline ou pelo Git.

Os ajustes de composição foram locais: notas amarelas cabem na largura disponível, a fórmula inline de decibéis admite quebra de linha sem mudança textual ou numérica, e a bibliografia usa 11 pontos e alinhamento à esquerda para acomodar DOI e URLs. A fórmula e o DOI que ultrapassavam a margem já existiam no PDF anterior; foram detectados e resolvidos durante a conferência desta entrega.

## Verificações executadas

**Tabela 2. Evidência de verificação e alcance dos resultados.**

| Verificação | Resultado | Limite |
|---|---|---|
| Compilação RMarkdown/Pandoc/XeLaTeX | PDF produzido, 45 páginas. | Verifica a produção do artefato; não comprova o argumento. |
| Reprodução independente em R | Matrizes dos três cenários e vetores de resultados idênticos ao baseline; três CSV correspondem aos cálculos, tolerância 1e-12. | Modelo didático, sem elicitação empírica. |
| Sensibilidade nova | H6 preferida nas sete retiradas de evidência; posterior entre 0,5680042 e 0,6522987. BF H6/H4 = 3,6886191; limiar das prior odds = 0,2711042. | Condicional às mesmas verossimilhanças e à independência; não estima dependência nem prova exaustividade. |
| Cobertura no fonte | 47 linhas de conteúdo acrescentadas/substituídas marcadas; zero linhas novas de corpo sem marcação; quatro referências novas no filtro; blocos fechados. | Configuração técnica não entra na contagem de prosa. |
| Conferência independente do argumento e do diff | PASS delimitado para os componentes implementados e decisões do Problema 1. | Não é nova revisão geral, demonstração de originalidade ou aprovação de todas as propostas do parecer. |
| Inspeção visual | Todas as 45 páginas examinadas em quatro folhas de contato. Inspeção adicional a 120 dpi das pp. 9, 31, 32, 33, 42 e 44. Texto, notas, fórmulas e referências marcados sem cortes; tabelas legíveis, com cabeçalho repetido na continuação da Tabela 1. | Julgamento visual, registrado separadamente dos testes mecânicos. |
| Conferência mecânica do PDF | Todas as páginas renderizadas; presença de amarelo; zero palavras fora dos limites físicos ou das margens horizontais com tolerância. | Cor presente não comprova, sozinha, cobertura semântica da marcação. |
| Consistência do diff | `git diff --check` passou. | Não substitui a leitura das alterações. |

Comandos, executados da raiz do repositório:

```bash
Rscript scripts/impeachment_bayes_example.R
Rscript quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/verification_checks.R
Rscript -e 'rmarkdown::render("paper_dados_format_quali.Rmd", output_format = "pdf_document", output_options = list(keep_tex = TRUE), quiet = FALSE)'
python3 scripts/python/check_marked_pdf.py
git diff --check
```

Para a inspeção visual, o PDF foi renderizado com `pdftoppm -scale-to 792 -png`; as páginas de detalhe foram renderizadas a 120 dpi. O script mecânico usa Poppler, pypdf e Pillow, e grava `2026-10-03_pdf-qa.json`; ele gera imagens temporárias. A produção do PDF continua possível com o comando habitual do projeto, sem `keep_tex = TRUE`. O TeX intermediário usado na inspeção não é um novo fonte canônico.

O R exibiu avisos de locale na inicialização, mas terminou as execuções com código 0 e sem divergências nos cálculos. Não foram instalados pacotes nem executado `_targets::tar_make()`.

## Artefatos e identidade da entrega

- `paper_dados_format_quali.Rmd` e `paper_dados_format_quali.pdf`: fontes e PDF canônicos revisados.
- `Quali-credibilidade.bib`: duas entradas acrescentadas.
- `scripts/codex_highlight.lua`: marcação amarela e composição local da bibliografia.
- `scripts/impeachment_bayes_example.R`: cálculos originais preservados, diagnósticos e verificações acrescentados.
- `data/derived/impeachment_bayes_example/{posteriors,leave_one_evidence_out,prior_odds_thresholds}.csv`: resultados reproduzíveis do exemplo didático.
- `scripts/python/check_marked_pdf.py` e `quality_reports/2026-10-03_pdf-qa.json`: conferência mecânica do PDF.
- `quality_reports/2026-10-02_notas-releitura.md`: autorização atual, escolhas de implementação e abandono do Problema 2 registrados, com preservação da discussão anterior.
- `quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/`: adjudicação, baseline e conferência independente.

SHA-256 do Rmd entregue: `a0fabc7bca51c28ab9190b266a4c6726527fd3d35003093c98abb3a010d92386`.

SHA-256 do PDF entregue: `959f7e87ca334925052e01d817ab1d15f3fe5728fc65d10252418d07bb97cc4e`.

Hashes dos demais artefatos canônicos e CSV constam de `adjudication/parecer-chatgpt/3d1b06bdb1e3/verification_source_coverage.json`. Os hashes identificam o snapshot examinado; uma compilação posterior pode alterar metadados e bytes do PDF.

## Questões que permanecem abertas

1. **Exaustividade (R1-F002).** O parecer propõe substituí-la por adequação do conjunto. A adjudicação não encontrou uma contraprova que obrigue essa mudança de tese. Foi preservado o compromisso do artigo, com retirada das promessas falsas de que estabilidade dos diagnósticos o demonstraria. Enfraquecer ou manter esse compromisso continua sendo decisão substantiva do autor.
2. **Complexidade de H6 (R1-F004).** Não há penalidade numérica única derivada da crítica. O texto explicita a limitação e oferece sensibilidade às prioris. Uma comparação empírica exigiria definir a família de modelos e justificar as prioris pelo conhecimento anterior; não foi imposto desconto arbitrário à hipótese composta.
3. **Dependência numa aplicação substantiva (parte de R1-F005).** A regra sequencial está correta, mas a matriz existente não contém condicionais incrementais elicitadas. Não se renomearam probabilidades marginais para aparentar uma correção empírica. Estimar ou elicitar novos valores continua sendo trabalho adicional.

Não houve nova reconstrução histórica, auditoria integral de todas as alegações bibliográficas preexistentes, proofread geral ou revisão de originalidade. Não houve commit nem push. Mensagem de commit proposta: `Revisa justificativas da inferência e correções locais com marcação amarela`.
