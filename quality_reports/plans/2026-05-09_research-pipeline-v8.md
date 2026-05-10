# Plano: Research Pipeline — paper v8 pós-paralelo-estrutural

**Status**: PAUSADO — reescrever estudo de caso antes do Devil's Advocate Round 2
**Data**: 2026-05-09

**Atualização 2026-05-09, pós-entrevista Limongi**: o Round 1 do Devil's Advocate identificou problemas remanescentes no estudo de caso do impeachment. Antes de rodar novo Devil's Advocate, a seção deve ser reescrita com base no mapa argumentativo da entrevista de Fernando Limongi em `quality_reports/limongi_argument_map.md`.

## Objetivo

Rodar pipeline de qualidade no `paper_dados_format_quali.Rmd` após a reescrita do estudo de caso do impeachment. O objetivo original era stress-testar o argumento depois da inserção do paralelo estrutural CR/IBE e da agenda metodológica nas Considerações Finais; a prioridade agora é não enviar ao Devil's Advocate uma seção empírica ainda esquemática.

## Modo

- **Stage 1** (code review): SKIP. R em `scripts/target_functions.R` e `_targets.R` é ilustrativo per CLAUDE.md ("simulações são ilustração didática, NÃO análise empírica do paper").
- **Stage 2** (Devil's Advocate): RUN. Loop até score ≥ 80 (max 5 rounds).
- **Stage 3** (Proofread): RUN. Fase 2 com pausa para aprovação do usuário.
- **JDI mode**: NO. Pausa para aprovação no Proofread.

## Arquivos

- Manuscrito: `paper_dados_format_quali.Rmd`
- Bibliografia: `Quali-credibilidade.bib`
- PDF compilado (commit `eea9a78` + recompile pós-edits): `paper_dados_format_quali.pdf`
- Base para reescrita do estudo de caso: `quality_reports/limongi_argument_map.md`
- Sínteses auxiliares: `quality_reports/limongi_chunks/part1_synthesis.md`, `quality_reports/limongi_chunks/part2_synthesis.md`

## Sequência

- [x] Stage 2 Round 1: Devil's Advocate reviewer → salva `quality_reports/stage2_devils_advocate_round1.md`
- [x] Reescrever estudo de caso do impeachment com base em Limongi antes do Round 2
- [x] Atualizar exemplo Bayesiano e, se necessário, `scripts/impeachment_bayes_example.R`
- [x] Recompilar `paper_dados_format_quali.pdf`
- [ ] Stage 2 Round 2: Devil's Advocate reviewer → salva `quality_reports/stage2_devils_advocate_round2.md`
- [ ] Se score < 80: Implementador → corrige → novo round (loop até score ≥ 80 ou 5 rounds)
- [ ] Stage 3 Fase 1: Proofread reviewer → salva `quality_reports/stage3_proofread_round1.md`
- [ ] Stage 3 Fase 2: Apresentar relatório, esperar aprovação
- [ ] Stage 3 Fase 3: Implementador aplica correções aprovadas
- [ ] Re-verify proofread (max 2 rounds adicionais)
- [ ] Relatório final consolidado: `quality_reports/pipeline_report_2026-05-09.md`

## Verificação

- [ ] Score Stage 2 ≥ 80
- [ ] Score Stage 3 ≥ 90
- [ ] Manuscrito ainda compila com xelatex pós-correções

## Decisões já tomadas

- Stage 1 SKIP é decisão deste plano, não revisitar dentro do loop
- Devil's Advocate deve respeitar a tese tripla da v8 (premissa + tradução BR + contribuição operacional via IBE) — não atacar a estrutura como se fosse "KKV errou"
- Considerar contexto: paper já passou por Edmans-review v8 (`2026-05-09_edmans-review-v8.md`); este pipeline é incremental, não primeira passagem
- Não rodar Round 2 antes de substituir a ilustração esquemática do impeachment por uma versão informada por Limongi.
- O novo framing conceitual não é "CR indisponível em pequeno-n"; é "design-based pode ser tentado, mas frequentemente retorna incerteza grande; evidência qualitativa e restrições estruturais discriminam rivais via IBE+Bayes".
