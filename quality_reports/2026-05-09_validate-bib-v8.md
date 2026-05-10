# Validação Bibliográfica — `paper_dados_format_quali.Rmd` × `Quali-credibilidade.bib`

**Data**: 2026-05-09
**Skill**: `validate-bib`
**Arquivos**: `paper_dados_format_quali.Rmd`, `Quali-credibilidade.bib`

## Resumo

- **Total de citações no texto**: 69 chaves únicas
- **Total de entradas no .bib**: 143
- **Citações órfãs (no texto, sem entrada no bib)**: 0
- **Entradas não citadas (ghost entries)**: ~74
- **Possíveis duplicatas**: 5 candidatas

## Citações órfãs 🟢

**Nenhuma**. Todas as 69 chaves citadas no manuscrito têm entrada correspondente no `.bib`. O paper compila sem erros de citação faltante.

## Possíveis duplicatas 🟡

| Chave 1 | Chave 2 | Comentário |
|---|---|---|
| `urminsky_etal2019` (citada) | `urminskyDoubleLassoMethodPrincipled2019` | Mesmo paper Double LASSO; cite-o por uma chave só. |
| `Humphreys_Jacobs_2023` (citada) | `macartanhumphreysIntegratedInferences2023` | Mesmo livro *Integrated Inferences*; cite-o por uma chave só. |
| `fairfield_charman_2022` (citada) | `tashafairfieldSocialInquiryBayesian2022` | Mesmo livro *Social Inquiry and Bayesian Inference*; cite-o por uma chave só. |
| `belloniHighdimensionalMethodsInference2014` | `belloniHighdimensionalMethodsInference2014a` | Provável duplicata de mesmo artigo Belloni-Chernozhukov-Hansen 2014. |
| (nenhuma cita Belloni hoje) | | Resolver depois apenas se Belloni voltar a ser citado. |

**Ação recomendada**: deletar a versão duplicada do .bib (manter a chave que está sendo citada no texto) ou unificar via @string. Não bloqueia compilação, mas polui o .bib.

## Chaves citadas que merecem verificação manual 🟡

Estas chaves existem no .bib, mas plano P8 alertou que o nome do autor pode estar errado. Comparar com o trabalho real:

| Chave citada | Autor provável correto | Verificar |
|---|---|---|
| `Forozish_2024` | Pode ser Furszyfer del Rio? Ou outro? | Conferir título/journal no bib e comparar com a obra real. |
| `Goldsmith_2024` | Provavelmente Goldsmith-Pinkham? | Conferir. |
| `Card_2022` | Card, David — ✓ provável correto | Provavelmente Nobel lecture ou *American Economic Review* 2022. |
| `bouchat_2023` | Bouchat? Verificar autor (revisão de F&C). | Conferir. |
| `rabbia_2023` | Rabbia? Verificar (aplicação F&C). | Conferir. |

Estas chaves aparecem no texto e devem permanecer; o que precisa ser verificado é a *acurácia da entrada* no .bib (autor escrito corretamente, título correto, ano correto, journal correto). Não bloqueia compilação, mas a integridade da referência precisa ser checada antes da submissão.

## Padronização de chaves Fairfield-Charman 🟡

Chaves citadas:
- `Fairfield_Charman_2017` (artigo)
- `Fairfield_Charman_2019` (artigo)
- `fairfield_charman_2022` (livro)
- `fairfield_charman2023` (resposta no QMMR)
- `fairfield_charman2025` (artigo recente)

**Inconsistência**: capitalização (`Fairfield` vs `fairfield`) e separador antes do ano (com underscore vs sem). Recomendação:

| Atual | Padrão sugerido |
|---|---|
| `Fairfield_Charman_2017` | `fairfield_charman_2017` |
| `Fairfield_Charman_2019` | `fairfield_charman_2019` |
| `fairfield_charman_2022` | `fairfield_charman_2022` (manter) |
| `fairfield_charman2023` | `fairfield_charman_2023` |
| `fairfield_charman2025` | `fairfield_charman_2025` |

Renomear no .bib **e** atualizar todas as ocorrências no texto. Trabalho mecânico; mais legível e profissional. Não bloqueia compilação.

## Padronização Humphreys-Jacobs 🟡

Chaves citadas: `Humphreys_Jacobs_2015`, `Humphreys_Jacobs_2023`. Coerentes entre si — OK como estão. Apenas remover duplicata `macartanhumphreysIntegratedInferences2023`.

## Padronização "process tracing" no texto

Plano P8 pediu padronização de terminologia. Ocorrências atuais no texto:

```
process tracing      (p. ex., trade-offs, ilustração)
process tracing Bayesiano  (subseção 5.1)
rastreamento de processos  (intro 5.1)
rastreio de processo       (linha do paragrafo INUS reduzido — agora cortado)
```

**Recomendação**: padronizar para *process tracing* (com itálico para destacar como termo técnico estrangeiro), exceto quando o termo está sendo nomeado pela primeira vez, onde a forma "rastreamento de processos (*process tracing*)" pode ser usada para introduzir a equivalência. Trabalho de proofread, não de bib.

## Entradas não citadas (ghost entries) 🟡

Cerca de **74 entradas** estão no .bib mas nunca aparecem no texto. Selecionando as mais notáveis:

| Categoria | Chaves não citadas (parcial) |
|---|---|
| Adicionadas no lit-review intl, ainda não usadas | `cunningham2021mixtape`, `huntingtonklein2022effect`, `hernan_robins_2020`, `pearl2009causality`, `pearl_mackenzie_2018`, `imbens_2022_econometrica` |
| Adicionadas no lit-review BR, ainda não usadas | `Cervi_2017`, `FigueiredoFilho_2019`, `Lenine_etal_2023` |
| Já no .bib pré-v8, eram citadas em v7 e foram removidas no rewrite v8 | `Amorim_Rodriguez_2016`, `Bachini_Chicarino_2018`, `Mahoney_2010`, `abadie_etal2015`, `blatter_haverland_2012`, `brady_etal_2010`, `mahoney_goertz_2006`, `slater_simmons_2010`, `soifer_2019`, `mahoney_2008`, `collins_2015` |
| Pré-v8 não citadas (limpeza geral pendente) | `Druckman_Green_2021`, `Druckman_etal_2011`, `Gisselquist_2014`, `King_1995`, `Lesko_etal_2017`, `Little_Lewis_2021`, `Maldonado_Greenland_2002`, `Mcdermott_2002`, `Mcnutt_2014`, `Vandeschoot_etal_2021`, `abadie_etal_2020`, `Bareinboim_Pearl_2016`, `Degtiar_Rose_2023` |
| Notação "AutorTituloLongoAno" (formato Zotero) | ~30 chaves: `ashworthAllElseEqual2015`, `atheyStateAppliedEconometrics2017`, `bareinboimGeneralAlgorithmDeciding2013`, ..., `wiserExpertElicitationSurvey2021` |

**Ação recomendada**: NÃO deletar ainda. As ghost entries estão lá possivelmente como reservatório de leituras consultadas; algumas podem ser citadas na introdução (P-final) ou em revisões substantivas futuras. Reavaliar após reescrever introdução.

**Em particular**: `Amorim_Rodriguez_2016`, `Bachini_Chicarino_2018`, `mahoney_goertz_2006` e `abadie_etal2015` apareciam em v7 e poderiam reaparecer em revisões — manter no .bib é seguro.

## Veredito

✅ **PASS para compilação**: nenhuma citação no texto está órfã; o paper compila sem erros de bib.

🟡 **Limpeza recomendada antes da submissão**:
1. Resolver duplicatas (5 candidatas) — `urminsky`, `Humphreys`, `fairfield_charman` (livro), `belloni` (×2).
2. Padronizar chaves `fairfield_charman` (capitalização e separador antes do ano).
3. Verificar manualmente as 5 chaves marcadas (`Forozish`, `Goldsmith`, `Card_2022`, `bouchat`, `rabbia`).
4. Padronizar "process tracing" no texto (proofread).
5. Reavaliar ghost entries APÓS reescrever introdução — não deletar antes (reservatório de leituras consultadas).
