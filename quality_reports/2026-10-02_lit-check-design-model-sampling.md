# Checagem de literatura: inferência *design-based*, *model-based* e *sampling-based*

**Data:** 2026-10-02
**Escopo:** verificar se as definições de de Chaisemartin & D'Haultfœuille (dC&DH, §2.4) são corretas e padrão; se a classificação pela justificativa (fonte da aleatoriedade) é padrão; conflitos terminológicos previsíveis; ontologia dos resultados potenciais estocásticos; dados de Chen & Pearl (2013); status de publicação de dC&DH.
**Restrições respeitadas:** nenhum arquivo do projeto foi editado além deste relatório; nenhum commit.

## Método e convenções de verificação

- **VERIFICADO (bib + texto):** dados bibliográficos conferidos por DOI (API Crossref) ou URL, e a passagem citada foi lida no texto (PDF publicado, preprint ou cópia local).
- **VERIFICADO (bib); texto NÃO VERIFICADO:** dados bibliográficos conferidos, mas não tive acesso ao texto; o conteúdo atribuído vem de fonte secundária indicada ou fica sem atribuição.
- **NÃO VERIFICADO:** nem os dados bibliográficos foram conferidos por DOI/URL.
- **Paginação.** Várias citações literais vêm de preprints (arXiv, NBER, SSRN) porque os sites das editoras (Cambridge Core, Taylor & Francis, SSRN) bloquearam acesso automatizado (HTTP 403/429). Nesses casos indico "p. X do arXiv vN". **A paginação da versão publicada difere**; antes de citar página no paper, conferir na versão publicada ou citar por seção.
- Textos locais usados: `cópia de BOOK CREDIBLE ANSWERS.pdf` (versão 25/09/2024) e `quali-credibility/DiD_deChaisemartin_dHaultfoeuille.pdf` (versão 27/02/2026, já presente no repo e ignorada pelo `.gitignore`); PDFs no Zotero do autor (Keele 2015, Holland 1986, Kocher & Monteiro 2016, Imbens 2024, Arkhangelsky & Imbens 2023); Brady & Collier 2010 e Mahoney & Goertz 2006 na pasta-pai; Aronow & Miller 2019 em `~/Downloads`; KKV 1994 em `Cursos/stat_basica`.
- **Aviso sobre o Zotero:** o anexo `Zotero/storage/A7LEAJ8P/Imbens e Rubin - 2015 - ...pdf` é um *syllabus* de 7 páginas de C. Frangakis (JHSPH), sem relação com o livro. O livro de Imbens & Rubin não está disponível localmente.

---

## 1. Veredito

### 1.1 Síntese

1. **As três definições de dC&DH estão corretas e são o uso dominante na econometria e na estatística causal recentes.** A classificação se faz pela fonte da aleatoriedade invocada para justificar variâncias, erros-padrão e testes. A tripartição aparece de forma explícita em Abadie, Athey, Imbens & Wooldridge (2023, QJE; p. 4 do arXiv v4), Rambachan & Roth (2026, JASA; pp. 2 e 8 do arXiv v8), Roth, Sant'Anna, Bilinski & Poe (2023, §5) e Arkhangelsky & Imbens (2024, p. 3 e §9 do NBER WP). Imbens (2024, p. 125) agrupa "*model- or sampling-based uncertainty*" contra a *design-based*, o que coincide com a observação de dC&DH de que as duas primeiras "*do not greatly differ*". Um antecedente informal da tripartição é o post de Angrist (2013), citado por Aronow & Miller (2019, p. 95, n. 7).

2. **A classificação pela justificativa, com o mesmo desenho admitindo análises diferentes, está documentada de forma explícita** para:
   - **DiD:** Athey & Imbens (2022) fazem DiD *design-based* (data de adoção aleatória) e situam dC&DH (2017, 2018) e a maior parte da literatura de DiD no campo oposto (p. 1 do arXiv v3); Rambachan & Roth (2026) formulam um "*design-based analog to the parallel trends assumption*"; Roth et al. (2023, §5) descrevem, para o mesmo estimador DiD, a inferência *sampling-based* canônica, as abordagens *model-based* (componentes de erro por cluster) e a *design-based*; Arkhangelsky & Imbens (2024, p. 25 do WP) dizem que a forma da hipótese de tendências paralelas depende de "*whether one takes a model-based or design-based perspective*"; o próprio dC&DH (versão 2026, p. 31) registra que a perspectiva *sampling-based* "*is often used in the DID literature (Abadie, 2005; Callaway and Sant'Anna, 2021)*".
   - **RD:** Cattaneo & Titiunik (2022) separam o arcabouço de continuidade (resultados potenciais como variáveis aleatórias, amostra de uma população) do arcabouço de randomização local (Fisheriano, com resultados potenciais fixos; Neyman; superpopulação). Cattaneo, Frandsen & Titiunik (2015) é a referência da versão Fisheriana.
   - **IV:** Dunning (2010, p. 289): a análise de IV "*can therefore be positioned between the poles of design-based and model-based inference, depending on the application*". Rambachan & Roth (2026) dão tratamento *design-based* a IV.
   - **Shift-share:** Borusyak, Hull & Jaravel (2025, §6) contrastam identificação *design-based* (choques com desenho conhecido) com a "*alternative model-based approach*" de Goldsmith-Pinkham, Sorkin & Swift (2020), que restringe os não observáveis do resultado e "*is coherent when the shocks are considered nonrandom*" (p. 21 da versão *advance access*).
   - **Controle sintético:** Arkhangelsky & Imbens (2024, §9, pp. 55–56 do WP) usam a reunificação alemã para mostrar o experimento mental exigido pela abordagem *design-based* e pela *sampling-based* no mesmo estudo.
   - **Regressão:** AAIW (2020, p. 9 do arXiv v2): "*Which regressors are viewed as causal and which are viewed as attributes depends on the interpretation we wish to give to the regression estimates*".
   - **Experimentos aleatorizados:** Imbens & Rubin (2015) dedicam capítulos distintos a Fisher, Neyman e inferência *model-based* para o mesmo experimento completamente aleatorizado (títulos verificados; texto não verificado); Athey & Imbens (2017) contrastam inferência baseada em aleatorização, inferência *sampling-based* e métodos *model-based*.

   A posição do autor de que "RDD/IV = *design-based*; DiD/controle sintético = *model-based*" é uma classificação equivocada **está correta** à luz dessa literatura.

### 1.2 Onde há divergência (o texto precisa qualificar)

**(a) "Existe um único sentido" é falso como descrição do uso.** Pelo menos três sentidos coexistem, e um parecerista de ciência política conhece pelo menos dois deles:
   1. **Sentido inferencial** (fonte da aleatoriedade para a incerteza): dC&DH, AAIW, Athey & Imbens, Rambachan & Roth, Roth et al., Ding.
   2. **Sentido da amostragem de surveys** (Särndal 1978; Hansen, Madow & Tepping 1983; Särndal, Swensson & Wretman 1992; Little 2004): *design-based* = aleatoriedade do plano amostral sobre população finita; *model-based* = modelo de superpopulação para as variáveis medidas. Nessa partição, o *sampling-based* de dC&DH com amostragem probabilística de população finita é *design-based*, e a superpopulação infinita hipotética é *model-based*. Aronow, Jang & Offer-Westort (2026) adotam essa partição clássica e estendem *design-based* à atribuição de tratamento.
   3. **Sentido de identificação ou de desenho de pesquisa:** *design-based approach/inference/research/identification* em Dunning (2010, 2012), Sekhon (2009), Keele (2015), Kocher & Monteiro (2016), Card (2022) e Borusyak, Hull & Jaravel (2025). Aqui *design-based* designa a estratégia em que a hipótese identificadora recai sobre o processo de atribuição (atribuição "*as-if random*", choques exógenos com desenho conhecido) ou, mais frouxamente, o primado do desenho sobre a modelagem; *model-based* designa a identificação por modelagem do resultado (ajuste por regressão, modelo estrutural, restrição sobre não observáveis como tendências paralelas).

   **Recomendação:** apresentar a tripartição como **o sentido adotado no artigo**, com nota de rodapé que reconheça os outros dois. Há precedentes diretos dessa nota: Keele (2015, p. 324, n. 5) e Aronow, Jang & Offer-Westort (2026, n. 1).

**(b) A fronteira entre *model-based* e *sampling-based* é instável na literatura.** Athey & Imbens (2017, pp. 1–2 do arXiv) descrevem a abordagem *sampling-based* como a que "*considers the treatment assignments to be fixed, while the outcomes are random*", que é a caracterização de dC&DH para *model-based*. Berk & Freedman (2003) tratam a distribuição do erro como "*an imaginary population*". Aronow, Jang & Offer-Westort (2026) reúnem superpopulação infinita e erro estocástico sob *model-based*. dC&DH dizem que as duas perspectivas "*do not greatly differ*". Para o argumento do paper, a distinção que importa é entre aleatoriedade na atribuição (*design-based*) e aleatoriedade nos resultados ou nas unidades (*model-based* e *sampling-based*). Convém dizer isso explicitamente para que um parecerista não leia a tripartição como três ontologias distintas.

**(c) Em DiD, trocar a justificativa muda também a hipótese identificadora e o estimando.** O estimador é o mesmo; o resto muda. Em Athey & Imbens (2022), o ponto de partida é a data de adoção aleatória mais restrições de exclusão; as hipóteses usuais de tendências comuns "*follow from some of our assumptions, but are not the starting point*" (p. 1 do arXiv v3), e o estimador DiD é não viesado para uma média ponderada específica de efeitos causais (resumo). Em Rambachan & Roth (2026), a hipótese é que as probabilidades de tratamento sejam não correlacionadas com as tendências de Y(0) na população finita, e o estimando é um análogo de população finita do ATT (o *expected ATT*). Em dC&DH, tendências paralelas recaem sobre E[Y(0)] condicional ao desenho. A formulação "o mesmo desenho pode ser *design-based* ou *model-based* conforme a justificativa" é correta desde que inclua essa condição. Na linguagem de Borusyak, Hull & Jaravel (2025), tendências paralelas são uma restrição *model-based* sobre não observáveis do resultado, o que é coerente com a escolha *model-based* de dC&DH para a inferência.

**(d) A perspectiva *design-based* também é um experimento mental em quase-experimentos.** dC&DH dizem isso (§2.4, "*Unavoidable thought experiments*"). Rambachan & Roth (2026, p. 9 do arXiv) acrescentam que a interpretação das probabilidades de tratamento "*depends on the particular, stochastic determinants of treatment that the researcher has in mind*". O paper não deve sugerir que *design-based* seja por definição mais credível fora de aleatorização efetiva.

**(e) Atribuição da dicotomia a Mahoney & Goertz (2006).** A Tabela 1 de M&G (p. 229) contrapõe "*Necessary and sufficient causes; mathematical logic*" a "*Correlational causes; probability/statistical theory*". O texto, porém, afirma que a ausência de termo de erro no modelo qualitativo não implica teste sob hipóteses determinísticas e cita procedimentos para causas necessárias/suficientes probabilísticas (p. 234); "*untenable deterministic assumptions*" aparece como objeção atribuída aos pesquisadores estatísticos (p. 233). KKV (1994, pp. 59–60) já tratam "*A Probabilistic World*" e "*A Deterministic World*" como observacionalmente equivalentes e dizem que o argumento "*applies with equal force to qualitative and quantitative researchers*". Um parecerista que conheça esses trechos pode ler a frase "M&G: quanti = probabilístico / quali = determinístico" como espantalho. Sugestão: atribuir a dicotomia a um uso corrente, ecoado pela Tabela 1 de M&G, e usar KKV como aliado.

**(f) Versão de dC&DH.** O rascunho de 25/09/2024 tem título e paginação obsoletos (detalhes em §4.3). As definições citadas são idênticas, palavra por palavra, na versão de 27/02/2026.

**(g) Observação minha (sem fonte específica):** a justificativa *design-based* é logicamente disponível para N pequeno, mas a distribuição de aleatorização fica muito pobre: o p-valor de um teste exato de Fisher é limitado inferiormente pela probabilidade da atribuição observada (com uma unidade tratada e uma de controle sob probabilidades iguais, o menor p-valor é 0,5), e com N = 1 não há grupo de comparação. Se o paper usar as três justificativas para casos qualitativos, convém dizer o que cada uma entrega com N pequeno, o que conversa com a Camada 3 (a tecnologia da CR exige múltiplas observações).

---

## 2. Tabela por fonte

Legenda de alinhamento: **Sim** = mesma classificação por fonte da aleatoriedade; **Parcial** = compatível com ressalvas de rótulo ou de nível (identificação vs. inferência); **Não** = outro sentido do termo.

### 2.1 Tabela

| # | Referência | Definição / uso (local) | Alinhamento com dC&DH | Status |
|---|---|---|---|---|
| 1 | de Chaisemartin, C., e X. D'Haultfœuille. No prelo. *Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions*. Princeton UP. Versões lidas: 25/09/2024 (§2.4, pp. 20–22) e 27/02/2026 (§2.4, pp. 28–32). SSRN 4487202. | *Design-based*: resultados potenciais não estocásticos ou condicionados, aleatoriedade no tratamento. *Model-based*: desenho D não estocástico ou condicionado, aleatoriedade nos resultados potenciais via choques. *Sampling-based*: aleatoriedade na seleção das unidades; D e resultados potenciais aleatórios. | (fonte) | VERIFICADO (bib + texto). PUP: press.princeton.edu/books/paperback/9780691264189; DOI SSRN 10.2139/ssrn.4487202 (Crossref) |
| 2 | Abadie, Athey, Imbens & Wooldridge (2020). "Sampling-Based versus Design-Based Uncertainty in Regression Analysis." *Econometrica* 88(1): 265–296. | Inferência *sampling-based* usa o processo dos indicadores de amostragem; *design-based* usa o processo que determina as atribuições; resultados potenciais "*non-stochastic attributes*" (pp. 2, 4–5 do arXiv v2). Não usa o termo *model-based*; apêndice bayesiano trata os resultados potenciais como aleatórios. | Sim (bipartição design/sampling) | VERIFICADO (bib + texto). DOI 10.3982/ECTA12675; arXiv 1706.01778v2 |
| 3 | Abadie, Athey, Imbens & Wooldridge (2023). "When Should You Adjust Standard Errors for Clustering?" *QJE* 138(1): 1–35. | Tripartição: "*conventional model-based econometric framework*" (componentes de erro, efeitos aleatórios sorteados); amostragem de clusters de população infinita; componente de desenho (atribuição) (p. 4 do arXiv v4). | Sim | VERIFICADO (bib + texto). DOI 10.1093/qje/qjac038; arXiv 1710.02926v4 |
| 4 | Athey & Imbens (2022). "Design-Based Analysis in Difference-In-Differences Settings with Staggered Adoption." *J. Econometrics* 226(1): 62–79. | "*design-based perspective where the stochastic nature and properties of the estimators arises from the stochastic nature of the assignment of the treatments, rather than a sampling-based perspective*" (p. 1 do arXiv v3); resultados potenciais "*deterministic*" (p. 3). Situa dC&DH (2017, 2018) fora da perspectiva *design-based*. | Sim | VERIFICADO (bib + texto do arXiv v3, set/2018). DOI 10.1016/j.jeconom.2020.10.012. Redação da versão publicada não conferida |
| 5 | Athey & Imbens (2017). "The Econometrics of Randomized Experiments." In *Handbook of Economic Field Experiments*, vol. 1, 73–140. Elsevier. | "*the sampling based approach considers the treatment assignments to be fixed, while the outcomes are random*"; "*the randomization-based approach takes the subject's potential outcomes ... as fixed*" (pp. 1–2 do arXiv); prefere métodos de aleatorização a "*model-based*". | Parcial: o rótulo *sampling-based* cobre o que dC&DH chamam de *model-based* | VERIFICADO (bib + texto). DOI 10.1016/bs.hefe.2016.10.003; arXiv 1607.00698 |
| 6 | Rambachan & Roth (2026). "Design-Based Uncertainty for Quasi-Experiments." *JASA* 121(553): 477–491. | Contrapõe superpopulação, "*model-based approach wherein the units are viewed as fixed, but one develops a statistical model for the outcome*" e *design-based* (condiciona em unidades e resultados potenciais; aleatoriedade só na atribuição) (p. 2 do arXiv v8); aplica a DiD e IV com probabilidades de tratamento heterogêneas e desconhecidas. | Sim | VERIFICADO (bib + texto). DOI 10.1080/01621459.2025.2526700; arXiv 2008.00602v8 |
| 7 | Roth, Sant'Anna, Bilinski & Poe (2023). "What's Trending in Difference-in-Differences?" *J. Econometrics* 235(2): 2218–2244. | Para DiD: abordagem canônica *sampling-based* (clusters de superpopulação infinita); "*Model-based approaches*" que modelam choques comuns por cluster; abordagem *design-based* com unidades fixas e atribuição estocástica (§5.1–5.2; pp. 4, 37, 40–42 do arXiv v3). | Sim | VERIFICADO (bib + texto). DOI 10.1016/j.jeconom.2023.03.008; arXiv 2201.01194v3 |
| 8 | Arkhangelsky & Imbens (2024). "Causal Models for Longitudinal and Panel Data: A Survey." *Econometrics Journal* 27(3): C1–C61. | "*design-based approach to inference where the focus is on uncertainty arising from the assignment mechanism, rather than a model-based or sampling-based perspective*" (p. 3 do NBER WP 31942); tendências paralelas dependem da perspectiva (p. 25); controle sintético e reunificação alemã (pp. 55–56). | Sim | VERIFICADO (bib + texto do NBER WP, dez/2023). DOI 10.1093/ectj/utae014 |
| 9 | Imbens (2024). "Causal Inference in the Social Sciences." *Annu. Rev. Stat. Appl.* 11: 123–152. | "*discussions of design-based versus model- or sampling-based uncertainty*" (p. 125). | Sim | VERIFICADO (bib + texto, PDF publicado local). DOI 10.1146/annurev-statistics-033121-114601 |
| 10 | Borusyak, Hull & Jaravel (2022). "Quasi-Experimental Shift-Share Research Designs." *ReStud* 89(1): 181–213. | Identificação via choques "*as-good-as-randomly assigned*"; rejeita sequências assintóticas *sampling-based* convencionais (pp. 1, 6 do arXiv v9). | Parcial: o foco é identificação | VERIFICADO (bib + texto). DOI 10.1093/restud/rdab030; arXiv 1806.01221v9 |
| 11 | Borusyak, Hull & Jaravel (2025). "Design-Based Identification with Formula Instruments: A Review." *Econometrics Journal* 28(1): 83–108. | *Design-based* = identificação que usa o processo de atribuição dos choques e a fórmula; "*alternative model-based approach*" = restrições sobre não observáveis do resultado (Goldsmith-Pinkham et al. 2020; tendências paralelas) (§1, §6; pp. 3, 21–22 da versão *advance access*). | Parcial: mesmo eixo (atribuição vs. resultado), aplicado à identificação | VERIFICADO (bib + texto, versão OA na LSE). DOI 10.1093/ectj/utae003 |
| 12 | Borusyak & Hull (2023). "Nonrandom Exposure to Exogenous Shocks." *Econometrica* 91(6): 2155–2185. | Resumo: choques contrafactuais "*that may as well have been realized*" e ajuste pela exposição esperada. | Parcial (identificação) | VERIFICADO (bib + resumo na página da Econometric Society). Texto completo NÃO VERIFICADO. DOI 10.3982/ECTA19367 |
| 13 | Cattaneo, Frandsen & Titiunik (2015). "Randomization Inference in the Regression Discontinuity Design." *J. Causal Inference* 3(1): 1–24. | RD com randomização local e abordagem Fisheriana (segundo Cattaneo & Titiunik 2022). | Sim | VERIFICADO (bib). Texto NÃO VERIFICADO diretamente; caracterização via #14. DOI 10.1515/jci-2013-0010 |
| 14 | Cattaneo & Titiunik (2022). "Regression Discontinuity Designs." *Annu. Rev. Econ.* 14: 821–851. | Continuidade: "*potential outcomes are taken to be random variables, with the n units ... forming a (random) sample*". Randomização local: Fisheriano ("*potential outcomes are seen as fixed, non-stochastic quantities, and the only randomness ... stems from the random assignment*"), Neyman, superpopulação (pp. 5, 8, 26–27 do arXiv v2). | Sim (rótulos Fisher/Neyman/superpopulação) | VERIFICADO (bib + texto). DOI 10.1146/annurev-economics-051520-021409; arXiv 2108.09400v2 |
| 15 | Imbens & Rubin (2015). *Causal Inference for Statistics, Social, and Biomedical Sciences*. Cambridge UP. | Caps. 5 "Fisher's Exact P-Values..." (pp. 57–82), 6 "Neyman's Repeated Sampling Approach..." (pp. 83–112), 7 "Regression Methods..." (pp. 113–140), 8 "Model-Based Inference for Completely Randomized Experiments" (pp. 141–186). | Provável Sim/Parcial (*model-based* = imputação bayesiana com resultados potenciais aleatórios) | VERIFICADO (bib e títulos/páginas dos capítulos via DOIs de capítulo no Crossref). Texto NÃO VERIFICADO (Cambridge bloqueou; anexo do Zotero é outro documento). DOI 10.1017/CBO9781139025751 |
| 16 | Ding (2024). *A First Course in Causal Inference*. Chapman & Hall/CRC. | Fisher e Neyman "*are both called randomization-based inference or design-based inference ... also called finite-population inference*" (abertura do cap. 8); cap. 9 "Bridging Finite and Super Population Causal Inference" introduz o arcabouço de superpopulação IID. Não nomeia *model-based*. | Sim (bipartição design/superpopulação) | VERIFICADO (bib + texto do arXiv v2). DOI 10.1201/9781003484080; caps. 8 (pp. 109–116) e 9 (pp. 117–122) na ed. CRC, segundo Crossref; pp. 119 e 129–130 no arXiv |
| 17 | Aronow & Miller (2019). *Foundations of Agnostic Statistics*. Cambridge UP. | Trabalham sob amostragem IID de superpopulação, vista como "*codification of uncertainty about generalizability*" (pp. 94–95); *design-based inference* como alternativa que dispensa amostragem IID (p. 238, n. 5); *design-based* também no sentido de surveys (p. 141); citam a tripartição de Angrist (p. 95, n. 7). | Parcial | VERIFICADO (bib + texto, PDF local). DOI 10.1017/9781316831762 |
| 18 | Aronow, Jang & Offer-Westort (2026). "On the Foundations of the Design-Based Approach." *Political Analysis*, online first, 1–16. | "*In model-based inference, randomness comes from largely allegorical notions such as sampling from an infinite super-population or a stochastic error term ... In design-based inference, randomness lies in which units are treated or sampled under the design*" (p. 1 do arXiv v3). N. 1 separa o sentido estatístico do sentido de ciências sociais (Card 2022; Dunning 2010; Kocher & Monteiro 2016). | Parcial: partição de dois polos; *sampling* dividido entre os dois | VERIFICADO (bib + texto do arXiv v3, ago/2026). DOI 10.1017/pan.2026.10045. Redação da versão PA não conferida |
| 19 | Särndal (1978). "Design-Based and Model-Based Inference in Survey Sampling." *Scand. J. Statistics* 5: 27–52. | Origem da dicotomia em surveys (segundo AJO-W 2026, que cita "*various subsets of the finite population*"). | Parcial/Não (sentido de surveys) | NÃO VERIFICADO (sem DOI; dados só por busca) |
| 20 | Hansen, Madow & Tepping (1983). "An Evaluation of Model-Dependent and Probability-Sampling Inferences in Sample Surveys." *JASA* 78(384): 776–793. | Inferência "*model-dependent*" vs. "*probability-sampling*" (título). | Parcial/Não (surveys) | VERIFICADO (bib). Texto NÃO VERIFICADO. DOI 10.1080/01621459.1983.10477018 |
| 21 | Särndal, Swensson & Wretman (1992). *Model Assisted Survey Sampling*. Springer. | Referência canônica de inferência *design-based* em surveys (assim citada por Aronow & Miller 2019, p. 141). | Parcial/Não (surveys) | VERIFICADO (bib). Texto NÃO VERIFICADO. DOI 10.1007/978-1-4612-4378-6 |
| 22 | Little (2004). "To Model or Not to Model? Competing Modes of Inference for Finite Population Sampling." *JASA* 99(466): 546–556. | Resumo: amostragem de populações finitas como "*perhaps the only area of statistics where the primary mode of analysis is based on the randomization distribution, rather than on statistical models for the measured variables*". | Parcial/Não (surveys) | VERIFICADO (bib + resumo do working paper, bepress UMich, nov/2003). Texto completo NÃO VERIFICADO. DOI 10.1198/016214504000000467 |
| 23 | Dunning (2010). "Design-Based Inference: Beyond the Pitfalls of Regression Analysis?" In Brady & Collier, eds., *Rethinking Social Inquiry*, 2ª ed., 273–311. Rowman & Littlefield. | *Design-based*: atribuição "*as-if random*" que imita experimento, análise simples (p. 277); *model-based*: ajuste estatístico de confundidores que produz independência "*always by assumption*" (p. 278); contraste heurístico, "*not absolute*" (p. 279); IV entre os polos "*depending on the application*" (p. 289). | Parcial: mistura identificação e inferência; classifica pela análise | VERIFICADO (bib + texto, PDF local). Sem DOI |
| 24 | Dunning (2012). *Natural Experiments in the Social Sciences: A Design-Based Approach*. Cambridge UP. | Segundo Keele (2015, p. 324), Dunning (2012) restringe *design based* a experimentos naturais e usa "*design-based inference*". | Parcial/Não | VERIFICADO (bib). Texto NÃO VERIFICADO. DOI 10.1017/CBO9781139084444 |
| 25 | Sekhon (2009). "Opiates for the Matches." *Annu. Rev. Polit. Sci.* 12: 487–508. | Três tradições: "*the experimental, the model-based, and the design-based*"; *model-based* = regressão multivariada; *design-based* = experimentos naturais e RD com componente "*as if random*" (pp. 487–488). | Não: classifica por família de desenho | VERIFICADO (bib + texto, PDF do autor). DOI 10.1146/annurev.polisci.11.060606.135444 |
| 26 | Keele (2015). "The Statistics of Causal Inference: A View from Political Methodology." *Political Analysis* 23(3): 313–335. | "*design-based approach*" sem definição consensual; "*a mode of statistical analysis that emphasizes design rather than statistical modeling*" (p. 324); usa "*approach*" para evitar confusão com "*design-based inference*" de surveys (n. 5); "*model-based functional form assumptions*" (p. 315, n. 2); "*what is your mode of inference?*" (Rubin 1991; p. 330). | Não (sentido de desenho), com reconhecimento explícito dos outros sentidos | VERIFICADO (bib + texto, PDF local). DOI 10.1093/pan/mpv007 |
| 27 | Kocher & Monteiro (2016). "Lines of Demarcation." *Perspectives on Politics* 14(4): 952–975. | "*design-based inference*" (DBI) como hierarquia metodológica que toma o RCT como ideal e prescreve experimentos naturais; DBI desconfia de "*matching and model-based statistical inference*" (pp. 952–953). | Não (sentido de desenho) | VERIFICADO (bib + texto, PDF local). DOI 10.1017/S1537592716002863 |
| 28 | Card (2022). "Design-Based Research in Empirical Microeconomics." *AER* 112(6): 1773–1781. | "*Designed-based studies typically use a simplified one-equation model of the outcome of interest—in contrast to model-based studies that specify a data generating process for all factors determining the outcome*" (resumo). | Não: *model-based* = especificação estrutural | VERIFICADO (bib + resumo, página da AEA). DOI 10.1257/aer.112.6.1773 |
| 29 | Holland (1986). "Statistics and Causal Inference." *JASA* 81(396): 945–960. | Variável = "*real-valued function*" definida em cada unidade de U (p. 945); efeito causal médio = valor esperado sobre U (p. 947); o elemento estocástico de Neyman para "*technical errors*" é posto de lado (p. 954). | n/a (resultados potenciais determinísticos; aleatoriedade na seleção de unidades/atribuição) | VERIFICADO (bib + texto, PDF local). DOI 10.1080/01621459.1986.10478354 |
| 30 | Dawid (2000). "Causal Inference without Counterfactuals." *JASA* 95(450): 407–424 (com discussão até p. 448). | Crítica ao caráter metafísico de resultados potenciais fixos ("*fatalism*", segundo fonte secundária). | n/a | VERIFICADO (bib, Crossref). Texto NÃO VERIFICADO. DOI 10.1080/01621459.2000.10474210 |
| 31 | VanderWeele & Robins (2012). "Stochastic Counterfactuals and Stochastic Sufficient Causes." *Statistica Sinica* 22(1): 379–392. | Contrafactuais estocásticos: para cada indivíduo, "*a particular set of interventions gives rise to a distribution of outcomes*" (p. 382); motivação pela física quântica (p. 390). | n/a | VERIFICADO (bib + texto, PDF do periódico). DOI 10.5705/ss.2008.186 |
| 32 | Berk & Freedman (2003). "Statistical Assumptions as Empirical Commitments." In Blomberg & Cohen, eds., *Law, Punishment, and Social Control*, 2ª ed., 235–254. Aldine de Gruyter. | Superpopulação "*imaginary*", "*convenient fictions*"; a distribuição do erro "*is an imaginary population*". | n/a (apoia a leitura de "experimento mental") | VERIFICADO (bib pelo CV de Freedman; texto pelo preprint em stat.berkeley.edu/~census/berk2.pdf). Reimpresso em Freedman (2010), cap. 2, DOI 10.1017/CBO9780511815874.004 |
| 33 | King, Keohane & Verba (1994). *Designing Social Inquiry*. Princeton UP. | "*Perspective 1: A Probabilistic World*" vs. "*Perspective 2: A Deterministic World*", "*observationally equivalent*", "*applies with equal force to qualitative and quantitative researchers*" (pp. 59–60). | n/a (aliado para Q4) | VERIFICADO (bib + texto, PDF local). DOI 10.1515/9781400821211 |
| 34 | Mahoney & Goertz (2006). "A Tale of Two Cultures." *Political Analysis* 14(3): 227–249. | Tabela 1 (p. 229); ver §1.2(e). | n/a | VERIFICADO (bib + texto, PDF local). DOI 10.1093/pan/mpj017 |
| 35 | Angrist, J. D. (2013). "Pop Quiz." *Mostly Harmless Econometrics* (blog), 28 nov. 2013. | Superpopulações / "*model-based approach: some kind of stochastic process generates the data*" / "*randomization inference*". | Sim (forma informal) | VERIFICADO (URL mostlyharmlesseconometrics.com/2013/11/pop-quiz-2/) |
| 36 | Goldsmith-Pinkham, Sorkin & Swift (2020). "Bartik Instruments: What, When, Why, and How." *AER* 110(8): 2586–2624. | Classificado por BHJ (2025) como a alternativa *model-based* para shift-share. | n/a | VERIFICADO (bib). Texto NÃO VERIFICADO. DOI 10.1257/aer.20181047 |
| 37 | Cattaneo, Titiunik & Vazquez-Bare (2017). "Comparing Inference Approaches for RD Designs." *JPAM* 36(3): 643–681. | Estende a randomização local de RD a Neyman e superpopulação (segundo #14). | Sim | VERIFICADO (bib). Texto NÃO VERIFICADO. DOI 10.1002/pam.21985 |

### 2.2 Citações literais mais úteis (com localização)

**dC&DH, versão 27/02/2026, §2.4, p. 28** (idêntica, palavra por palavra, à versão 25/09/2024, §2.4, p. 20):
> "First, one may take a design-based perspective, where potential outcomes are non-stochastic or conditioned upon, and randomness comes from the treatment: one assumes that the treatment, or at least some element of it like its timing, is randomly assigned. Second, one may take a model-based perspective, where the study design D is non-stochastic or conditioned upon, and randomness comes from potential outcomes: one assumes that stochastic shocks affect groups' potential outcomes. [...] Third, one may take a sampling-based perspective, where randomness comes from the random selection of the G groups we observe from a larger population. As different samples lead to different study designs and potential outcomes, both the study design D and the potential outcomes are random under that perspective."

**dC&DH 2026, pp. 28–29** ("Unavoidable thought experiments"):
> "the model-based perspective always relies on a thought experiment, where one imagines that nature draws some shocks affecting potential outcomes. [...] In the natural experiments we consider, researchers do not effectively randomize treatment, so that the design-based perspective also relies on a thought experiment [...] the sampling-based perspective also relies on a thought experiment, where one imagines that the sample is drawn from an hypothetical infinite super-population."

**dC&DH 2026, p. 31** (DiD admite mais de uma perspectiva):
> "This approach is uncontroversial in the design-based approach to inference (Abadie, Athey, Imbens and Wooldridge, 2023). It is also uncontroversial in the sampling-based approach, which is often used in the DID literature (Abadie, 2005; Callaway and Sant'Anna, 2021)."

**dC&DH 2026, p. 32:**
> "Going back and forth between these two conceptual frameworks imposes a cognitive cost on the reader. At the same time, strengthening one's translation skills between those two languages can be useful, because both are used in the methodological papers on DID."

**AAIW 2020, arXiv v2, p. 2:**
> "Sampling-based inference uses information about the process that determines the sampling indicators R1, ..., Rn to assess the variability of estimators across different samples. [...] Design-based inference uses information about the process that determines the assignments X1, ..., Xn to assess the variability of estimators across different samples."

**AAIW 2020, arXiv v2, pp. 4–5:**
> "In our setting, potential outcomes are viewed as non-stochastic attributes for unit i, irrespective of the realized value of Xi. They, as well as the additional observed attributes, Zi [...] remain fixed in repeated sampling thought experiments, whereas Ri and Xi are stochastic"

**AAIW 2023, arXiv v4, p. 4:**
> "In the conventional model-based econometric framework, the researcher takes a stand on the error component structure of a model for the outcome variable. [...] a repeated sampling thought experiment entails that, for each sample, different values of the state random effects are drawn from their distributions. [...] A second, closely related, framework for clustering [...] is motivated by a sampling mechanism that in a first stage selects clusters at random from an infinite population"

**Athey & Imbens 2022, arXiv v3, p. 1:**
> "In contrast to most of the DID literature, e.g., [...] de Chaisemartin and D'Haultfœuille [2017, 2018], we take a design-based perspective where the stochastic nature and properties of the estimators arises from the stochastic nature of the assignment of the treatments, rather than a sampling-based perspective where the uncertainty arises from the random sampling of units from a large population."

**Athey & Imbens 2017, arXiv 1607.00698, pp. 1–2:**
> "In essence, the sampling based approach considers the treatment assignments to be fixed, while the outcomes are random. Inference is based on the idea that the subjects are a random sample from a much larger population. In contrast, the randomization-based approach takes the subject's potential outcomes [...] as fixed, and considers the assignment of subjects to treatments as random."

**Rambachan & Roth, arXiv v8, p. 2:**
> "Traditional approaches to statistical inference that view the sample as being drawn from a super-population may be unnatural in such settings [...]. One possible alternative in such settings is a model-based approach wherein the units are viewed as fixed, but one develops a statistical model for the outcome. [...] The literature on design-based inference addresses these difficulties by conditioning on both the units in the finite population and their potential outcomes, and instead viewing the stochastic assignment of treatment as the sole source of randomness in the data."

**Rambachan & Roth, arXiv v8, pp. 8–9:**
> "Justifying these analyses from a sampling or model-based perspective requires viewing the 50 U.S. states as being drawn from some hypothetical super-population of states or modeling these outcomes as a random process. By contrast, our framework views the 50 U.S. states [...] and their potential outcomes (Yi(0), Yi(1)) as fixed." [...] "the interpretation of the treatment probabilities pi depends on the particular, stochastic determinants of treatment that the researcher has in mind (e.g., court delays or weather); uncertainty is then interpreted relative to that source, holding other determinants of treatment fixed."

**Roth et al. 2023, arXiv v3, p. 40** (a alternativa que corresponde à escolha de dC&DH):
> "while all of the 'model-based' papers above treat νjt as random, an alternative perspective would be to condition on the values of νjt and view the remaining uncertainty as coming only from the sampling of the individual units within clusters [...] the alternative approach would treat the two states as fixed and view any state-level shocks between NJ and PA as a violation of the parallel trends assumption."

**Arkhangelsky & Imbens, NBER WP 31942, p. 25:**
> "The substantive content and the exact form of the assumption depend on [...] whether one takes a model-based or design-based perspective"

**Arkhangelsky & Imbens, NBER WP 31942, p. 56:**
> "A design based approach would require the researcher to contemplate an alternative world where either other countries would have joint with East Germany, or an alternative world where the re-unification with West Germany would have happened in a different year. [...] a sampling-based approach would require the researcher to consider a world with additional countries that could experience a unification event"

**Borusyak, Hull & Jaravel 2025, advance access, p. 21:**
> "An alternative approach to estimating β in the partially linear model (6.1) restricts the unobserved outcome error εi, without restricting the assignment process of conditionally unconfounded shocks. [...] Specification of the shock design plays no role in this strategy. In fact, the strategy is coherent when the shocks are considered nonrandom"
e p. 22: "The cost of this alternative model-based approach is that Assumption 6.1 can be very restrictive."

**Cattaneo & Titiunik 2022, arXiv v2, pp. 26–27:**
> "In the Fisherian framework, the observations in the study are seen as the population of interest, not as a random sample from a larger population. As a consequence, the potential outcomes are seen as fixed, non-stochastic quantities, and the only randomness in the model stems from the random assignment of the treatment. [...] in the so-called super-population framework, the observations in the study are seen as a random sample taken from a larger population. The potential outcomes are therefore independent and identically distributed random variables, not fixed quantities."
Nota: na p. 8 os autores descrevem a abordagem de Neyman como "*potential outcomes are non-random, but sampled from an underlying infinite population*", formulação própria que não coincide com a de Ding (2024), para quem Neyman é inferência de população finita.

**Ding 2024, arXiv v2, abertura do cap. 8:**
> "Both of them are justified by the physical randomization which is ensured by the design of the experiments. Because of this, they are both called randomization-based inference or design-based inference. Because they concern a finite population of units in the experiments, they are also called finite-population inference."

**Aronow, Jang & Offer-Westort, arXiv v3, p. 1 e n. 1:**
> "In classical survey sampling, design-based and model-based inference differ by the assumed source of stochasticity in the data-generating process. In model-based inference, randomness comes from largely allegorical notions such as sampling from an infinite super-population or a stochastic error term in the outcome model. In design-based inference, randomness lies in which units are treated or sampled under the design that assigns probabilities to 'various subsets of the finite population' (Särndal 1978)."
> N. 1: "Here, we are interested in the classical, statistical meaning of design-based inference, although the term often takes on a different meaning in the social sciences. For a reference on the social science interpretation of design-based inference, see: Card (2022), Dunning (2010), and Kocher and Monteiro (2016)."

**Keele 2015, p. 324 e n. 5:**
> "Unfortunately there isn't a widely agreed-upon definition of what it means to use a design-based approach. Dunning (2012) maintains that only natural experiments can be classified as design based. [...] We might define the design-based approach by saying it is a mode of statistical analysis that emphasizes design rather than statistical modeling."
> N. 5: "I exclusively use the term 'design-based approach' to avoid confusion with an older use of the term 'design-based inference' used in the literature on survey sampling."

**Dunning 2010, p. 279 e p. 289:**
> "Overall, as a heuristic distinction, the contrast between design-based and model-based inference is valuable, yet for several reasons this contrast is not absolute. First, strong research designs—including true experiments and natural experiments—also require statistical models."
> "Natural experiments often play a key role in generating instrumental variables. However, whether the ensuing analysis should be viewed as more design-based or more model-based depends on the techniques used to analyze the data. [...] Instrumental-variables analysis can therefore be positioned between the poles of design-based and model-based inference, depending on the application."

**Aronow & Miller 2019, p. 95, n. 7** (citando Angrist 2013):
> "Some would say all data come from 'super-populations,' [...] Others take a model-based approach: some kind of stochastic process generates the data at hand; there is always more where they came from. Finally, an approach known as randomization inference recognizes that even in finite populations, counterfactuals remain hidden, and therefore we always require inference."

---

## 3. Conflitos terminológicos que o paper precisa antecipar

1. **Survey sampling.** Em surveys, *design-based* = aleatoriedade do plano amostral sobre população finita fixa; *model-based* = modelo de superpopulação (Särndal 1978; Hansen, Madow & Tepping 1983; Särndal, Swensson & Wretman 1992; Little 2004; Royall 1970 para a vertente preditiva). O *sampling-based* de dC&DH se divide entre os dois polos dessa tradição: amostragem probabilística de população finita com desenho conhecido é *design-based* em survey; superpopulação infinita hipotética é *model-based*. AAIW (2020) chamam de *sampling-based* a amostragem de população finita. Aronow & Miller (2019, p. 141) e Aronow, Jang & Offer-Westort (2026) usam *design-based* para os dois mecanismos (amostragem e atribuição). **Como antecipar:** nota de rodapé que diga que o artigo segue a terminologia de dC&DH e AAIW (2020), na qual a amostragem de unidades recebe rótulo próprio, e que a estatística de surveys chama de *design-based* a amostragem probabilística com desenho conhecido.

2. **"*Design-based identification*" (Borusyak, Hull & Jaravel 2025; Borusyak & Hull 2023).** O termo se refere a identificação: a hipótese de exogeneidade recai sobre o processo de atribuição dos choques, com desenho conhecido ou estimável. O oposto é a estratégia que "*leverage[s] a model for unobservables*" (resumo de BHJ 2025), como tendências paralelas. O eixo é o mesmo de dC&DH (aleatoriedade/hipótese na atribuição vs. no resultado), aplicado à identificação. Um parecerista pode dizer que "DiD é *model-based* por definição" porque tendências paralelas são uma restrição sobre resultados. **Resposta disponível:** Athey & Imbens (2022) e Rambachan & Roth (2026) dão a DiD fundamentação *design-based*, com hipótese identificadora formulada sobre a atribuição.

3. **Ciência política: *design-based* como "desenho primeiro" ou experimento natural** (Sekhon 2009; Dunning 2010, 2012; Keele 2015; Kocher & Monteiro 2016). Aqui o termo nomeia uma hierarquia ou família de desenhos (RCT > experimento natural/RD > *matching*/regressão). É exatamente a "classificação por aplicação" que o autor rejeita, e é o sentido que a audiência da BPSR provavelmente conhece. Dois pontos ajudam o autor: (i) Keele (2015) admite que o termo não tem definição consensual; (ii) Dunning (2010, p. 289) diz que a classificação depende da análise ("*depends on the techniques used to analyze the data*"), mesmo dentro dessa tradição. Kocher & Monteiro (2016, pp. 952–953) são relevantes para o argumento qualitativo: sustentam que a validade de um experimento natural depende de evidência qualitativa sobre o processo de atribuição.

4. ***Model-based* como especificação estrutural ou modelagem do resultado** (resposta à pergunta 3, último item). Sim, há fontes que usam *model-based* nesse sentido, sem referência à fonte da aleatoriedade:
   - Card (2022, resumo): "*model-based studies that specify a data generating process for all factors determining the outcome*".
   - Dunning (2010, pp. 278–279): ajuste estatístico por regressão para produzir independência "*always by assumption*", com teoria do processo gerador ("*response schedule*").
   - Sekhon (2009, p. 488): "*By far the dominant method [...] is model-based, and the most popular model is multivariate regression*".
   - Keele (2015, p. 315, n. 2): "*Linearity and additivity [...] are model-based functional form assumptions*".
   - BHJ (2025, §6.2): *model-based* = restrições sobre o erro do resultado.
   - Roth et al. (2023, §5.1) e AAIW (2023): *model-based* = modelar a estrutura de componentes de erro; aqui o sentido é inferencial e coincide com dC&DH.
   Não encontrei fonte que use *model-based* como sinônimo literal de "identificação estrutural" no sentido da econometria estrutural (Heckman); Card (2022) é o mais próximo.

5. **Instabilidade entre *model-based* e *sampling-based*** (ver §1.2b). Athey & Imbens (2017) chamam de *sampling-based* a abordagem que fixa a atribuição e trata os resultados como aleatórios.

6. **Variantes internas ao *design-based*.** Fisher (hipótese nula estrita, p-valores exatos) vs. Neyman (efeito médio, variância conservadora); Ding (2024, cap. 8) e Imbens & Rubin (2015, caps. 5–6). Cattaneo & Titiunik (2022) usam "Neyman" com uma formulação que mistura população finita e amostragem. Um parecerista pode perguntar qual versão o paper tem em mente. Com poucas observações, Cattaneo & Titiunik (2022, p. 27) recomendam a versão Fisheriana porque ela "*provide[s] finite-sample valid inference for the sharp null hypothesis of no treatment effect*"; as versões de Neyman e de superpopulação dependem de aproximações de grandes amostras.

7. **Atribuição da dicotomia a Mahoney & Goertz (2006).** Ver §1.2(e).

8. **Citação de dC&DH.** Ver §4.3.

---

## 4. Respostas às perguntas 4–6

### 4.1 Pergunta 4: resultados potenciais estocásticos são indeterminismo ontológico ou artifício de modelagem?

**Resposta curta:** na literatura de inferência causal consultada, a aleatoriedade invocada para inferência é tratada predominantemente como **experimento mental ou artifício de modelagem**. VanderWeele & Robins (2012) são a exceção parcial: motivam contrafactuais estocásticos por uma possível indeterminação física, mas mostram que seus resultados valem nos dois casos.

- **dC&DH (§2.4):** as três perspectivas dependem de experimentos mentais fora de aleatorização ou amostragem efetivas; o *model-based* "*always relies on a thought experiment, where one imagines that nature draws some shocks*". O exemplo dado (choques climáticos sobre produtividade agrícola) é empírico, mas a distribuição conjunta dos choques é "*a judgment call*" do pesquisador. VERIFICADO.
- **AAIW (2020):** resultados potenciais como "*non-stochastic attributes*", fixos em "*repeated sampling thought experiments*" (pp. 4–5 do arXiv). VERIFICADO.
- **Rambachan & Roth (2026, p. 9 do arXiv):** a aleatoriedade da atribuição é relativa à fonte estocástica escolhida pelo pesquisador, "*holding other determinants of treatment fixed*". VERIFICADO.
- **Holland (1986):** resultados potenciais Y_t(u) são valores definidos em cada unidade (p. 945); a aleatoriedade entra pela população de unidades U e pela atribuição (p. 947). Sobre Neyman: "*Neyman's discussion also introduced the notion of a stochastic element that is added to Y to allow for 'technical errors' [...]. If we ignore this problem of measurement error and assume zero 'technical errors' [...]*" (p. 954). Holland trata a componente estocástica como erro de medida a ser posto de lado. VERIFICADO.
- **VanderWeele & Robins (2012):** definem o arcabouço estocástico ("*for each individual, a particular set of interventions gives rise to a distribution of outcomes for that individual*", p. 382). Na conclusão (p. 390): "*Developments during the last century in quantum physics suggest that the world may be inherently probabilistic. [...] Extending the theory of sufficient causes to a stochastic setting may thus constitute an important step towards conceptualizing causation in a manner more consistent with physical realities. We have shown that regardless of whether the underlying causal mechanisms are deterministic or stochastic, the same empirical conditions can be used to test for sufficient cause interactions.*" Acrescentam um argumento de generalidade: "*Because counterfactual outcomes cannot be simultaneously observed, assumptions about them cannot be empirically verified; it is important that assumptions made about counterfactuals be as general as possible.*" A motivação é ontológica; o resultado é agnóstico. VERIFICADO.
- **Dawid (2000):** dados bibliográficos VERIFICADOS (95[450]: 407–424). A crítica de Dawid ao caráter "fatalista" ou metafísico de resultados potenciais determinísticos e não testáveis aparece em fonte secundária (slides em português, hedibert.org/wp-content/uploads/2015/11/causality-meeting6.pdf: "Um modelo metafísico trata os resultados Yi(u) como atributos 'pré-determinados' de u"). **Texto primário NÃO VERIFICADO**; não citar literalmente sem conferir no JASA.
- **Berk & Freedman (2003):** a superpopulação é "*an imaginary population. Such a population has no empirical existence, but is defined in an essentially circular way—as that population from which the sample may be assumed to be randomly drawn. At the risk of the obvious, inferences to imaginary populations are also imaginary*" (n. 2 do preprint); "*These are convenient fictions*" (seção "An Imaginary Population and Imaginary Sampling Mechanism"); "*The error distribution is an imaginary population and the errors εi are treated as if they were a random sample from this imaginary population*" (seção sobre regressão); "*hypothetical super-populations don't generate real statistics*" (n. 12). Paginação do preprint (pp. 1–2, 4, 9, 13) difere da publicada (pp. 235–254). VERIFICADO.
- **Aronow, Jang & Offer-Westort (2026):** aleatoriedade *model-based* vem de "*largely allegorical notions*". VERIFICADO (arXiv v3).
- **Aronow & Miller (2019, p. 95):** "*the superpopulation model can be viewed as a codification of uncertainty about generalizability to broader populations*". VERIFICADO.
- **KKV (1994, pp. 59–60)**, especialmente útil para o argumento do paper: distinguem "*Perspective 1: A Probabilistic World*" ("*Random variation exists in nature [...] and can never be eliminated*") e "*Perspective 2: A Deterministic World*" ("*Random variation is only that portion of the world for which we have no explanation*"); afirmam que "*for most purposes these two perspectives can be regarded as observationally equivalent [...] a choice between the two perspectives depends on faith or belief rather than on empirical verification*" e que "*This argument applies with equal force to qualitative and quantitative researchers*". Na n. 12, a disputa na física quântica "*is unlikely to affect the logic of inference or practice of research in the social sciences*". VERIFICADO.

**Implicação para o paper (com condições):** a escolha entre resultados potenciais fixos (*design-based*) e estocásticos (*model-based*) é uma escolha sobre onde colocar o modelo de probabilidade. Ela não compromete o pesquisador com uma tese sobre determinismo ou indeterminismo do mundo. A frase segura é algo como "tipicamente tratada como experimento mental (dC&DH; AAIW 2020; Berk & Freedman 2003), embora haja motivações ontológicas na literatura (VanderWeele & Robins 2012)". Com KKV (1994, pp. 59–60), o argumento contra a dicotomia "quanti = probabilístico / quali = determinístico" ganha um aliado na própria fonte que a tradição qualitativa costuma tomar como adversária.

### 4.2 Pergunta 5: Chen & Pearl (2013)

**VERIFICADO** (PDF baixado de paecon.net; página de citação sugerida no próprio PDF):
- Autores: Bryant Chen e Judea Pearl (University of California, Los Angeles).
- Título: "Regression and Causation: A Critical Examination of Six Econometrics Textbooks". No PDF o título aparece em caixa baixa ("Regression and causation: a critical examination of six econometrics textbooks").
- Periódico: *real-world economics review*, issue no. 65, 27 September 2013, pp. 2–20.
- URL: http://www.paecon.net/PAEReview/issue65/ChenPearl65.pdf
- Sem DOI (o periódico não atribui DOI). Há registro no SSRN (abstract 2338705), visto em resultado de busca e não aberto (SSRN bloqueou acesso automatizado).
- Citação sugerida pelo próprio periódico: "Bryant Chen and Judea Pearl, 'Regression and causation: a critical examination of six econometrics textbooks', real-world economics review, issue no. 65, 27 September 2013, pp. 2-20, http://www.paecon.net/PAEReview/issue65/ChenPearl65.pdf".

### 4.3 Pergunta 6: status de *Credible Answers*

- **Título atual:** *Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions*. O título do rascunho de 25/09/2024 ("Credible Answers to Hard Questions: Differences-in-Differences for Natural Experiments") está obsoleto.
- **Princeton University Press:** página do livro (press.princeton.edu/books/paperback/9780691264189/causal-inference-with-differences-in-differences) informa data de publicação **8 de dezembro de 2026**, *copyright* 2027, 360 páginas; ISBN capa dura 9780691264172, brochura 9780691264189, e-book PDF 9780691264196, e-book EPUB 9780691299235. **Em 2026-10-02 o livro ainda não saiu.** VERIFICADO.
- **SSRN:** "Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions", abstract 4487202, DOI 10.2139/ssrn.4487202 (registro Crossref criado em 25/06/2023). O site de C. de Chaisemartin lista o livro como "*Forthcoming, Princeton University Press*" e linka uma "*Preliminary version*" (SSRN). Qual versão o SSRN hospeda hoje: NÃO VERIFICADO (SSRN retornou 403).
- **arXiv:** nenhuma versão do livro encontrada (busca na API do arXiv por autor e título; há artigos dos autores, nenhum é o livro).
- **Versão mais recente disponível localmente:** `quali-credibility/DiD_deChaisemartin_dHaultfoeuille.pdf`, datada de 27/02/2026, com o título da PUP. Diferenças relevantes para o paper em relação ao rascunho de 2024:
  - §2.4 renomeada "Discussion of the book's perspective on statistical inference∗" (seção marcada como não central), pp. 28–32 (antes "Framework for statistical inference", pp. 20–22).
  - As três definições e o parágrafo "Unavoidable thought experiments" são idênticos.
  - "*in all the empirical applications revisited in this book, these testable implications are rejected*" (2026) substitui "*in most of the natural experiments we have revisited so far for this book, these testable implications are violated*" (2024).
  - Recomendação de *clustering*: em 2024, cluster no nível mais desagregado que forme painel; em 2026, cluster "*either at the level at which the treatment is assigned, following Bertrand et al. (2004), or at the most disaggregated level at which one can construct a panel dataset*".
  - A perspectiva *sampling-based* é usada no cap. 7 (2026; antes cap. 6) e ganhou a "Assumption IID"; "Assumption 5" virou "Assumption IND".
- **Forma recomendada de citação (Chicago author-date):**
  de Chaisemartin, Clément, e Xavier D'Haultfœuille. No prelo. *Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions*. Princeton, NJ: Princeton University Press. Versão preliminar de 27 de fevereiro de 2026, SSRN, https://doi.org/10.2139/ssrn.4487202.
  Citar por seção ("§2.4") e evitar número de página até a publicação. Depois de dezembro de 2026, trocar pela edição publicada e conferir se o ano do *copyright* (2027) será o ano de referência.

---

## 5. Referências recomendadas e BibTeX

### 5.1 O que citar para cada afirmação do paper

| Afirmação no paper | Citar |
|---|---|
| As três justificativas classificadas pela fonte da aleatoriedade | de Chaisemartin & D'Haultfœuille (no prelo, §2.4); Abadie et al. (2020); Abadie et al. (2023); Rambachan & Roth (2026) |
| *Model-based* e *sampling-based* próximos | de Chaisemartin & D'Haultfœuille (no prelo, §2.4); Imbens (2024, p. 125) |
| O mesmo desenho admite justificativas diferentes: DiD | Athey & Imbens (2022); Rambachan & Roth (2026); Roth et al. (2023, §5); Arkhangelsky & Imbens (2024) |
| Idem: RD | Cattaneo, Frandsen & Titiunik (2015); Cattaneo & Titiunik (2022) |
| Idem: IV e shift-share | Dunning (2010, p. 289); Borusyak, Hull & Jaravel (2025); Rambachan & Roth (2026) |
| Idem: controle sintético | Arkhangelsky & Imbens (2024, §9) |
| Experimentos: Fisher, Neyman, *model-based* | Imbens & Rubin (2015, caps. 5, 6 e 8; conferir texto antes de citar página); Ding (2024, caps. 8–9) |
| Outros sentidos do termo (nota de rodapé) | Särndal, Swensson & Wretman (1992) ou Little (2004) para surveys; Keele (2015, p. 324 e n. 5); Aronow, Jang & Offer-Westort (2026, n. 1); Dunning (2012); Card (2022); Borusyak, Hull & Jaravel (2025) |
| Aleatoriedade como experimento mental | de Chaisemartin & D'Haultfœuille (no prelo, §2.4); Berk & Freedman (2003); Aronow & Miller (2019, p. 95) |
| Probabilístico vs. determinístico observacionalmente equivalentes, para quali e quanti | King, Keohane & Verba (1994, pp. 59–60) |
| Motivação ontológica para contrafactuais estocásticos | VanderWeele & Robins (2012) |

### 5.2 Entradas já existentes em `Quali-credibilidade.bib` (não duplicar)

Conferi as chaves por leitura do `.bib`, sem editá-lo:
- `abadie_etal_2020` (AAIW 2020; já tem DOI)
- `holland_1986` (já tem DOI)
- `Keele_2015a` (sem DOI; sugerir `doi = {10.1093/pan/mpv007}`)
- `King_etal_1994` (já tem DOI)
- `kocherLinesDemarcationCausation2016` (sem DOI; sugerir `doi = {10.1017/S1537592716002863}`)
- `mahoney_goertz_2006` (já tem DOI)
- `Card_2022` (sem DOI; sugerir `doi = {10.1257/aer.112.6.1773}`)
- `Imbens_Rubin_2015` (sem DOI; sugerir `doi = {10.1017/CBO9781139025751}`)
- `brady_collier_2010` (livro editado onde está Dunning 2010)
- `Athey_Imbens_2017` é o artigo do *JEP* ("The State of Applied Econometrics"), diferente do capítulo de Handbook abaixo.

### 5.3 Entradas novas (apenas fontes com dados bibliográficos VERIFICADOS)

Os dados vêm do Crossref (DOI) ou da URL indicada. Nos itens marcados `% texto não verificado`, conferir o conteúdo antes de citar para afirmação específica.

```bibtex
@book{deChaisemartin_DHaultfoeuille_forthcoming,
  author    = {de Chaisemartin, Cl{\'e}ment and D'Haultf{\oe}uille, Xavier},
  title     = {Causal Inference with Differences-in-Differences: Credible Answers to Hard Questions},
  publisher = {Princeton University Press},
  address   = {Princeton, NJ},
  year      = {forthcoming},
  note      = {Preliminary version, February 27, 2026. SSRN, \url{https://doi.org/10.2139/ssrn.4487202}}
}
% Conferir como o CSL Chicago renderiza year = {forthcoming}; alternativa: year = {2026} + pubstate = {forthcoming}.

@article{abadie_etal_2023,
  author  = {Abadie, Alberto and Athey, Susan and Imbens, Guido W. and Wooldridge, Jeffrey M.},
  title   = {When Should You Adjust Standard Errors for Clustering?},
  journal = {The Quarterly Journal of Economics},
  year    = {2023},
  volume  = {138},
  number  = {1},
  pages   = {1--35},
  doi     = {10.1093/qje/qjac038}
}

@article{athey_imbens_2022,
  author  = {Athey, Susan and Imbens, Guido W.},
  title   = {Design-Based Analysis in Difference-In-Differences Settings with Staggered Adoption},
  journal = {Journal of Econometrics},
  year    = {2022},
  volume  = {226},
  number  = {1},
  pages   = {62--79},
  doi     = {10.1016/j.jeconom.2020.10.012}
}

@incollection{athey_imbens_2017_handbook,
  author    = {Athey, Susan and Imbens, Guido W.},
  title     = {The Econometrics of Randomized Experiments},
  booktitle = {Handbook of Economic Field Experiments},
  volume    = {1},
  pages     = {73--140},
  publisher = {Elsevier},
  year      = {2017},
  doi       = {10.1016/bs.hefe.2016.10.003}
}
% Organizadores do Handbook nao conferidos nesta sessao; completar antes de usar.

@article{rambachan_roth_2026,
  author  = {Rambachan, Ashesh and Roth, Jonathan},
  title   = {Design-Based Uncertainty for Quasi-Experiments},
  journal = {Journal of the American Statistical Association},
  year    = {2026},
  volume  = {121},
  number  = {553},
  pages   = {477--491},
  doi     = {10.1080/01621459.2025.2526700}
}

@article{roth_etal_2023,
  author  = {Roth, Jonathan and Sant'Anna, Pedro H. C. and Bilinski, Alyssa and Poe, John},
  title   = {What's Trending in Difference-in-Differences? {A} Synthesis of the Recent Econometrics Literature},
  journal = {Journal of Econometrics},
  year    = {2023},
  volume  = {235},
  number  = {2},
  pages   = {2218--2244},
  doi     = {10.1016/j.jeconom.2023.03.008}
}

@article{arkhangelsky_imbens_2024,
  author  = {Arkhangelsky, Dmitry and Imbens, Guido W.},
  title   = {Causal Models for Longitudinal and Panel Data: A Survey},
  journal = {The Econometrics Journal},
  year    = {2024},
  volume  = {27},
  number  = {3},
  pages   = {C1--C61},
  doi     = {10.1093/ectj/utae014}
}

@article{imbens_2024_arsa,
  author  = {Imbens, Guido W.},
  title   = {Causal Inference in the Social Sciences},
  journal = {Annual Review of Statistics and Its Application},
  year    = {2024},
  volume  = {11},
  pages   = {123--152},
  doi     = {10.1146/annurev-statistics-033121-114601}
}

@article{borusyak_hull_jaravel_2022,
  author  = {Borusyak, Kirill and Hull, Peter and Jaravel, Xavier},
  title   = {Quasi-Experimental Shift-Share Research Designs},
  journal = {The Review of Economic Studies},
  year    = {2022},
  volume  = {89},
  number  = {1},
  pages   = {181--213},
  doi     = {10.1093/restud/rdab030}
}

@article{borusyak_hull_jaravel_2025,
  author  = {Borusyak, Kirill and Hull, Peter and Jaravel, Xavier},
  title   = {Design-Based Identification with Formula Instruments: A Review},
  journal = {The Econometrics Journal},
  year    = {2025},
  volume  = {28},
  number  = {1},
  pages   = {83--108},
  doi     = {10.1093/ectj/utae003}
}

@article{borusyak_hull_2023,
  author  = {Borusyak, Kirill and Hull, Peter},
  title   = {Nonrandom Exposure to Exogenous Shocks},
  journal = {Econometrica},
  year    = {2023},
  volume  = {91},
  number  = {6},
  pages   = {2155--2185},
  doi     = {10.3982/ECTA19367}
}
% texto completo nao verificado (apenas resumo)

@article{goldsmithpinkham_etal_2020,
  author  = {Goldsmith-Pinkham, Paul and Sorkin, Isaac and Swift, Henry},
  title   = {Bartik Instruments: What, When, Why, and How},
  journal = {American Economic Review},
  year    = {2020},
  volume  = {110},
  number  = {8},
  pages   = {2586--2624},
  doi     = {10.1257/aer.20181047}
}
% texto nao verificado

@article{cattaneo_frandsen_titiunik_2015,
  author  = {Cattaneo, Matias D. and Frandsen, Brigham R. and Titiunik, Roc{\'i}o},
  title   = {Randomization Inference in the Regression Discontinuity Design: An Application to Party Advantages in the {U.S.} {Senate}},
  journal = {Journal of Causal Inference},
  year    = {2015},
  volume  = {3},
  number  = {1},
  pages   = {1--24},
  doi     = {10.1515/jci-2013-0010}
}
% texto nao lido diretamente; caracterizacao via Cattaneo & Titiunik (2022)

@article{cattaneo_titiunik_2022,
  author  = {Cattaneo, Matias D. and Titiunik, Roc{\'i}o},
  title   = {Regression Discontinuity Designs},
  journal = {Annual Review of Economics},
  year    = {2022},
  volume  = {14},
  pages   = {821--851},
  doi     = {10.1146/annurev-economics-051520-021409}
}

@article{cattaneo_titiunik_vazquezbare_2017,
  author  = {Cattaneo, Matias D. and Titiunik, Roc{\'i}o and Vazquez-Bare, Gonzalo},
  title   = {Comparing Inference Approaches for {RD} Designs: A Reexamination of the Effect of {Head Start} on Child Mortality},
  journal = {Journal of Policy Analysis and Management},
  year    = {2017},
  volume  = {36},
  number  = {3},
  pages   = {643--681},
  doi     = {10.1002/pam.21985}
}
% texto nao verificado

@book{ding_2024,
  author    = {Ding, Peng},
  title     = {A First Course in Causal Inference},
  publisher = {Chapman and Hall/CRC},
  address   = {Boca Raton, FL},
  year      = {2024},
  doi       = {10.1201/9781003484080}
}
% "address" segue a sede usual da CRC; nao conferido no registro Crossref.

@book{aronow_miller_2019,
  author    = {Aronow, Peter M. and Miller, Benjamin T.},
  title     = {Foundations of Agnostic Statistics},
  publisher = {Cambridge University Press},
  address   = {Cambridge},
  year      = {2019},
  doi       = {10.1017/9781316831762}
}

@article{aronow_jang_offerwestort_2026,
  author  = {Aronow, P. M. and Jang, Austin and Offer-Westort, Molly},
  title   = {On the Foundations of the Design-Based Approach},
  journal = {Political Analysis},
  year    = {2026},
  pages   = {1--16},
  doi     = {10.1017/pan.2026.10045},
  note    = {Online first}
}

@article{little_2004,
  author  = {Little, Roderick J.},
  title   = {To Model or Not to Model? {C}ompeting Modes of Inference for Finite Population Sampling},
  journal = {Journal of the American Statistical Association},
  year    = {2004},
  volume  = {99},
  number  = {466},
  pages   = {546--556},
  doi     = {10.1198/016214504000000467}
}
% texto completo nao verificado (apenas resumo)

@article{hansen_madow_tepping_1983,
  author  = {Hansen, Morris H. and Madow, William G. and Tepping, Benjamin J.},
  title   = {An Evaluation of Model-Dependent and Probability-Sampling Inferences in Sample Surveys},
  journal = {Journal of the American Statistical Association},
  year    = {1983},
  volume  = {78},
  number  = {384},
  pages   = {776--793},
  doi     = {10.1080/01621459.1983.10477018}
}
% texto nao verificado

@book{sarndal_swensson_wretman_1992,
  author    = {S{\"a}rndal, Carl-Erik and Swensson, Bengt and Wretman, Jan},
  title     = {Model Assisted Survey Sampling},
  series    = {Springer Series in Statistics},
  publisher = {Springer},
  address   = {New York},
  year      = {1992},
  doi       = {10.1007/978-1-4612-4378-6}
}
% texto nao verificado

@incollection{dunning_2010,
  author    = {Dunning, Thad},
  title     = {Design-Based Inference: Beyond the Pitfalls of Regression Analysis?},
  booktitle = {Rethinking Social Inquiry: Diverse Tools, Shared Standards},
  editor    = {Brady, Henry E. and Collier, David},
  edition   = {2},
  publisher = {Rowman \& Littlefield},
  address   = {Lanham, MD},
  year      = {2010},
  pages     = {273--311}
}

@book{dunning_2012,
  author    = {Dunning, Thad},
  title     = {Natural Experiments in the Social Sciences: A Design-Based Approach},
  publisher = {Cambridge University Press},
  address   = {Cambridge},
  year      = {2012},
  doi       = {10.1017/CBO9781139084444}
}
% texto nao verificado

@article{sekhon_2009,
  author  = {Sekhon, Jasjeet S.},
  title   = {Opiates for the Matches: Matching Methods for Causal Inference},
  journal = {Annual Review of Political Science},
  year    = {2009},
  volume  = {12},
  pages   = {487--508},
  doi     = {10.1146/annurev.polisci.11.060606.135444}
}

@article{dawid_2000,
  author  = {Dawid, A. P.},
  title   = {Causal Inference without Counterfactuals},
  journal = {Journal of the American Statistical Association},
  year    = {2000},
  volume  = {95},
  number  = {450},
  pages   = {407--424},
  doi     = {10.1080/01621459.2000.10474210}
}
% texto nao verificado

@article{vanderweele_robins_2012,
  author  = {VanderWeele, Tyler J. and Robins, James M.},
  title   = {Stochastic Counterfactuals and Stochastic Sufficient Causes},
  journal = {Statistica Sinica},
  year    = {2012},
  volume  = {22},
  number  = {1},
  pages   = {379--392},
  doi     = {10.5705/ss.2008.186}
}

@incollection{berk_freedman_2003,
  author    = {Berk, Richard A. and Freedman, David A.},
  title     = {Statistical Assumptions as Empirical Commitments},
  booktitle = {Law, Punishment, and Social Control: Essays in Honor of {Sheldon} {Messinger}},
  editor    = {Blomberg, T. G. and Cohen, S.},
  edition   = {2},
  publisher = {Aldine de Gruyter},
  address   = {New York},
  year      = {2003},
  pages     = {235--254}
}
% Dados do CV de D. Freedman (stat.berkeley.edu/~census/cv.pdf). Reimpresso em Freedman (2010),
% Statistical Models and Causal Inference, cap. 2, pp. 23-44, doi 10.1017/CBO9780511815874.004.

@article{chen_pearl_2013,
  author  = {Chen, Bryant and Pearl, Judea},
  title   = {Regression and Causation: A Critical Examination of Six Econometrics Textbooks},
  journal = {Real-World Economics Review},
  year    = {2013},
  number  = {65},
  pages   = {2--20},
  url     = {http://www.paecon.net/PAEReview/issue65/ChenPearl65.pdf}
}

@misc{angrist_2013_popquiz,
  author       = {Angrist, Joshua D.},
  title        = {Pop Quiz},
  howpublished = {Mostly Harmless Econometrics (blog), November 28},
  year         = {2013},
  url          = {http://www.mostlyharmlesseconometrics.com/2013/11/pop-quiz-2/}
}
```

### 5.4 Fontes sem BibTeX (NÃO VERIFICADAS ou fora do escopo)

- Särndal, C.-E. (1978). "Design-Based and Model-Based Inference in Survey Sampling." *Scandinavian Journal of Statistics* 5: 27–52. Sem DOI; volume e páginas vistos só em resultado de busca. Conferir no JSTOR antes de usar.
- Royall (1970), *Biometrika* 57(2): 377–387, doi 10.1093/biomet/57.2.377: bibliografia verificada no Crossref, texto não lido; cabe só se o paper detalhar a vertente preditiva *model-based* de surveys.
- Abadie (2021), "Using Synthetic Controls", *JEL* 59(2): 391–425, doi 10.1257/jel.20191450: bibliografia verificada, texto não lido. Não usei para afirmar nada sobre inferência em controle sintético.
