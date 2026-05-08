# Lit-review: o split identificação vs inferência está consolidado na fronteira metodológica internacional?

**Data**: 2026-05-08
**Pergunta-mãe**: o claim do paper v8 — "identificação causal e inferência estatística são problemas distintos; já consolidado na fronteira via revolução da credibilidade (quanti) e Fairfield-Charman / Humphreys-Jacobs / Slater-Ziblatt / IBE (quali)" — está calibrado, ou "consolidado" é forte demais?
**Veredito curto**: defensável **com calibração** — na quanti está mais que consolidado (é lugar-comum de manual desde ~2010); na quali está **emergente e crescente**, mas ainda não unânime.

## 1. Visão geral do campo

Há uma **assimetria clara** entre quanti e quali. No lado quantitativo (econometria aplicada e ciência política causal-empirista), o split entre identificação e inferência/estimação já é doutrina de manual — o **princípio organizador** da revolução da credibilidade. Mostly Harmless (Angrist & Pischke 2009), The Effect (Huntington-Klein 2022), The Mixtape (Cunningham 2021), What If (Hernán & Robins 2020) e Counterfactuals (Morgan & Winship 2015) seguem todos a mesma arquitetura: primeiro o problema causal/identificação, depois o problema de estimação/inferência. A formulação canônica recente — Lundberg, Johnson & Stewart 2021 ASR — torna a separação **tripartite**: estimando teórico → estimando empírico (sob suposições de identificação) → estimação.

No lado qualitativo, o split é mais novo e ainda em consolidação. F&C (2017, 2022), H&J (2015, 2023), Bennett & Checkel (2015) e Beach & Pedersen (2019) operam dentro do split. Mas há tradição co-existente, especialmente pós-KKV, que trata identificação e inferência como uma coisa só. Slater & Ziblatt 2013 revaloriza controlled comparison como ferramenta de identificação, mas com linguagem Campbell-Stanley (validade), não a do split formal. Brady & Collier (2010) e Mahoney (2010, 2021) reconhecem o problema mas não cristalizam o split como axioma.

## 2. Mapa por subliteratura

### 2.1 Quanti — revolução da credibilidade (status: **consolidado, lugar-comum**)

- **Angrist & Pischke 2009**, cap. 1: a 3ª das quatro FAQs é "what is your identification strategy?" — anterior à pergunta de inferência. *No .bib como `Angrist_Pischke_2009`.*
- **Angrist & Pischke 2010 JEP** — manifesto da credibilidade. *No .bib.*
- **Cunningham 2021 (Mixtape)** — "the space between theory and estimation". **Adicionar ao .bib.**
- **Huntington-Klein 2022 (The Effect)** — Parte 1 identificação via DAGs, Parte 2 estimação. **Adicionar.**
- **Hernán & Robins 2020 (What If)** — Parte 1 sem modelos / 2 com modelos / 3 longitudinal complexa; identificação é Parte 1, estimação Parte 2-3. **Adicionar.**
- **Lundberg, Johnson & Stewart 2021 ASR** — pipeline tripartite estimando-teórico → estimando-empírico → estimador. **Citação-âncora.** *No .bib como `Lundberg_etal_2021`.*
- **Athey & Imbens 2017 JEP** — survey state-of-the-field. *No .bib como `atheyStateAppliedEconometrics2017`.*
- **Imbens 2020 JEL (PO vs DAG)** — pressupõe o split. **Adicionar.**
- **Imbens 2022 Econometrica (Nobel lecture)** — atenção: foi em **Econometrica**, não JEL. **Adicionar.**
- **Card 2022 (Nobel lecture)** — distingue model-based de design-based. *No .bib como `Card_2022`.*
- **Goldsmith-Pinkham 2024** — documenta o split institucionalmente. *No .bib como `Goldsmith_2024`.*
- **Samii 2016 J. Politics** — ponte para a CP. *No .bib como `Samii_2016`.* **Paper-ponte para BPSR.**

**Status: (a) Consolidado/lugar-comum.**

### 2.2 DAG / SCM (status: **consolidado dentro do paradigma**)

- **Pearl 2009 (Causality)** — caps. 3-4 são sobre identificação (do-calculus, back/front door). **Verificar/adicionar.**
- **Pearl & Mackenzie 2018 (Book of Why)** — divulgação. **Adicionar opcional.**
- **Pearl & Bareinboim 2014/2022** — identificação fora da amostra. *No .bib.*
- **Hernán & Robins 2020** — ponte PO-DAG.
- **Morgan & Winship 2015** — divisão explícita identificação/estimação. **Adicionar.**

**Status: split é doutrina; o que se debate é PO vs DAG, não a separação.**

### 2.3 Quali metodológico (status: **emergente, presente e crescente, NÃO unânime**)

- **Fairfield & Charman 2017 PA** — paper-âncora. Distingue conjunto de hipóteses rivais (problema epistêmico-de-desenho) da atualização Bayesiana via likelihood (cálculo inferencial). *No .bib.*
- **F&C 2019 PA** — defesa de hipóteses rivais. *No .bib.*
- **F&C 2022 (Social Inquiry and Bayesian Inference, CUP)** — livro-manifesto. *No .bib.*
- **Humphreys & Jacobs 2015 APSR (BIQQ)** — separa priors de assignment propensities/informativeness (identificação) de posterior updating (inferência). *No .bib.*
- **H&J 2023 (Integrated Inferences, CUP)** — DAG + Bayes; **ponto onde quali alcança formalmente a arquitetura quanti**. *No .bib.*
- **Bennett & Checkel 2015** — best practices PT; split aparece no cap. 1 e appendix Bayesiano. *No .bib.*
- **Beach & Pedersen 2019** — case-centric vs theory-centric PT. **Verificar entrada no .bib.**
- **Slater & Ziblatt 2013 CPS** — controlled comparison gera validade interna+externa; linguagem Campbell-Stanley, não split formal. *No .bib.* **Citação de apoio, não fonte canônica do split.**
- **Mahoney 2010, 2021** — tipologias causais (necessary/sufficient, INUS/SUIN), não split formal. *No .bib.*
- **Brady & Collier 2010** — anti-KKV; reconhece dois tipos de problema (data-set obs vs CPOs) mas arquitetura distinta. *No .bib.*
- **Goertz & Mahoney 2012** — duas culturas, mas foco em *como* cada cultura trata causalidade.

**Status: (b) Presente e crescente, mas não unânime.**

### 2.4 IBE (status: **conexão presente mas não codificada como standard**)

- **Lipton 2004** — fonte filosófica; IBE como Bayes iluminado por critérios explanatórios. **Adicionar se citado.**
- **F&C 2022, 2025** — invocam IBE explicitamente. *No .bib.*
- **H&J 2023** — arquitetura cognata (sem usar o label).
- **Beach & Pedersen 2019** — versão pragmática.

**Status: racional filosófico coerente com F&C/H&J, mas não vocabulário standard.**

### 2.5 Surveys / state-of-the-field

- **Lundberg-Johnson-Stewart 2021** — exemplar.
- **Druckman & Green 2021** — survey experimental. *No .bib.*
- **Findley, Kikuta & Denly 2021 ARPS** — external validity. *No .bib.*
- **Keele 2015 PA**. *No .bib.*
- **Imbens 2020/2022**.
- **Goldsmith-Pinkham 2024**.

## 3. Trechos-chave

- **Angrist & Pischke 2009, cap. 1**: identification strategy = "the manner in which a researcher uses observational data to approximate a real experiment" — anterior à estimação. *Verificar paginação na ed. Princeton.*
- **Hernán & Robins 2020**: estrutura tripartite Parte I (sem modelos = identificação) / Parte II-III (com modelos = estimação via g-formula, IPW, g-estimation). *Citação direta requer consulta ao PDF em https://miguelhernan.org/whatifbook.*
- **Lundberg, Johnson & Stewart 2021, ASR 86(3): 532-565**: "Researchers should (1) set a theoretical estimand, clearly connecting this quantity to theory; (2) link to an empirical estimand, which is informative about the theoretical estimand under some identification assumptions; and (3) learn from data." **Citação-âncora.**
- **Card 2022 (Nobel lecture)**: "model-based" vs "design-based" research; design-based = "primarily a strategy for identifying causalities".
- **Samii 2016 J. Politics 78(3): 941-955**: identification strategy = "the combination of a clearly labeled source of identifying variation in a causal variable and the use of a particular econometric [or other statistical] technique to exploit this information".
- **Slater & Ziblatt 2013 CPS 46(10): 1301-1327**: controlled comparisons "can generate both internal and external validity when practitioners (a) craft arguments with general variables or mechanisms, (b) seek out representative variation, and (c) select cases that maximize control over alternative explanations".

## 4. Veredito calibrado e wording alternativo

**Veredito (uma frase)**: o claim "consolidado na fronteira metodológica internacional" é **defensável para a literatura quanti** (doutrina de manual desde Angrist-Pischke 2009 e cristalizado por Lundberg-Johnson-Stewart 2021), mas **forte demais para a literatura quali** — onde o split está **presente e em consolidação crescente** (cluster F&C / H&J / Beach-Pedersen) mas ainda **convive** com tradições anti-KKV de outra arquitetura conceitual (Brady-Collier, Mahoney, Goertz).

**Wording alternativo (versão longa)**:
> "Esse split é hoje doutrina consolidada na metodologia quantitativa (Angrist & Pischke 2009; Lundberg, Johnson & Stewart 2021; Imbens 2020, 2022) e vem ganhando terreno crescente na metodologia qualitativa via a virada Bayesiana (Fairfield & Charman 2017, 2022; Humphreys & Jacobs 2015, 2023) e o resgate da comparação controlada como ferramenta de identificação (Slater & Ziblatt 2013), embora ainda coexista com tradições que tratam inferência causal qualitativa como um único problema integrado."

**Versão cirúrgica (uma só frase)**:
> "Já consolidado na fronteira quantitativa (revolução da credibilidade, do design-based de Card 2022 ao framework de estimands de Lundberg, Johnson & Stewart 2021) e em consolidação crescente na fronteira qualitativa via Bayes explícito (Fairfield & Charman 2022; Humphreys & Jacobs 2023)."

**Justificativa**: dizer "consolidado" sem qualificar é vulnerável a parecerista que conheça Brady & Collier 2010 ou Mahoney 2021. Dizer "ainda contestado" subestima o cluster F&C/H&J. A formulação "consolidado em quanti, em consolidação em quali" é a única que sobrevive ao crivo empírico.

**Implicação para a v8**: a Camada 1 ("premissa, não vender como descoberta") deve ser **explícita sobre essa assimetria**. Não é só "presente, não original" — é "consolidado em quanti, em consolidação em quali, e a contribuição-de-tradução é importar isso para a CP brasileira (Camada 2) com a sofisticação operacional de tratar enumeração-de-rivais como substituto do controle-de-confundidores em desenhos quali (Camada 3)". A assimetria **ajuda** a justificar a nota: o quali ainda fecha o gap, e a CP-BR está atrasada nesse fechamento.

## 5. Refs novas para acrescentar ao .bib (10 entries)

```bibtex
@book{cunningham2021mixtape,
  author    = {Cunningham, Scott},
  title     = {Causal Inference: The Mixtape},
  publisher = {Yale University Press},
  year      = {2021},
  address   = {New Haven, CT}
}

@book{huntingtonklein2022effect,
  author    = {Huntington-Klein, Nick},
  title     = {The Effect: An Introduction to Research Design and Causality},
  publisher = {CRC Press},
  year      = {2022},
  address   = {Boca Raton, FL}
}

@book{hernan_robins_2020,
  author    = {Hern{\'a}n, Miguel A. and Robins, James M.},
  title     = {Causal Inference: What If},
  publisher = {Chapman \& Hall/CRC},
  year      = {2020},
  address   = {Boca Raton, FL}
}

@book{morgan_winship_2015,
  author    = {Morgan, Stephen L. and Winship, Christopher},
  title     = {Counterfactuals and Causal Inference: Methods and Principles for Social Research},
  publisher = {Cambridge University Press},
  year      = {2015},
  edition   = {2},
  address   = {Cambridge}
}

@book{pearl2009causality,
  author    = {Pearl, Judea},
  title     = {Causality: Models, Reasoning, and Inference},
  publisher = {Cambridge University Press},
  year      = {2009},
  edition   = {2},
  address   = {Cambridge}
}

@book{pearl_mackenzie_2018,
  author    = {Pearl, Judea and Mackenzie, Dana},
  title     = {The Book of Why: The New Science of Cause and Effect},
  publisher = {Basic Books},
  year      = {2018},
  address   = {New York}
}

@article{imbens_2020_jel,
  author  = {Imbens, Guido W.},
  title   = {Potential Outcome and Directed Acyclic Graph Approaches to Causality: Relevance for Empirical Practice in Economics},
  journal = {Journal of Economic Literature},
  volume  = {58},
  number  = {4},
  pages   = {1129--1179},
  year    = {2020},
  doi     = {10.1257/jel.20191597}
}

@article{imbens_2022_econometrica,
  author  = {Imbens, Guido W.},
  title   = {Causality in Econometrics: Choice vs Chance},
  journal = {Econometrica},
  volume  = {90},
  number  = {6},
  pages   = {2541--2566},
  year    = {2022},
  doi     = {10.3982/ECTA21204},
  note    = {Nobel lecture}
}

@book{lipton2004ibe,
  author    = {Lipton, Peter},
  title     = {Inference to the Best Explanation},
  publisher = {Routledge},
  year      = {2004},
  edition   = {2},
  address   = {London}
}

@book{beach_pedersen_2019,
  author    = {Beach, Derek and Pedersen, Rasmus Brun},
  title     = {Process-Tracing Methods: Foundations and Guidelines},
  publisher = {University of Michigan Press},
  year      = {2019},
  edition   = {2},
  address   = {Ann Arbor, MI}
}
```

**Refs já no .bib que servem como âncora** (não duplicar): `Angrist_Pischke_2009`, `Angrist_Pischke_2010`, `Lundberg_etal_2021`, `Card_2022`, `Samii_2016`, `Goldsmith_2024`, `atheyStateAppliedEconometrics2017`, `Fairfield_Charman_2017`, `Fairfield_Charman_2019`, `fairfield_charman_2022`, `Humphreys_Jacobs_2015`, `Humphreys_Jacobs_2023`, `Bennett_Checkel_2015`, `slater_ziblatt_2013`, `brady_collier_2010`, `Mahoney_2010`, `mahoney_goertz_2006`, `Druckman_Green_2021`, `Findley_etal_2021`, `Keele_2015a`, `Keele_2015b`, `Pearl_Bareinboim_2022`, `Bareinboim_Pearl_2016`.

## 6. Nota final sobre prioridades para a v8

1. **Não vender o split como descoberta** — é premissa, não contribuição.
2. **Calibrar wording**: "consolidado em quanti, em consolidação em quali", não "consolidado e ponto".
3. **A alavanca da nota é Camada 2 + Camada 3** — CP-BR ainda não fechou esse gap (Camada 2, a substanciar com 5-8 refs BR — tarefa separada deste lit-review), e a manobra operacional de "enumeração de rivais como substituto do controle de confundidores em desenhos quali pequeno-n" (Camada 3) é a contribuição própria.
