# Lit-review consolidado — claim "consolidado" da Camada 1 do plano v8

**Data**: 2026-05-08
**Skill**: `lit-review`
**Pipeline**: dois agentes paralelos (intl + BR) → síntese consolidada
**Parciais**:
- `quality_reports/2026-05-08_lit-review-intl-partial.md` (literatura internacional)
- `quality_reports/2026-05-08_lit-review-br-partial.md` (literatura BR)

---

## 1. Pergunta-mãe

O claim da **Camada 1** do plano v8:

> "Identificação causal e inferência estatística são problemas distintos. Já consolidado na fronteira metodológica internacional via revolução da credibilidade (quanti) e Fairfield-Charman / Humphreys-Jacobs / Slater-Ziblatt / inference to the best explanation (quali)."

está calibrado, ou "consolidado" é forte demais (autor sugere que pode ser apenas "presente, não original")?

## 2. Veredito consolidado

**O wording original "consolidado" precisa de DUAS calibrações:**

### Calibração 1 — assimetria intl quanti vs. quali

- **Quanti**: "consolidado" é correto e até modesto. O split é **doutrina de manual desde Angrist-Pischke 2009** e foi cristalizado por Lundberg-Johnson-Stewart 2021 ASR (pipeline tripartite estimando-teórico → estimando-empírico → estimador). Manuais centrais: Mostly Harmless (Angrist-Pischke 2009), The Effect (Huntington-Klein 2022), The Mixtape (Cunningham 2021), What If (Hernán-Robins 2020), Counterfactuals (Morgan-Winship 2015). Imbens 2020 JEL e 2022 Econometrica (Nobel lecture) tomam o split como pressuposto. Card 2022 (Nobel) usa "design-based" para nomear o split. Status: **(a) consolidado/lugar-comum**.

- **Quali**: "consolidado" é **forte demais**. O split está **presente e em consolidação crescente** (cluster Fairfield-Charman 2017/2022, Humphreys-Jacobs 2015/2023, Beach-Pedersen 2019, Bennett-Checkel 2015), mas **convive** com tradições anti-KKV de outra arquitetura conceitual (Brady-Collier 2010, Mahoney 2010/2021, Goertz-Mahoney 2012). Slater-Ziblatt 2013 revaloriza identification em quali, mas com vocabulário Campbell-Stanley (validade), não com o par técnico identification/inference da credibility revolution. Status: **(b) presente e crescente, não unânime**.

### Calibração 2 — gap BR confirmado

A literatura metodológica brasileira opera **predominantemente** dentro da moldura inferencial pós-KKV. Razão **8:4:0** (Tipo A : Tipo B : Tipo C) entre papers metodológicos publicados em periódicos CP/RI BR:

- **Tipo A (gap explícito)**: Rezende 2017, 2019; Mesquita 2017; Paula 2018; Leite & Rocha 2019; Figueiredo Filho et al. 2021; Bachini & Chicarino 2018 (já no .bib); Amorim Neto & Rodriguez 2016 (já no .bib). + 3 manuais (Cervi 2017, Figueiredo Filho 2019, Manual ENAP quali).
- **Tipo B (avanço parcial)**: Rezende 2011; Sposito et al. 2022 (já no .bib — recontextualizar); Vick & Gurza Lavalle 2020; Perissinotto 2024.
- **Tipo C (já fez o split)**: **0 confirmadas em periódico CP/RI BR**.

Ressalva crítica: **Glauco Peres da Silva (2023), *Desenho de Pesquisa* (ENAP)** é o único candidato BR plausível a Tipo C que não foi possível verificar (PDF binário não parseável via WebFetch). Se o livro tematiza o split, o veredito BR muda de "predominantemente ausente" para "predominantemente ausente, com exceção do livro-texto recente de Silva 2023". **Inspecionar manualmente antes da submissão da v8.**

A figura central do mainstream BR é **Flávio da Cunha Rezende** (UFPE), com a agenda do "Pluralismo Inferencial pós-KKV" — pluralização **dentro** do framework KKV, não ruptura com ele. **Sposito et al. 2022** (já no .bib) é o avanço parcial mais próximo do que a v8 propõe — organiza seleção de casos por tipo de alegação causal (probabilística / mecanística / set-theoretic), um movimento design-first sem o vocabulário identification/inference.

## 3. Wording recomendado para a v8

Três versões em ordem decrescente de detalhe. Escolher conforme o lugar do paper.

### Versão longa (para introdução / seção sobre revolução da credibilidade)

> Esse split é hoje doutrina consolidada na metodologia quantitativa (Angrist & Pischke 2009; Lundberg, Johnson & Stewart 2021; Imbens 2020, 2022) e vem ganhando terreno crescente na metodologia qualitativa via a virada Bayesiana (Fairfield & Charman 2017, 2022; Humphreys & Jacobs 2015, 2023) e o resgate da comparação controlada como ferramenta de identificação (Slater & Ziblatt 2013), embora ainda coexista com tradições que tratam inferência causal qualitativa como um único problema integrado (Brady & Collier 2010; Mahoney 2010). A literatura metodológica brasileira, por sua vez, opera predominantemente dentro da moldura inferencial pós-KKV — em que desenho, identificação e estimação são facetas de um mesmo problema de "inferência válida" (Rezende 2017, 2019; Mesquita 2017; Paula 2018; Leite & Rocha 2019; Figueiredo Filho et al. 2021) — com avanços parciais que reconhecem especificidades epistemológicas do quali (Rezende 2011) ou organizam o desenho em torno de tipos de alegação causal (Sposito, Gabriel & Artioli 2022) sem completar o split conceitual.

### Versão cirúrgica (para abstract ou primeira frase de seção)

> Já consolidado na fronteira quantitativa (revolução da credibilidade, do design-based de Card 2022 ao framework de estimands de Lundberg, Johnson & Stewart 2021) e em consolidação crescente na fronteira qualitativa via Bayes explícito (Fairfield & Charman 2022; Humphreys & Jacobs 2023), esse split ainda não foi incorporado à literatura metodológica brasileira (Rezende 2017, 2019; Mesquita 2017; Sposito et al. 2022 como avanço parcial).

### Versão sintética (para a Camada 1 conforme aparece no plano)

Substituir
> "Já consolidado na fronteira metodológica internacional via revolução da credibilidade (quanti) e Fairfield-Charman / Humphreys-Jacobs / Slater-Ziblatt / inference to the best explanation (quali)."

por

> "Já consolidado na fronteira metodológica quantitativa internacional (revolução da credibilidade) e em consolidação crescente na fronteira qualitativa via Fairfield-Charman, Humphreys-Jacobs e a inferência à melhor explicação (Lipton 2004), embora ainda conviva com tradições que tratam o problema como integrado."

## 4. Implicação para a estrutura do paper v8

### A premissa permanece como premissa, não como descoberta

O plano da v8 já tinha **rejeitado** "vender o split como descoberta" e classificado a Camada 1 como "premissa, não vender como descoberta". O lit-review **confirma** a decisão e fornece o argumento empírico: dizer "consolidado" sem qualificar é vulnerável a parecerista que conheça Brady-Collier ou Mahoney; dizer "ainda contestado" subestima o cluster F&C/H&J. A formulação calibrada "consolidado em quanti, em consolidação em quali" é a única que sobrevive ao crivo empírico.

### A assimetria *ajuda* o paper

A assimetria quanti/quali na fronteira intl é **ativo argumentativo** para a v8, não passivo:
- Em quanti, é fato consumado — não precisa defender.
- Em quali, está em consolidação — então o paper *contribui* para o fechamento do gap (Camada 3, com o ponto operacional sobre rivais como substituto de DAG-U).
- E em CP-BR, não chegou — o paper traduz para BR (Camada 2) com a sofisticação operacional da Camada 3.

Logo a tese-em-três-camadas **se fortalece** com a calibração: não é "premissa banal + tradução BR + ponto operacional", é "premissa parcialmente em consolidação + tradução BR + ponto operacional sobre como completar essa consolidação no quali".

### A Camada 2 (gap BR) está substanciada

O plano da v8 exigia 5–8 refs BR para evitar que a Camada 2 fosse retórica. **Temos 8 refs Tipo A + 4 Tipo B + 3 manuais**, todos com argumento e justificativa de classificação. A v8 pode citar:
- 5 centrais (Rezende 2017, Mesquita 2017, Paula 2018, Leite & Rocha 2019, Figueiredo Filho et al. 2021)
- 2 já no .bib (Amorim Neto & Rodriguez 2016, Bachini & Chicarino 2018) recontextualizadas
- 2 avanços parciais positivos (Rezende 2011, Sposito et al. 2022) — citar é honesto e fortalece o argumento
- Manuais (Cervi 2017, Figueiredo Filho 2019) como evidência do vocabulário pedagógico instalado

### Pendência operacional bloqueante para a v8

**Inspecionar Glauco Peres da Silva, *Desenho de Pesquisa* (ENAP 2023)** antes de submeter. URL do PDF: https://repositorio.enap.gov.br/bitstream/1/3330/4/Livro_desenho_de_pesquisa%20(2).pdf. Se o livro distinguir formalmente identificação de inferência, o claim BR precisa ser calibrado: "predominantemente, com exceção emergente do livro-texto de Silva 2023 que sinaliza transição".

## 5. Refs novas para .bib (consolidado dos dois parciais)

### Internacional (10 entries)

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

### Brasil (12 entries)

```bibtex
@article{Rezende_2019,
  title = {As L{\'o}gicas Da Infer{\^e}ncia Causal Na {{Ci{\^e}ncia Pol{\'i}tica}}: Argumento e Evid{\^e}ncias},
  author = {{da Cunha Rezende}, Fl{\'a}vio},
  year = {2019},
  journal = {Revista Pol{\'i}tica Hoje},
  volume = {28},
  number = {2},
  url = {https://periodicos.ufpe.br/revistas/politicahoje/article/view/248309}
}

@article{Rezende_2011,
  title = {Raz{\~o}es Emergentes Para a Validade Dos Estudos de Caso Na Ci{\^e}ncia Pol{\'i}tica Comparada},
  author = {{da Cunha Rezende}, Fl{\'a}vio},
  year = {2011},
  journal = {Revista Brasileira de Ci{\^e}ncia Pol{\'i}tica},
  number = {6},
  pages = {297--337},
  url = {https://www.scielo.br/j/rbcpol/a/KFWXKW9VQNmQTt7BZpsV4rx/}
}

@article{Mesquita_2017,
  title = {Desenho de Pesquisa, Infer{\^e}ncia e Causalidade Em {{Ci{\^e}ncia Pol{\'i}tica}} e {{Rela{\c c}{\~o}es Internacionais}}: Uma Introdu{\c c}{\~a}o Did{\'a}tica},
  author = {Mesquita, Rafael},
  year = {2017},
  journal = {Revista Pol{\'i}tica Hoje},
  volume = {26},
  number = {2},
  url = {https://periodicos.ufpe.br/revistas/politicahoje/article/view/234402}
}

@article{Paula_2018,
  title = {Em Busca Da Infer{\^e}ncia V{\'a}lida: M{\'e}todos e Testes de Hip{\'o}teses Nos Estudos Legislativos Brasileiros},
  author = {Paula, Julio Cesar Guimar{\~a}es de},
  year = {2018},
  journal = {Revista Brasileira de Ci{\^e}ncia Pol{\'i}tica},
  number = {26},
  url = {https://www.scielo.br/j/rbcpol/a/pG4v5GHwd3rJxsLThVckNrv/}
}

@article{Leite_Rocha_2019,
  title = {Desenho de Pesquisa, Infer{\^e}ncia e Causalidade: Caminhos Entre a Abordagem Qualitativa e Quantitativa},
  author = {Leite, Rodrigo and Rocha, Gustavo de Andrade},
  year = {2019},
  journal = {Revista Eletr{\^o}nica de Ci{\^e}ncia Pol{\'i}tica},
  volume = {10},
  number = {1},
  pages = {107--119},
  url = {https://revistas.ufpr.br/politica/article/view/61004}
}

@article{Figueiredo_etal_2021,
  title = {Metodologias de Pesquisa Em Ci{\^e}ncia Pol{\'i}tica: Uma Breve Introdu{\c c}{\~a}o},
  author = {Figueiredo Filho, Dalson Britto and Fernandes, Ant{\^o}nio and Borba, Lucas and Aguiar, Tha{\'i}s Helena},
  year = {2021},
  journal = {BIB - Revista Brasileira de Informa{\c c}{\~a}o Bibliogr{\'a}fica em Ci{\^e}ncias Sociais},
  number = {94},
  url = {https://bibanpocs.emnuvens.com.br/revista/article/view/175}
}

@article{Vick_Lavalle_2020,
  title = {{\'E} a Pol{\'i}tica\dots A Efetividade Das Confer{\^e}ncias e Seus Mecanismos Causais},
  author = {Vick, Fernanda and Gurza Lavalle, Adri{\'a}n},
  year = {2020},
  journal = {Opini{\~a}o P{\'u}blica},
  volume = {26},
  number = {3},
  pages = {556--586},
  url = {https://periodicos.sbu.unicamp.br/ojs/index.php/op/article/view/8663884}
}

@article{Perissinotto_2024,
  title = {{{QCA}} e {{Process Tracing}}: Conectando Ci{\^e}ncia Pol{\'i}tica e Hist{\'o}ria},
  author = {Perissinotto, Renato},
  year = {2024},
  journal = {Revista Brasileira de Ci{\^e}ncias Sociais},
  volume = {39},
  url = {https://www.scielo.br/j/rbcsoc/a/KYXyZtgMQxXKmzLHshMfrRD/}
}

@book{Cervi_2017,
  title = {Manual de M{\'e}todos Quantitativos Para Iniciantes Em Ci{\^e}ncia Pol{\'i}tica},
  author = {Cervi, Emerson Urizzi},
  year = {2017},
  publisher = {{CPOP/UFPR}},
  address = {Curitiba},
  volume = {1},
  url = {https://cpop.ufpr.br/wp-content/uploads/2017_cervi_mq_vol1.pdf}
}

@book{FigueiredoFilho_2019,
  title = {M{\'e}todos Quantitativos Em Ci{\^e}ncia Pol{\'i}tica},
  author = {Figueiredo Filho, Dalson Britto},
  year = {2019},
  publisher = {{Intersaberes}},
  address = {Curitiba}
}

@book{Silva_2023,
  title = {Desenho de Pesquisa},
  author = {Silva, Glauco Peres da},
  year = {2023},
  publisher = {{ENAP}},
  series = {Cole{\c c}{\~a}o Metodologias de Pesquisa},
  address = {Bras{\'i}lia},
  url = {https://repositorio.enap.gov.br/bitstream/1/3330/4/Livro_desenho_de_pesquisa%20(2).pdf}
}

@article{Lenine_etal_2023,
  title = {Process Tracing Na Ci{\^e}ncia Pol{\'i}tica e Nas Rela{\c c}{\~o}es Internacionais Brasileiras: Uma An{\'a}lise Bibliom{\'e}trica (2012-2023)},
  author = {Lenine, Enzo and Grizenti, Eduardo and Bia, Agnes and Cardoso, Beatriz},
  year = {2023},
  journal = {Carta Internacional},
  volume = {18},
  number = {3},
  url = {https://cartainternacional.abri.org.br/Carta/article/view/1365}
}
```

**Total**: 22 entries novas (10 intl + 12 BR). Algumas com `% verificar` no parcial BR (paginação/fascículo) — limpar antes do append final.

## 6. Atualização requerida no plano da v8

A linha 18 do plano (`quality_reports/plans/2026-05-08_v8-reformulacao.md`):

> "**Camada 1 — premissa (não vender como descoberta):**
> Identificação causal e inferência estatística são problemas distintos. Já consolidado na fronteira metodológica internacional via revolução da credibilidade (quanti) e Fairfield-Charman / Humphreys-Jacobs / Slater-Ziblatt / inference to the best explanation (quali)."

deve ser substituída por:

> "**Camada 1 — premissa (não vender como descoberta):**
> Identificação causal e inferência estatística são problemas distintos. Consolidado na fronteira metodológica quantitativa (revolução da credibilidade — Angrist-Pischke 2009, Lundberg-Johnson-Stewart 2021, Imbens 2020/2022, Card 2022) e em consolidação crescente na fronteira qualitativa via virada Bayesiana (Fairfield-Charman 2017/2022, Humphreys-Jacobs 2015/2023, Beach-Pedersen 2019) e a inferência à melhor explicação (Lipton 2004), embora ainda conviva com tradições que tratam o problema como integrado (Brady-Collier 2010, Mahoney 2010/2021)."

E a P4 do plano (linha 90-98) ganha as 8 refs Tipo A + 4 Tipo B + 3 manuais como insumo concreto, eliminando o risco de "claim retórico".

## 7. Limites do lit-review

- Cobertura BR não é varredura sistemática (DADOS, BPSR, RBCS, RBCP, RBPI, Opinião Pública, Lua Nova de 2010-2025); 12 papers + 3 manuais são consistentes com o universo, não exaustivos.
- Cobertura intl prioriza textos canônicos; não revisita systematic reviews recentes do estado-da-arte para CP especificamente (ex.: ARPS).
- **Pendência bloqueante**: verificação manual de Silva 2023 (ENAP) — único candidato BR plausível a Tipo C não verificado.
- Pontos cegos: dissertações Lattes-CAPES recentes, RI brasileira (RBPI/Carta Internacional), produção aplicada de Barberia/Freitas/Silva.

## 8. Próximas ações

1. **Apresentar veredito ao autor** e confirmar wording final (versão longa/cirúrgica/sintética conforme uso no paper).
2. **Inspecionar manualmente Silva 2023 (ENAP)** — abrir o PDF localmente e classificar.
3. **Atualizar a linha 18 do plano da v8** com o wording calibrado.
4. **Append das 22 entries** em `Quali-credibilidade.bib` (com confirmação do autor; algumas precisam de paginação/fascículo verificados).
5. **Recontextualizar** Sposito et al. 2022, Bachini & Chicarino 2018, Amorim Neto & Rodriguez 2016 conforme a classificação A/B do parcial BR.
