# Silva (2023) ENAP — Consolidado da leitura paralela e implicação para a tese da v8

**Data**: 2026-05-08
**Pipeline**: 6 agentes paralelos lendo o livro por seções → este documento sintetiza o veredito
**Partials individuais**: `2026-05-08_glauco-silva-2023-partials.md` (10K palavras com trechos textuais)

---

## 1. Pergunta-mãe

O livro de Glauco Peres da Silva (DCP-USP), *Desenho de Pesquisa* (ENAP, 2023, ed. revisada), é o único candidato BR plausível a ser **Tipo C** (já fez o split identificação ≠ inferência) que o lit-review BR não pôde verificar via WebFetch. Se for Tipo C, o claim do gap BR da Camada 2 da v8 precisa ser calibrado.

## 2. Veredito consolidado

**Silva (2023) é Tipo A predominante, com elementos isolados de Tipo B no quanti (§3.1.2). NÃO é Tipo C.**

Distribuição por seção:

| Seção | Páginas | Classificação |
|---|---|---|
| §1 Introdução | 7-14 | A |
| §2.1 Produção de conhecimento | 15-24 | A |
| §2.2 Teorias e explicação | 25-33 | A |
| §2.3 **Causalidade** | 33-43 | **A** (pré-credibility-revolution; Brady 2008 cêntrico) |
| §3 + §3.1 + §3.1.1 Experimentos | 45-70 | A com qualificação (vocabulário antigo Sekhon 2007/Morton-Williams 2010) |
| §3.1.2 **Quase-experimentais** | 71-88 | **B com inclinações C, mas predominantemente B** |
| §3.2 **N-pequeno (quali)** | 89-108 | **A puro** (zero Bayes, zero F&C/H&J/Beach-Pedersen/Bennett-Checkel; zero process tracing nomeado; zero rivais como passo) |
| §3.3 Métodos mistos | 109-114 | A |
| §4 Considerações finais | 115-117 | A |
| Referências | 118-119 | A (cluster Pearl/DAG/SCM totalmente ausente) |

## 3. Os 3 achados decisivos

### 3.1 §2.3 Causalidade é Brady 2008, não Pearl/Rubin

A seção que **deveria** tematizar o split (Causalidade, pp. 33-43) abre com a nota de rodapé: *"A referência básica utilizada nesta seção é Brady (2008)"* (n. 23, p. 33). E executa exatamente isso: pluralismo filosófico em quatro perspectivas (Neo-Humeana / contrafactual / manipulação / mecanismos), apresentado via Brady (2008, p. 219, Tabela 1 do livro de Silva). **Zero Pearl. Zero Rubin. Zero potential outcomes formais. Zero credibility revolution.** O contrafactual é Lewisiano ("se X fosse ocorrer..."), não Y(0)/Y(1). O termo "identificação" aparece, mas em sentido coloquial — não como conceito técnico distinto de estimação.

Esta seção sozinha já decide a classificação. Um livro-texto BR de 2023 cuja seção **central** sobre causalidade é Brady-2008-cêntrica (sem Pearl, sem Rubin formalizado, sem Angrist-Pischke, sem F&C, sem H&J) **não fez o split**.

### 3.2 §3.1.2 mostra que o vocabulário existe — em registro pré-2010

Em §3.1.1 (pp. 66-67), Silva usa explicitamente "estratégias de identificação" e nomeia confounder/viés de variável omitida. Em §3.1.2, apresenta competentemente RDD, PSM, synth control, DiD com a intuição moderna correta. **Mas**:

- Sem o aparato unificador (não há "identifying assumption" como termo, não há "parallel trends" nomeado, não há "continuity assumption", não há DAGs, não há ignorabilidade)
- Sem a literatura pós-2010 (sem Cattaneo, McCrary, Goodman-Bacon, Callaway-Sant'Anna, Abadie 2021)
- Sem os manuais pedagógicos contemporâneos da credibility revolution (sem Angrist-Pischke 2009/2014, sem Cunningham 2021, sem Huntington-Klein 2022, sem Hernán-Robins 2020)

O bib do livro confirma: tem o "primeiro andar" da credibility revolution (Rosenbaum-Rubin 1983, Heckman-Hotz 1989, Bertrand-Duflo-Mullainathan 2004, Abadie-Diamond-Hainmueller 2010, Hahn-Todd-Van der Klaauw 1999, Lee 2008) **mas não tem a camada pedagógico-conceitual contemporânea**. É a credibility revolution na versão econometria-de-avaliação-de-programas (anos 1990-2000), não na versão design-based-pós-2010.

### 3.3 §3.2 N-pequeno é o achado mais decisivo para a v8

A seção qualitativa (pp. 89-108) **não cita uma única vez**:
- Bayes / Bayesiano / atualização / prior / posterior / likelihood ratio
- Fairfield & Charman (2017, 2022)
- Humphreys & Jacobs (2015, 2023)
- Bennett & Checkel (2015)
- Beach & Pedersen (2013, 2019)
- Process tracing (nem como termo, nem traduzido)
- Hipóteses rivais / explicações alternativas / explicações concorrentes (como passo metodológico estruturado)
- DAGs em quali / variável omitida em quali / U não-observado

A moldura é **Vennesson (2008) + Rueschemeyer (2003) + Seawright-Gerring (2008) + Mill (em nota) + Skocpol/Thompson/Michels como exemplos + path dependence (Mahoney 2000, Thelen 2003)**. KKV é tratado como interlocutor a quem se responde por "formação de conceitos", **não pela revolução da credibilidade nem pelo giro Bayesiano**.

**A noção de "credibilidade quali = robustez da comparação entre rivais" — peça central da Camada 3 da v8 — não é o quadro do livro**. A pergunta "como o quali responde à objeção do U?" sequer é colocada.

## 4. Implicação para a tese da v8 — RECALIBRAÇÃO MENOR, NÃO REVERSÃO

O veredito do lit-review BR original (`2026-05-08_lit-review-camada1-claim.md`) **NÃO muda em substância**. Silva 2023 não é Tipo C. **Pelo contrário, é evidência reforçada do gap.** Mas há nuance honesta a fazer.

### 4.1 O que NÃO muda

- A Camada 2 (gap BR) **permanece defensável e agora com substanciação adicional**: Silva 2023 vira a peça central de evidência. Não é só "a literatura metodológica BR antiga não fez o split"; é "mesmo o livro-texto recente (2023), de editora oficial (ENAP), de autor jovem (DCP-USP), apresentando o estado-da-arte do *desenho de pesquisa* para CP brasileira, opera dentro de moldura Brady 2008 + Vennesson 2008 + Seawright 2016, sem Pearl/Rubin/F&C/H&J".
- A Camada 3 (contribuição operacional sobre rivais como substituto de DAG-U) **permanece intacta** — Silva 2023 não antecipa o problema nem a solução.
- O wording "consolidado em quanti, em consolidação em quali" para a fronteira intl **permanece**.

### 4.2 O que muda (recalibração honesta)

A formulação da Camada 2 não pode mais dizer "a CP brasileira ignora a credibility revolution" sem qualificação. **Tem que dizer**:

> "A CP brasileira começa a importar termos isolados da revolução da credibilidade (Silva 2023 usa 'estratégias de identificação' em §3.1.1, apresenta RDD/PSM/synth control/DiD em §3.1.2 com intuição moderna correta), mas:
> 
> (a) Sem o framework conceitual unificador (DAGs, potential outcomes formais, ignorabilidade, parallel trends nomeada, continuity assumption);
> 
> (b) Sem a literatura pedagógica contemporânea (Angrist-Pischke 2009/2014, Cunningham 2021, Huntington-Klein 2022, Hernán-Robins 2020);
> 
> (c) Sem estender o vocabulário para o quali (a §3.2 N-pequeno opera em vocabulário Vennesson + Rueschemeyer + Mill + path dependence, sem Bayes, sem F&C, sem H&J, sem process tracing nomeado, sem rivais como passo metodológico);
> 
> (d) Sem o split design/estimação como princípio organizador (a §2.3 Causalidade é Brady 2008-cêntrica, pluralismo filosófico pré-credibility-revolution).
> 
> O gap, portanto, não é de **vocabulário** (alguns termos chegaram), mas de **arquitetura conceitual** — o split identificação ≠ inferência ainda não é princípio organizador da pedagogia metodológica BR, e a virada Bayesiana qualitativa simplesmente não está no horizonte."

Esta é uma versão **mais defensável** do gap — mais específica, mais difícil de refutar.

## 5. Wording recomendado para a v8 (atualizado)

Substituir a versão sintética anterior:

> "Já consolidado na fronteira metodológica quantitativa internacional (revolução da credibilidade) e em consolidação crescente na fronteira qualitativa via Fairfield-Charman, Humphreys-Jacobs e a inferência à melhor explicação (Lipton 2004), embora ainda conviva com tradições que tratam o problema como integrado."

por (versão para a Camada 1):

> "Esse split é hoje doutrina consolidada na metodologia quantitativa internacional (Angrist-Pischke 2009; Lundberg-Johnson-Stewart 2021; Imbens 2020, 2022) e em consolidação crescente na metodologia qualitativa via virada Bayesiana (Fairfield-Charman 2017, 2022; Humphreys-Jacobs 2015, 2023) e inferência à melhor explicação (Lipton 2004). A literatura metodológica brasileira começa a importar termos do design-based identification (Silva 2023 nomeia 'estratégias de identificação' em manual ENAP), mas sem incorporá-lo como princípio organizador — a seção sobre causalidade do mesmo manual é organizada em torno do pluralismo filosófico de Brady (2008), e a seção qualitativa opera em vocabulário Vennesson-Rueschemeyer-Mill, sem Bayes nem process tracing nomeado. A v8 propõe que o split, e em particular sua extensão para desenhos qualitativos via comparação Bayesiana de explicações rivais, é a contribuição metodológica que falta consolidar na CP brasileira."

## 6. Refs novas para .bib que emergem da leitura do Silva

Já existe `Silva_2023` na lista do lit-review BR original. Atualizar a entrada para refletir conteúdo confirmado (não mais "VERIFICAR conteudo"):

```bibtex
@book{Silva_2023,
  author    = {Silva, Glauco Peres da},
  title     = {Desenho de Pesquisa},
  publisher = {ENAP},
  series    = {Cole{\c c}{\~a}o Metodologias de Pesquisa},
  address   = {Bras{\'i}lia},
  year      = {2023},
  edition   = {Edi{\c c}{\~a}o revisada},
  isbn      = {978-65-87791-31-9},
  pages     = {119},
  url       = {https://repositorio.enap.gov.br/bitstream/1/3330/4/Livro_desenho_de_pesquisa%20(2).pdf},
  note      = {Manual da ENAP/DCP-USP. Tipo A predominante: opera em moldura Brady 2008 + Vennesson 2008 + Seawright 2016, sem Pearl/Rubin/F\&C/H\&J. Importa "estratégias de identificação" em §3.1.1 (pp. 66-67) sem fazer dele princípio organizador.}
}
```

A nota é interna ao bib — pode ser comentada antes de submeter (`note = {…}`) ou removida.

Outras refs novas que emergem do livro do Silva e podem virar entries (se forem usadas pela v8):
- **Brady, Henry E. (2008).** "Causation and Explanation in Social Science." In *Oxford Handbook of Political Methodology* (Box-Steffensmeier, Brady, Collier eds.), Cap. 10. — pode entrar se a v8 quiser citar a moldura pluralista de Silva.
- **Sekhon, Jasjeet S. (2008).** "The Neyman-Rubin Model of Causal Inference and Estimation Via Matching Methods." In *Oxford Handbook of Political Methodology*. — referência Silva-2023-internal de Rubin.
- **Vennesson, Pascal (2008).** "Case studies and process tracing: theories and practices." In *Approaches and Methodologies in the Social Sciences* (Della Porta-Keating eds.), Cap. 12. — referência Silva-2023-internal para estudo de caso.
- **Seawright, Jason (2016).** *Multi-Method Social Science: Combining Qualitative and Quantitative Tools*. CUP. — já no .bib como `seawright_2018`? Verificar; se não, adicionar.

## 7. Próximas ações

1. **Citar Silva (2023) na v8** como peça central de evidência da Camada 2, com a recalibração da Seção 4.2 acima.
2. **Atualizar a entrada Silva_2023 no .bib** (remover marca "VERIFICAR conteudo").
3. **Considerar adicionar Brady 2008** ao .bib se a v8 quiser usar a moldura "quatro perspectivas" como ponto de contraste contra o split Pearl/Rubin.
4. **Atualizar o documento `2026-05-08_lit-review-camada1-claim.md`** com a nota de que Silva 2023 foi verificado como Tipo A predominante (com elementos B em §3.1.2), não Tipo C.
5. **Aguardar agente Spirling-Stewart** (rodando em background) para fechar a leitura do framework IBE intl que ancora a Camada 3 da v8.

---

**TL;DR**: Silva (2023) **não muda** o veredito BR — apenas o **substancia mais e o calibra com mais finura**. A pendência bloqueante do lit-review BR está resolvida: o livro NÃO faz o split, e isso é evidência forte (não fraca) para a v8.
