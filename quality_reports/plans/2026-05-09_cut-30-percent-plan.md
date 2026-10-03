# Plano Editorial: Cortar Pelo Menos 30% Do Paper

**Data**: 2026-05-09  
**Arquivo-alvo**: `paper_dados_format_quali.Rmd`  
**Status**: diagnóstico para revisão do autor; não implementar sem nova autorização.

## Diagnóstico Quantitativo

O manuscrito tem aproximadamente:

- **12.221 palavras de corpo** no Rmd.
- **41 páginas** no PDF compilado.

Meta de corte:

- Cortar pelo menos **30%**, isto é, cerca de **3.700 a 4.000 palavras**.
- Alvo aproximado: **8.500 palavras** e algo como **28-30 páginas**, dependendo de tabelas, figura e referências.

## Tamanho Por Seção

| Seção | Palavras aprox. | Diagnóstico |
|---|---:|---|
| Introdução | 652 | Boa, mas o roteiro final pode ser encurtado. |
| A recepção brasileira do debate metodológico pós-KKV | 945 | Importante para contribuição BR; cortar moderadamente. |
| Revolução da Credibilidade | 1.377 | Precisa virar enquadramento, não revisão histórica. |
| De variável omitida a explicação rival | 2.082 | Central, mas tem redundância e a seleção de casos pode ser comprimida. |
| Soluções Práticas para Inferência em Amostras Pequenas | 553 | Pode ser incorporada à seção seguinte. |
| Novos Desenhos Causais Qualitativos | 2.315 | Principal alvo; está didática demais, quase mini-manual. |
| Limitações práticas e trade-offs | 465 | Pode virar parágrafo curto ou nota. |
| Ilustração: impeachment de 2016 | 1.637 | Boa, mas longa para ilustração; cortar prosa em torno das tabelas. |
| Transportabilidade, Generalização ou Validade Externa | 1.210 | Muito redundante; mistura versão nova e restos antigos. |
| Considerações Finais | 981 | Repete quase tudo; deve cair pela metade. |

## Plano De Corte Recomendado

### 1. Cortar Pesado “Novos Desenhos Causais Qualitativos”

**Corte estimado**: ~1.500 palavras.

Reduzir de 2.315 para ~800 palavras.

Manter:

- Fairfield-Charman como formalização do *process tracing* Bayesiano.
- Humphreys-Jacobs como modelo de *queries* causais.
- Diferença operacional entre as duas abordagens.
- Ponto comum: ambas operacionalizam a comparação de explicações rivais.

Cortar ou mover para apêndice:

- Discussão longa sobre elicitação de prioris.
- Exemplo do acidente de avião.
- Detalhamento dos quatro tipos causais.
- Subsubseções sobre efeito causal ao nível do caso, atribuição causal e caminhos causais.
- Repetições sobre variáveis omitidas que já aparecem na seção central.

Recomendação estrutural: fundir as seções “Soluções Práticas” e “Novos Desenhos” em uma seção curta, algo como:

> “Duas implementações: *process tracing* Bayesiano e *queries* causais”

### 2. Reescrever “Transportabilidade”

**Corte estimado**: ~750 palavras.

Reduzir de 1.210 para ~450 palavras.

Problema atual:

- A seção começa com a tese correta, mas depois volta a versões antigas: Campbell/McDermott, fórmula de validade externa, simpósio QMMR, Filipinas/Vietnã, escopo de Fairfield-Charman.
- Há redundância com a introdução, seção central e conclusão.

Manter:

- Validade externa como transportabilidade.
- Pequeno-n não é o problema lógico da generalização.
- Transportar efeitos/explicações exige teoria independente sobre modificadores de efeito e condições de escopo.
- Redefinir escopo ad hoc enfraquece a finitude do conjunto de rivais.

Cortar:

- Fórmula de validade externa, a menos que seja essencial.
- Longo exemplo Filipinas/Vietnã.
- Repetição de que quali e quanti têm limitações semelhantes.

### 3. Condensar “Revolução da Credibilidade”

**Corte estimado**: ~550 palavras.

Reduzir de 1.377 para ~800 palavras.

Manter:

- Identificação causal versus inferência estatística.
- Diferença entre design-based e model-based identification.
- Framework Bayesiano subjetivista apenas no que sustenta o paralelo CR/IBE.

Cortar:

- Histórico longo Leamer/credibility em economia.
- Explicações muito didáticas de potenciais outcomes.
- Exemplo INUS/incêndio em versão longa.

Opção forte:

- Transformar a figura do incêndio em nota ou mover para apêndice. A figura custa espaço de página e talvez não seja essencial para a tese principal.

### 4. Enxugar A Seção Central

**Corte estimado**: ~500 palavras.

Reduzir de 2.082 para ~1.550 palavras.

Manter:

- Framing correto: CR pode ser tentada em pequeno-n, mas frequentemente devolve incerteza grande.
- O trabalho inferencial vem de restrições qualitativas substantivas.
- Objeção muda de “e se houver U?” para “qual rival substantiva foi omitida?”.
- Distinção com Spirling-Stewart.

Cortar/comprimir:

- “A seleção de casos, decomposta” está longa e reaparece na conclusão.
- Exemplos Card-Krueger e Abadie podem ser encurtados.
- O paralelo estrutural CR/IBE pode ser mais direto.

### 5. Condensar A Ilustração Do Impeachment

**Corte estimado**: ~400 palavras.

Reduzir de 1.637 para ~1.200 palavras.

Manter:

- Tentativa design-based → incerteza grande.
- Hipóteses rivais com H6 composta.
- Evidências sequenciais.
- Moral metodológica.

Cortar:

- Prosa explicativa antes e depois das tabelas.
- Sensibilidade pode virar um parágrafo sem tabela, ou uma tabela menor.
- Hipóteses/evidências podem ser descritas em frases mais curtas.

### 6. Cortar Considerações Finais Pela Metade

**Corte estimado**: ~480 palavras.

Reduzir de 981 para ~500 palavras.

Manter:

- Tese principal.
- Implicação para o debate pós-KKV.
- Mudança do critério de credibilidade.
- Agenda de protocolos, mas em forma compacta.

Cortar:

- Reexplicação longa sobre seleção de casos.
- Repetição da simetria CR/IBE.
- Detalhamento dos três protocolos, que pode ser uma frase ou nota.

### 7. Pequenos Cortes Na Introdução E Recepção Brasileira

**Corte estimado**: ~300 palavras.

Introdução:

- Reduzir o roteiro do paper a uma frase.
- Compactar a apresentação das duas camadas.

Recepção brasileira:

- Encurtar o parágrafo sobre Sposito/EQ/PE/TC.
- Manter Silva 2023 e o gap arquitetural como evidência central.
- Reduzir enumeração de precedentes brasileiros.

## Soma Esperada Dos Cortes

| Bloco | Corte estimado |
|---|---:|
| Novos Desenhos + Soluções Práticas | ~1.500 |
| Transportabilidade | ~750 |
| Revolução da Credibilidade | ~550 |
| Seção central | ~500 |
| Impeachment | ~400 |
| Considerações finais | ~480 |
| Introdução + recepção brasileira | ~300 |
| **Total potencial** | **~4.480** |

Mesmo que nem todos os cortes sejam implementados integralmente, há margem para atingir a meta de **3.700-4.000 palavras**.

## Arquitetura Recomendada Pós-Corte

1. Introdução.
2. Recepção brasileira e o gap arquitetural.
3. Identificação, inferência e revolução da credibilidade.
4. De variável omitida a explicação rival.
5. Duas implementações: *process tracing* Bayesiano e *queries* causais.
6. Ilustração: impeachment de 2016.
7. Transportabilidade e escopo.
8. Considerações finais.

## Regra Editorial

Não cortar a tese central. Cortar principalmente:

- didatismo que explica literatura já citada;
- exemplos auxiliares que não carregam a contribuição;
- repetições da diferença identificação/inferência;
- reconstruções longas de debates que podem ser citados em vez de narrados;
- detalhes operacionais que podem ir para apêndice ou nota.

