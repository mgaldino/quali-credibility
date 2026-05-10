# AGENTS.md

Instruções locais para agentes Codex trabalhando neste repositório.

## Escopo Do Projeto

Este repositório contém o paper `paper_dados_format_quali.Rmd`, uma nota metodológica em português sobre Revolução da Credibilidade, inferência Bayesiana e pesquisa qualitativa causal.

O diretório pai contém `CLAUDE.md`, mas este arquivo é a referência operacional para Codex dentro do repo `quali-credibility/`.

## Preferências Do Autor

- Usar português com acentos em textos, relatórios e documentação.
- Preferir R para análise de dados e simulações estatísticas.
- Preferir Python para scraping, transcrição, processamento de texto e tarefas de ML.
- Manter computação em scripts separados, não embutida no manuscrito quando for substantiva.
- Tornar análises, relatórios e papers reprodutíveis.
- Preservar arquivos brutos e derivados no diretório do projeto, mas arquivos grandes ficam apenas locais.
- Não usar Git LFS.
- Não alterar arquivos fora do repositório sem autorização explícita.
- Em R, usar `dplyr::select()` ao selecionar colunas.
- Tabelas e figuras em relatórios/papers devem ser numeradas e ter caption.

## Sobre Q&A

A antiga regra "sempre começar com Q&A" não se aplica mais ao trabalho com Codex. Não fazer perguntas e respostas simuladas. Perguntar ao autor apenas quando uma decisão substantiva não puder ser inferida com segurança do contexto local.

## Estado Conceitual Atual Da v8

Não usar mais o enquadramento antigo segundo o qual a Revolução da Credibilidade estaria simplesmente "indisponível" em pequeno-n ou segundo o qual IBE "substitui" CR de maneira direta.

Formulação correta após a discussão de 2026-05-09:

- Estratégias design-based da Revolução da Credibilidade podem, em princípio, ser tentadas em estudos de pequeno-n.
- Em muitos casos qualitativos, essas estratégias retornam grande incerteza para estimativas pontuais de efeito causal, porque há poucos dados, poucos comparáveis ou baixa potência.
- A contribuição qualitativa não é "Bayes resolve escassez de evidência". Sem restrições estruturais, priors apertadas dominam o resultado e isso não é evidência.
- O que a análise qualitativa adiciona são evidências processuais e restrições estruturais: sequência, mecanismos, mediadores, atores, regras institucionais, lógica INUS/SUIN, condições necessárias/suficientes, e testes negativos entre explicações rivais.
- O alvo inferencial da parte qualitativa é discriminar explicações rivais via IBE + Bayes, não necessariamente estimar pontualmente um efeito causal.
- O paper deve distinguir os alvos: estimativa pontual design-based com incerteza versus comparação de explicações rivais com evidência processual e restrições substantivas.

Evitar:

- "Não há como usar SCM/RDD/IV/DiD em caso qualitativo."
- "N pequeno torna CR operacionalmente indisponível por definição."
- "Bayes resolve falta de dados."
- "Lava Jato" e "coalizão" como hipóteses independentes e mutuamente exclusivas no estudo de caso.

## Estudo De Caso Do Impeachment

O estudo de caso foi reescrito em 2026-05-09 com base em Limongi. Antes de novas mudanças substantivas, ler a seção atual em `paper_dados_format_quali.Rmd` e o script `scripts/impeachment_bayes_example.R`.

Fonte principal de trabalho:

- `quality_reports/limongi_argument_map.md`
- `quality_reports/limongi_chunks/part1_synthesis.md`
- `quality_reports/limongi_chunks/part2_synthesis.md`
- chunks detalhados em `quality_reports/limongi_chunks/part*_chunk*.md`

Tese integrada reconstruída da entrevista:

> O impeachment resultou de deserção estratégica da coalizão governista, ativada e reconfigurada pela Lava Jato, mediada pelo gatekeeping de Eduardo Cunha e pelo cálculo de que Temer oferecia melhor proteção e acesso a poder para PMDB, PSDB, PP e aliados.

Hipóteses rivais preferidas para a nova ilustração:

- Jurídico-formal: pedaladas/crime de responsabilidade.
- Ruas/opinião pública: mobilização popular anti-Dilma.
- Economia/popularidade: crise econômica e queda de aprovação.
- Personalista: Cunha como empreendedor individual do impeachment.
- Institucionalista: colapso estrutural do presidencialismo de coalizão.
- Lava Jato simples: operação anticorrupção derruba Dilma diretamente.
- Hipótese composta central: deserção intracoalizão mediada por Lava Jato, Cunha e solução Temer.

O exemplo deve mostrar por que evidências processuais com timestamps, mecanismos e restrições estruturais discriminam rivais melhor do que uma enumeração abstrata de hipóteses.

## Dados, Áudio E Transcrição

Áudios do podcast com Limongi foram baixados com `yt-dlp` e transcritos localmente com Whisper.

Arquivos grandes de mídia em `data/raw/audio/` são locais e ignorados pelo Git. Não usar Git LFS.

Documentação da coleta:

- `data/docs/SOURCES.yaml`
- `data/docs/COLLECTION_LOG.md`
- `data/checksums.sha256`
- `scripts/python/download_limongi_podcast_audio.py`
- `scripts/python/transcribe_limongi_whisper.py`

Não colar transcrição longa em respostas, commits ou relatórios. Usar a transcrição apenas como evidência de trabalho, com paráfrases e timestamps.

## Fluxo De Trabalho

- Trabalhar a partir de `paper_dados_format_quali.Rmd`.
- Não criar `paper_dados_format_quali_v8.Rmd`; versionamento deve ser por git tag quando a versão estiver pronta.
- Recompilar PDF com:

```bash
Rscript -e 'rmarkdown::render("paper_dados_format_quali.Rmd", output_format = "pdf_document", quiet = FALSE)'
```

- Não rodar `_targets::tar_make()` sem autorização.
- Commits e push exigem autorização explícita do autor.
- Ao final de mudanças substantivas, informar arquivos alterados, testes/compilação feitos e pendências.

## Próximos Passos Recomendados

1. Rodar Devil's Advocate Round 2 sobre o paper já com o estudo de caso reescrito.
2. Se o score ainda ficar abaixo de 80, corrigir os pontos substantivos indicados.
3. Recompilar `paper_dados_format_quali.pdf`.
4. Em seguida, fazer proofread e preparar commit/tag da v8.
