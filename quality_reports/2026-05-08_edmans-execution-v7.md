# Parecer de Execution (Framework Edmans 2025) — paper_dados_format_quali_v7.Rmd

**Avaliador**: Claude (Opus 4.7) atuando como editor de top journal de CP
**Data**: 2026-05-08
**Manuscrito**: paper_dados_format_quali_v7.Rmd (Manoel Galdino, USP)
**Tipo**: Nota de pesquisa teorica/metodologica
**Status anterior**: Rejeitado em BPSR; editor convidou ressubmissao

---

## Score: 5.5/10

## Tipo de paper: Teorico/Metodologico (nota de pesquisa)

## Resumo da estrategia argumentativa

O paper parte de duas premissas — (P1) a "revolucao da credibilidade" tornou a *identificacao causal* ortogonal a *inferencia estatistica*; (P2) a inferencia Bayesiana permite quantificar incerteza com qualquer $n$ — e tira tres conclusoes interligadas: (C1) nao ha diferenca *intrinseca* entre quali e quanti em validade interna; (C2) duas abordagens recentes (process tracing Bayesiano de Fairfield-Charman; queries causais de Humphreys-Jacobs) operacionalizam isso; (C3) ambas as tradicoes estao igualmente limitadas em validade externa, mas a quali pode ate ser superior se teorizar mecanismos que delimitem escopo. A cadeia central pode ser parafraseada como: identificacao depende do desenho, nao do $n$; logo, se quali tem desenho crivel, e indistinguivel de quanti em validade interna; logo, criticas baseadas em $n$ sao mal-direcionadas.

## Principio "Argumentos vs. Evidencia argumentativa"

**A cadeia geral e plausivel, mas as premissas operacionais que precisam sustenta-la ficam sub-justificadas em pontos criticos.** O argumento conceitual (identificacao $\neq$ inferencia estatistica) esta correto e bem ancorado na literatura. O problema esta no *salto* de (P1)+(P2) para (C1): o paper precisa demonstrar que metodos qualitativos *de fato podem* satisfazer condicoes de identificacao, nao apenas que *se* satisfizessem, seriam equivalentes. Esse salto e parcialmente coberto pela secao sobre process tracing Bayesiano e queries causais, mas o tratamento da *condicao de ignorability* (linha 73) — pedra angular da identificacao — nao e nunca aplicado de volta a contextos qualitativos. Em outras palavras: o paper assume que o desenho qualitativo pode ser "crivel" (linha 47, 109) sem demonstrar *como*. Isso aproxima o argumento de uma tautologia condicional ("*se* a quali resolveu identificacao, entao sua validade interna iguala a quanti") cujo antecedente e exatamente o ponto controverso.

## Avaliacao por dimensao

### T.1 Distancia premissas-conclusoes [Questionavel]

A distancia premissas-conclusoes e *parcialmente* tautologica em pontos centrais:

- **Linha 81**: "supondo que a pesquisa qualitativa resolveu o problema do desenho de pesquisa, nos perguntar: como fazer inferencia em amostras pequenas?". Isso *supoe* o que precisa ser demonstrado. O paper argumenta que credibility revolution mostra que identificacao e ortogonal a $n$ — ok. Mas a pergunta empirica relevante e: *como, na pratica, pesquisa qualitativa atinge identificacao?* Essa questao e contornada.
- **Linhas 211-213**: "Se, como a literatura metodologica argumenta, pesquisa qualitativa nao tem maiores problemas em atingir validade interna, entao nao ha qualquer diferenca com relacao a esse aspecto". Aqui o "se" e carregando todo o trabalho — e a propria literatura citada (Brady-Collier, Seawright, Slater-Ziblatt) *nao* concede isso de modo trivial. Seawright explicitamente argumenta que comparativos qualitativos sao "exceptionally weak tools for causal inference" (linha 43). O autor parafraseia essa critica como ja superada, mas a passagem nao e bem fundamentada.
- **Linha 113**: "Essas solucoes deixam claro e sem sombra de duvidas o argumento do trabalho de que o problema de identificacao e completamente distinto e ortogonal ao problema da inferencia estatistica." Esta e uma conclusao excessivamente forte. Apresentar duas abordagens que *combinam* identificacao + inferencia Bayesiana nao demonstra que sao "ortogonais" — pelo contrario, demonstra que ambas sao necessarias e operam em tandem. Confunde-se "logicamente distintas" (correto) com "ortogonais" (palavra mais forte que nao se segue).

A reformulacao construtiva: o argumento *poderia* ser sustentado se o autor admitisse que (a) sem hipoteses rivais bem-estruturadas a quali nao identifica; (b) com hipoteses rivais bem-estruturadas, ela ainda *nao* elimina viesses de variavel omitida (o proprio autor admite isso na linha 201-203, em tensao com a tese central).

### T.2 Parcimonia [Adequada com problemas pontuais]

**Pontos positivos**: A estrutura geral (introducao $\to$ revolucao da credibilidade $\to$ inferencia em $n$ pequeno $\to$ duas abordagens $\to$ generalizacao) e logica e cada secao tem funcao identificavel.

**Redundancias**:
- Linhas 211-213, 216, 218, 243: a mesma tese ("nao ha diferenca intrinseca em validade interna/externa entre quali e quanti") e repetida pelo menos quatro vezes em formas ligeiramente diferentes nas secoes 5 e Consideracoes Finais.
- Linhas 73 (ignorability) e 216 (re-mencao a identificacao) cobrem o mesmo terreno conceitual sem aprofundamento adicional.

**Falta de justificacao em elos da cadeia**:
- A passagem sobre INUS/SUIN (linhas 75-77) e tecnica e *deslocada*. Nao e claro o que ela acrescenta a tese central — parece um tecnicismo inserido para mostrar que o autor sabe traduzir notacoes, mas nao avanca o argumento.
- A subsecao "Fundamentos da Probabilidade Bayesiana" (linhas 95-107) e didatica mas pesada para uma nota de pesquisa. Reduzir.

**O que e premissa / o que e conclusao**: na maior parte do paper isso e claro. Mas em algumas passagens a premissa e suposta como conclusao consensual (ex.: linha 47, "o template quantitativo ao qual boa parte dos qualitativistas estao respondendo foi, em grande parte, rejeitado pelo proprio desenvolvimento metodologico"). Essa e uma tese forte que requer evidencia, nao deveria operar como premissa.

### T.3 Caminho causal / operacionalizacao [Vaga]

Para um paper metodologico, a operacionalizacao das recomendacoes e o teste pratico fundamental. Aqui o paper *enfraquece*:

- **Linha 89**: "estudos qualitativos bem desenhados geralmente se concentram em contextos onde o sinal e forte". *Como* o leitor identifica que esta em um contexto de sinal forte? O paper nao oferece criterios.
- **Linha 232**: "teorizar explicitamente sobre os mecanismos causais que definem os limites do escopo de aplicacao, fornecendo criterios claros e testaveis para avaliar a validade da generalizacao". Esta e a recomendacao central, mas e quase tautologica — "teorize bem o seu escopo" e um conselho que ninguem rejeitaria. Como, *operacionalmente*, o pesquisador faz isso? O paper aponta para Skocpol como exemplo, mas nao destila um procedimento.
- **Linha 234**: o exemplo Skocpol ("paises agrarios, nao-colonizados recentemente e com estados proto-burocraticos enfrentando adversarios economicamente desenvolvidos") e ilustrativo, mas e *atribuido* a Skocpol post-hoc. Skocpol nao apresentou esses criterios como condicoes formais de transportabilidade. O paper *projeta* a estrutura desejavel sobre o trabalho classico.

**Confusao entre objetos teoricos**: existe uma deslizada conceitual entre "validade interna", "identificacao causal" e "ausencia de viesses". O autor por vezes os trata como sinonimos (linha 211: "atingir validade interna" = identificacao); outras vezes como conceitos distintos (linha 220: validade interna como adequacao a *pergunta de pesquisa* na amostra). A definicao Campbell de validade interna (linha 220) nao e identica a identificacao causal de Rubin/Pearl. O paper precisa decidir qual usa e ser consistente.

### Uso da literatura citada [Algumas imprecisoes]

**KKV (King, Keohane, Verba 1994)**: representado de forma razoavel como unificador do paradigma quantitativo, mas a leitura na linha 47 ("o template quantitativo ao qual boa parte dos qualitativistas estao respondendo foi, em grande parte, rejeitado") e uma *interpretacao* forte. KKV foi criticado *interno* a literatura quali; a credibility revolution criticou a *pratica regressional* quanti. Sao criticas distintas. Conflar as duas e questionavel.

**Slater-Ziblatt 2013**: representacao razoavel (linhas 59, 161) — comparacao controlada como ferramenta de *eliminacao entre hipoteses rivais*. Mas o autor parece superinterpretar: Slater-Ziblatt *nao* afirmam que isso resolve viesses de variavel omitida, e o paper oscila sobre se essa abordagem resolve ou nao (linha 59: "nao e claro em que isso permite superar"; linha 165: implica que sim "contorna o problema"; linha 203: "nao necessariamente garantem que o efeito causal e sem vies"). **Esta inconsistencia e um problema sustancial** — as linhas 165 e 203 dizem coisas opostas em distancia de duas paginas.

**Fairfield-Charman 2022**: representacao precisa do framework (Bayes factors, decibeis). A critica do autor sobre "redefinicao continua de escopo" (linhas 224-232) e *parcialmente* justa, mas exagerada — F&C nao recomendam *redefinir continuamente* o escopo a cada nova evidencia; eles oferecem condicoes de escopo como elemento explicito da hipotese. A critica do autor confunde a posicao dos autores com uma *consequencia operacional indesejavel* dela. Isso *pode* ser uma critica valida, mas precisa ser enquadrada como tal.

**Humphreys-Jacobs 2023**: representacao razoavel das categorias (adverso/benefico/cronico/destinado, linha 175) e das queries causais. Bom.

**Skocpol 1979**: usada como exemplo de "boa pratica" (linhas 162-165, 234). Esta e a apropriacao mais *retorica* do paper. Skocpol nao formulou seu trabalho em termos Bayesianos nem de hipoteses rivais explicitas; ela conduziu Mill's methods comparativo. Atribuir-lhe ex-post a estrutura de "comparacao de posterior odds" e generosidade interpretativa, nao argumento. Funciona como exemplo retorico mas o leitor critico nota.

**Mahoney-Goertz 2006 (linha 57)**: o contraponto via controle sintetico (Abadie) e *fragil*. Mahoney-Goertz argumentam sobre *culturas* de pesquisa, nao sobre impossibilidade tecnica de estudo de caso quanti. Controle sintetico e uma tecnica quanti que requer dados longitudinais — nao e o que Mahoney-Goertz tinham em mente. Paragrafo precisa de revisao.

**Leamer 1983**: corretamente posicionado como precursor (linha 63).

**Pearl-Bareinboim**: citacao apropriada (linha 222). Definicao formal de transportabilidade esta correta.

### Coerencia conceitual [Adequada com tensoes]

**Forca**: o paper *acerta* a distincao central entre identificacao e inferencia estatistica. Esse e o fio condutor e ele e, no nucleo, correto.

**Tensoes/incoerencias**:

1. **Validade interna**: usada em pelo menos *tres* sentidos no paper:
   - (a) Identificacao (linhas 49, 211)
   - (b) Adequacao do desenho a pergunta de pesquisa na amostra (linha 220, citando Mcdermott)
   - (c) Sinal de credibilidade da inferencia (linha 89: "validade interna, que estao relacionadas as condicoes formais de identificacao")

   Estas nao sao identicas. Identificacao e uma propriedade *populacional*; validade interna pode ser *amostral*. O paper precisa um de:
   - (i) declarar que esta usando os termos como sinonimos e justificar; ou
   - (ii) distinguir e usar consistentemente.

2. **Validade externa**: o paper afirma na linha 209 que "A literatura qualitativa metodologica tem feito confusao sobre os conceitos de validade interna, misturando generalizacao de uma amostra para a populacao como validade externa". Isso e uma *acusacao*, mas precisa ser substanciada — *qual* trecho de KKV ou Brady-Collier confunde os conceitos? E mais grave: o *proprio* autor na linha 49 fala que a "incerteza chave nos estudos qualitativos causais e a da variabilidade nos resultados potenciais" (que e uma questao de inferencia, nao de validade interna). Pedras de vidro.

3. **"Ortogonalidade"** (linha 113, 243): identificacao e inferencia estatistica nao sao *ortogonais* — sao *etapas distintas em uma pipeline*. Sao *logicamente separaveis* mas pratica e teoricamente interdependentes. "Ortogonal" tem conotacao especifica em estatistica (correlacao zero) que aqui nao se aplica.

4. **"Esparsividade"** (linha 59, 203): o paper invoca Double LASSO/regularizacao como metafora para hipoteses rivais. A analogia e *evocativa* mas inexata — Double LASSO e uma tecnica de selecao de variaveis em alta dimensao com garantias assintoticas, nao um procedimento de comparacao Bayesiana de hipoteses. Usar esse termo solto enfraquece a precisao.

### Argumento sinal-ruido [Parcialmente convincente]

Linhas 87-93 contem o nucleo do argumento. Avaliacao:

**O que funciona**: e correto que detectabilidade depende de relacao sinal-ruido, nao apenas de $n$. Esse e um ponto legitimo da estatistica padrao (poder estatistico). E correto que estudos qualitativos *podem* selecionar contextos de sinal forte.

**O que nao funciona**:

1. **A passagem da linha 89 ("estudos qualitativos bem desenhados *geralmente* se concentram em contextos onde o sinal e forte") e empirica — e nao e justificada por evidencia**. E uma afirmacao otimista. Igualmente plausivel: estudos qualitativos *escolhem casos disponiveis*, nao casos com sinal forte. Sem evidencia bibliometrica ou pelo menos exemplos sistematicos, isso e *wishful thinking*.

2. **Conexao com credibility revolution**: o paper *nao* fecha o circuito. Sinal-ruido e uma propriedade da *inferencia estatistica*; credibility revolution e sobre *identificacao*. O paragrafo das linhas 87-93 esta inserido dentro de uma secao sobre *identificacao*, mas trata de *inferencia*. O autor poderia argumentar: "alem de identificacao, ainda ha a questao do poder estatistico, e Bayes ajuda com isso". Mas o paragrafo e ambiguo.

3. **A afirmacao da linha 89 ("nada dizem sobre a validade interna, que estao relacionadas as condicoes formais de identificacao")** e um non-sequitur dentro do paragrafo. O autor afirma que selecionar contextos de sinal forte resolve inferencia mas nao identificacao — *correto* — mas a frase e tao curta que o leitor sai sem saber se isso e um aviso, uma concessao ou uma divagacao.

4. **A formulacao "sinal-ruido" e usada metaforicamente, nunca formalmente**. Em estatistica, sinal-ruido tem definicao precisa (effect size / SE, ou effect size / sigma). O paper usa o termo solto. Fica vago.

**Resumo**: argumento *gestalt* defensavel mas *operacionalmente* fraco. Nao se conecta de maneira clara com a tese central.

### Critica a transportabilidade de F&C [Justa mas vaga]

Linhas 224-236. Avaliacao:

**O que funciona**:
- A intuicao basica e correta: *se* sempre podemos restringir o escopo, generalizacao se torna trivial. Existe um problema potencial em F&C nesta direcao.
- O exemplo (linha 226) — "evidencia da Filipinas apenas sera incapaz de informar ou diferenciar entre as hipoteses" — e bem construido.
- A conexao com transportabilidade de Pearl-Bareinboim (linha 222) e correta.

**O que nao funciona**:
- **A acusacao e exagerada**. Linha 228: "torna virtualmente impossivel estabelecer conclusoes generalizaveis estaveis". F&C nao recomendam *redefinir continuamente* — recomendam que escopo seja *parte* da hipotese. A diferenca e crucial. O autor nao representa a posicao de F&C de forma maximamente caridosa.
- **A solucao alternativa proposta e *vaga* e nao e claramente diferente do que F&C ja fazem**. Linha 232: "teorizar explicitamente sobre os mecanismos causais que definem os limites do escopo de aplicacao". Isso e *exatamente* o que F&C tentam fazer ao tornar o escopo parte da hipotese. A diferenca real entre as duas posicoes nao fica clara.
- **O exemplo Skocpol (linha 234) e atribuido*post-hoc*** uma estrutura formal que ela mesma nao explicitou desta maneira. O autor *interpreta* Skocpol como tendo teorizado mecanismos de escopo. Isso e plausivel como reconstrucao racional, mas nao e o argumento que Skocpol fez. Nao e *exemplo* de boa pratica do criterio do autor; e *projecao* do criterio sobre a obra.
- **Operacionalidade**: o leitor que termina a secao 5 *nao sabe* como, na pratica, formular criterios de escopo de modo a evitar o problema que o autor acusa F&C. A recomendacao e abstrata.

**Veredicto**: a critica tem um nucleo legitimo (perigo de scope shifting ad hoc), mas: (a) caricatura F&C, (b) nao oferece criterio operacional alternativo, (c) usa Skocpol como exemplo de modo retorico nao demonstrativo.

## Veredicto geral sobre execution

O paper tem um *argumento central correto e importante*: a distincao entre identificacao e inferencia estatistica e fundamental, foi obscurecida em parte do debate quali-quanti, e re-articula-la abre espaco para reabilitar pesquisa qualitativa causal. Esse argumento merece publicacao.

Contudo, a *execucao* desse argumento sofre de tres problemas estruturais que motivam o score 5.5:

1. **Tautologias condicionais nao reconhecidas**: o paper repetidamente assume que quali "resolveu o desenho de pesquisa" para concluir que quali tem validade interna equivalente a quanti. *Como* a quali resolve o desenho e *exatamente* a questao em disputa.

2. **Inconsistencia interna em pontos cruciais**: linhas 165 e 203 dizem coisas conflitantes sobre se hipoteses rivais resolvem ou nao viesses de variavel omitida. Validade interna e usada em tres sentidos. "Ortogonalidade" e usada de forma metaforica e literal.

3. **Operacionalidade fraca**: as recomendacoes praticas (selecione casos com sinal forte; teorize escopo) sao corretas mas vagas. Um leitor que queira *aplicar* o argumento nao sai com criterios acionaveis.

Adicionalmente, o uso de Skocpol como exemplo recorrente e *retorico mais que demonstrativo*; a critica a Fairfield-Charman e em parte uma caricatura; e a passagem sobre sinal-ruido nao se conecta solidamente com a credibility revolution. Para uma nota de pesquisa em revista de CP brasileira de bom nivel, e *publicavel apos revisoes substantivas*. Para top journal internacional (APSR, AJPS), precisaria reescrever boa parte das secoes 3.5 e 5.

## Sugestoes construtivas

1. **Resolver a tautologia condicional**: Em vez de assumir que quali "resolveu o desenho" e seguir, dedicar uma subsecao explicita a *como, sob quais condicoes*, desenhos qualitativos satisfazem (ou aproximam) ignorability/identificacao. As duas abordagens (process tracing Bayesiano, queries causais) deveriam ser apresentadas como *propostas* de solucao, com seus *propios pressupostos*, nao como demonstracao da tese.

2. **Resolver inconsistencia das linhas 165 vs 203**: Decidir se hipoteses rivais (a) eliminam ou (b) atenuam ou (c) re-enquadram o problema de variavel omitida. Defender uma tese e ser consistente.

3. **Usar "validade interna" em UM sentido apenas**: Adotar a definicao Pearl/Rubin (identificacao) ou a Campbell/Mcdermott (adequacao do desenho a pergunta na amostra) e ser explicito.

4. **Substituir "ortogonal" por "logicamente distinto" ou "separavel"**: Mais preciso, evita conotacao estatistica enganosa.

5. **Operacionalizar sinal-ruido**: ou (a) formalizar (effect size/SE como criterio para selecao de casos qualitativos), ou (b) remover o argumento, que solto enfraquece. Considerar referencia a literatura de power analysis em estudos de caso.

6. **Substituir Skocpol como "exemplo" por exemplo construido pelo autor**: Construir um exemplo *originalmente* no formato proposto (hipoteses rivais com escopo teorizado, posteriores comparados) seria mais convincente que reler Skocpol post-hoc.

7. **Tratar Fairfield-Charman com maior caridade interpretativa**: Distinguir explicitamente (a) o que F&C *recomendam* de (b) o que e *consequencia operacional indesejavel* de seguir essa recomendacao sem cuidado. Apresentar o problema de scope shifting como *risco a ser mitigado*, nao como falha intrinseca do framework.

8. **Reduzir o material didatico** (Bayes basics linhas 95-107, INUS/SUIN linhas 75-77) para ganhar espaco para os pontos acima. Para uma nota de pesquisa, esses fundamentos podem ser passados em uma frase com referencia.

9. **Substanciar a acusacao da linha 209**: ou citar trechos especificos de KKV/Brady-Collier/Slater-Ziblatt confundindo validade interna com generalizacao, ou reformular a frase de modo mais qualificado.

10. **Esclarecer se "credibility revolution rejeitou KKV"** (linha 47): credibility revolution e KKV criticam coisas distintas. Esclarecer relacao histórica em vez de implicar substituicao.

11. **Revisar linguagem em pontos retoricos fortes**: "deixam claro e sem sombra de duvidas" (linha 113), "completamente ortogonal", "sem fundamento solido" (linha 218) — top journals desconfiam de hedging fraco. Linguagem mais qualificada ajuda.

12. **Considerar adicionar uma secao explicita sobre limites**: o paper se beneficiaria de uma "Limitacoes" curta em que o autor reconheca: (a) que o argumento depende de quali poder satisfazer ignorability — o que e contestado; (b) que metodos Bayesianos exigem treinamento que muitos qualitativistas nao tem; (c) que generalizacao via mecanismos teorizados ainda nao tem demonstracao empirica. Isso *fortalece* o paper, nao o enfraquece.
