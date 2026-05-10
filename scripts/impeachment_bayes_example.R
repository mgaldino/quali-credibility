# Illustrative posterior calculations for the impeachment example in
# paper_dados_format_quali.Rmd. These are didactic likelihoods, not empirical
# estimates.

posterior <- function(likelihoods, priors = NULL) {
  if (is.null(priors)) {
    priors <- rep(1 / nrow(likelihoods), nrow(likelihoods))
  }

  unnormalized <- priors * apply(likelihoods, 1, prod)
  unnormalized / sum(unnormalized)
}

db <- function(a, b) {
  10 * log10(a / b)
}

likelihoods_base <- rbind(
  H1_juridico = c(
    E1_aprovacao = 0.35,
    E2_juridico = 0.30,
    E3_dois_tercos = 0.40,
    E4_coalizao = 0.30,
    E5_cunha = 0.30,
    E6_temer_odebrecht = 0.35,
    E7_contraste_temer = 0.25
  ),
  H2_ruas = c(
    E1_aprovacao = 0.65,
    E2_juridico = 0.40,
    E3_dois_tercos = 0.35,
    E4_coalizao = 0.40,
    E5_cunha = 0.35,
    E6_temer_odebrecht = 0.45,
    E7_contraste_temer = 0.40
  ),
  H3_economia = c(
    E1_aprovacao = 0.85,
    E2_juridico = 0.35,
    E3_dois_tercos = 0.50,
    E4_coalizao = 0.45,
    E5_cunha = 0.35,
    E6_temer_odebrecht = 0.35,
    E7_contraste_temer = 0.45
  ),
  H4_cunha = c(
    E1_aprovacao = 0.45,
    E2_juridico = 0.65,
    E3_dois_tercos = 0.65,
    E4_coalizao = 0.70,
    E5_cunha = 0.90,
    E6_temer_odebrecht = 0.55,
    E7_contraste_temer = 0.50
  ),
  H5_lava_jato_simples = c(
    E1_aprovacao = 0.55,
    E2_juridico = 0.50,
    E3_dois_tercos = 0.50,
    E4_coalizao = 0.60,
    E5_cunha = 0.60,
    E6_temer_odebrecht = 0.85,
    E7_contraste_temer = 0.45
  ),
  H6_composta = c(
    E1_aprovacao = 0.60,
    E2_juridico = 0.75,
    E3_dois_tercos = 0.75,
    E4_coalizao = 0.80,
    E5_cunha = 0.75,
    E6_temer_odebrecht = 0.80,
    E7_contraste_temer = 0.75
  )
)

likelihoods_cunha_strong <- likelihoods_base
likelihoods_cunha_strong[
  "H4_cunha",
  c("E2_juridico", "E3_dois_tercos", "E4_coalizao",
    "E6_temer_odebrecht", "E7_contraste_temer")
] <- c(0.80, 0.75, 0.80, 0.65, 0.65)

likelihoods_lava_jato_simple_strong <- likelihoods_base
likelihoods_lava_jato_simple_strong[
  "H5_lava_jato_simples",
  c("E2_juridico", "E3_dois_tercos", "E4_coalizao",
    "E5_cunha", "E7_contraste_temer")
] <- c(0.65, 0.65, 0.70, 0.70, 0.60)

results <- list(
  base = posterior(likelihoods_base),
  cunha_mais_forte = posterior(likelihoods_cunha_strong),
  lava_jato_simples_mais_forte = posterior(likelihoods_lava_jato_simple_strong)
)

print(round(do.call(cbind, results), 3))
cat(
  "\nBase odds H6/H4 in dB:",
  round(db(results$base["H6_composta"], results$base["H4_cunha"]), 1),
  "\n"
)
cat(
  "Base odds H6/H5 in dB:",
  round(db(results$base["H6_composta"], results$base["H5_lava_jato_simples"]), 1),
  "\n"
)
