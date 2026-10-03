# Verificações aritméticas da adjudicação, sem alteração do script canônico.
# Executar da raiz: Rscript quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3/checks.R
out_dir <- "quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3"
sink(file.path(out_dir, "checks_output.txt"))
source(file.path(out_dir, "artifacts/impeachment_bayes_example_baseline.R"))

weights <- rep(1 / nrow(likelihoods_base), nrow(likelihoods_base)) *
  apply(likelihoods_base, 1, prod)
names(weights) <- rownames(likelihoods_base)
cat("\nPesos não-normalizados:\n")
print(weights)
baseline_order <- names(sort(weights, decreasing = TRUE))
cat("\nRanking de base:\n")
print(baseline_order)

remove_checks <- lapply(names(weights), function(removed) {
  remaining <- setdiff(names(weights), removed)
  normalized <- weights[remaining] / sum(weights[remaining])
  new_order <- names(sort(normalized, decreasing = TRUE))
  expected_order <- baseline_order[baseline_order %in% remaining]
  stopifnot(identical(new_order, expected_order))
  ratios_before <- outer(weights[remaining], weights[remaining], "/")
  ratios_after <- outer(normalized, normalized, "/")
  error <- max(abs(ratios_before - ratios_after))
  stopifnot(error < 1e-10)
  data.frame(removida = removed, top_1 = new_order[1],
             ranking_restrito_preservado = TRUE,
             erro_maximo_odds = error)
})
cat("\nLeave-one-rival-out preserva ranking restrito e odds:\n")
print(do.call(rbind, remove_checks), row.names = FALSE)

cat("\nLeave-one-evidence-out sob o modelo independente ilustrativo:\n")
evidence_checks <- lapply(colnames(likelihoods_base), function(removed) {
  changed <- likelihoods_base[, setdiff(colnames(likelihoods_base), removed), drop = FALSE]
  p <- posterior(changed)
  data.frame(removida = removed, top_1 = names(which.max(p)),
             posterior_H6 = unname(p["H6_composta"]))
})
print(do.call(rbind, evidence_checks), row.names = FALSE)

# A uniformidade depende da partição. Mesma evidência para todas as variantes.
coarse_likelihood <- matrix(c(0.6, 0.4), ncol = 1,
  dimnames = list(c("A", "B"), "E"))
fine_likelihood <- matrix(c(0.6, 0.6, 0.6, 0.6, 0.4), ncol = 1,
  dimnames = list(c("A1", "A2", "A3", "A4", "B"), "E"))
coarse <- posterior(coarse_likelihood)
fine_uniform <- posterior(fine_likelihood)
fine_preserved <- posterior(fine_likelihood, c(rep(1 / 8, 4), 1 / 2))
cat("\nPartição de prioris:\n")
print(c(A_particao_original = coarse["A"],
        A_uniforme_nova_particao = sum(fine_uniform[1:4]),
        A_massa_original_preservada = sum(fine_preserved[1:4])))
stopifnot(abs(coarse["A"] - 0.6) < 1e-10,
          abs(sum(fine_uniform[1:4]) - 6 / 7) < 1e-10,
          abs(sum(fine_preserved[1:4]) - coarse["A"]) < 1e-10)

# P(H6|E)/P(H4|E) = prior odds H6/H4 multiplicada pelo Bayes factor.
joint_likelihood <- apply(likelihoods_base, 1, prod)
bf_h6_h4 <- joint_likelihood["H6_composta"] / joint_likelihood["H4_cunha"]
cat("\nBF H6/H4 e fronteira de prior odds para inverter preferência par a par:\n")
print(c(BF_H6_H4 = bf_h6_h4, prior_odds_H6_H4_at_tie = 1 / bf_h6_h4))

# Contraexemplo à soma de probabilidades = 1 para rivais genericamente definidas.
cat("\nRivais não exaustivas: P(A)=0,4, P(B)=0,3, P(outro)=0,3; soma A+B=0,7.\n")
cat("Rivais sobrepostas: P(A)=0,6, P(B)=0,6, P(A intersecção B)=0,2; soma A+B=1,2.\n")
cat("\nPASS: todas as verificações aritméticas especificadas.\n")
sink()
