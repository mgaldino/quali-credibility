# Verificação independente da implementação; executar da raiz do projeto.
project_root <- normalizePath(".")
review_dir <- file.path(project_root,
  "quality_reports/adjudication/parecer-chatgpt/3d1b06bdb1e3")
verification_tmp <- tempfile("quali-verification-")
dir.create(verification_tmp)
setwd(verification_tmp)
sink(file.path(review_dir, "verification_checks_output.txt"))
baseline_env <- new.env()
current_env <- new.env()
source(file.path(review_dir, "artifacts/impeachment_bayes_example_baseline.R"),
       local = baseline_env)
source(file.path(project_root, "scripts/impeachment_bayes_example.R"),
       local = current_env)
stopifnot(
  identical(baseline_env$likelihoods_base, current_env$likelihoods_base),
  identical(baseline_env$likelihoods_cunha_strong,
            current_env$likelihoods_cunha_strong),
  identical(baseline_env$likelihoods_lava_jato_simple_strong,
            current_env$likelihoods_lava_jato_simple_strong),
  identical(baseline_env$results, current_env$results)
)

independent_loeo <- vapply(seq_len(ncol(baseline_env$likelihoods_base)),
  function(k) {
    weights <- apply(baseline_env$likelihoods_base[, -k, drop = FALSE], 1, prod)
    weights / sum(weights)
  }, numeric(nrow(baseline_env$likelihoods_base)))
stopifnot(max(abs(independent_loeo - current_env$leave_one_evidence_out)) < 1e-12)
joint <- apply(baseline_env$likelihoods_base, 1, prod)
independent_bf <- joint["H6_composta"] / joint["H4_cunha"]
independent_threshold <- 1 / independent_bf
independent_range <- range(independent_loeo[6, ])
stopifnot(round(independent_bf, 3) == 3.689,
          round(independent_threshold, 3) == 0.271,
          identical(round(independent_range, 3), c(0.568, 0.652)))

expected_outputs <- list(
  "posteriors.csv" = data.frame(
    hipotese = rownames(baseline_env$likelihoods_base),
    do.call(cbind, baseline_env$results)),
  "leave_one_evidence_out.csv" = current_env$loeo_summary,
  "prior_odds_thresholds.csv" = current_env$prior_thresholds
)
for (name in names(expected_outputs)) {
  on_disk <- read.csv(file.path(project_root,
    "data/derived/impeachment_bayes_example", name),
    stringsAsFactors = FALSE)
  comparison <- all.equal(on_disk, expected_outputs[[name]],
    check.attributes = FALSE, tolerance = 1e-12)
  stopifnot(isTRUE(comparison))
}
cat("\nPASS: matriz e cenários idênticos; base numérica idêntica; LOEO,")
cat(" BF e prior odds verificados independentemente; três CSV correspondem.\n")
print(c(BF_H6_H4 = independent_bf,
        threshold_H6_H4 = independent_threshold,
        LOEO_min = independent_range[1], LOEO_max = independent_range[2]))
sink()
setwd(project_root)
