#' @export
print.OaxacaBlinderDecomp <- function(x) {
  cat("Oaxaca Blinder Decomposition model")
  cat("\n----------------------------------")
  cat("\nType:", x$meta$type)
  cat("\nFormula:", x$meta$formula)
  cat("\nData:", x$meta$dataset_name)
  invisible(x)
}
#' @export
summary.OaxacaBlinderDecomp <- function(x) {
  print(x)
  fml_comp <- x$meta$formula_components
  dep_var <- fml_comp$dep_var
  group_var <- fml_comp$group_var
  group1 <- x$meta$group_levels[1]
  group2 <- x$meta$group_levels[2]

  cat("\n\nDescriptives\n")
  n_tbl <- table(x$meta$data[[group_var]])
  pct_tbl <- sprintf("%.1f%%", 100 * n_tbl / sum(n_tbl))
  npct_df <- setNames(data.frame(n_tbl, pct_tbl), c("group", "n", "%n"))
  rownames(npct_df) <- as.character(npct_df$group)
  npct_df$group <- NULL
  npct_df <- npct_df[as.character(c(group1, group2)), ]
  npct_df[[paste("mean(", dep_var, ")", sep = "")]] <- c(
    round(x$gaps$EY_a, digits = 2),
    round(x$gaps$EY_b, digits = 2)
  )
  rownames(npct_df) <- paste(group_var, "==", rownames(npct_df), sep = "")
  print(npct_df)

  cat("\nGap:", round(x$gaps$gap, digits = 2))
  cat("\n% Diff:", sprintf("%.2f%%", 100 * x$gaps$pct_gap))
  cat("\n")

  overall_res <- unlist(x$overall)
  out <- data.frame(values = overall_res, pct = 100 * overall_res / x$gaps$gap)
  out$values <- round(out$values, 2)
  out$pct <- round(out$pct, 3)
  out$pct <- sprintf("%.1f%%", out$pct)
  colnames(out) <- c("coefficient", "  % of gap")

  if (!is.null(x$bootstraps)) {
    out <- cbind(out, x$bootstraps$overall)
  }

  print(out)
  invisible(x)
}
#' @export
coef.OaxacaBlinderDecomp <- function(x, ci = FALSE) {
  estimates <- x$varlevel[!(names(x$varlevel) %in% c("EX_a", "EX_b"))]

  if (ci && !is.null(x$bootstraps)) {
    # reshape estimates from wide to long
    coeftypes <- colnames(estimates)
    nterms <- nrow(estimates)
    ncoeftypes <- length(coeftypes)
    estimates <- as.data.frame(estimates)
    estimates[, "term"] <- rownames(estimates)
    estimates <- reshape(
      estimates,
      direction = "long",
      v.names = "coefficient",
      varying = 1:ncoeftypes
    )
    estimates["coef_type"] <- rep(coeftypes, rep(nterms, ncoeftypes))
    estimates <- estimates[c("coef_type", "term", "coefficient")]
    rownames(estimates) <- NULL

    # join point estimates + ci's
    bs_varlevel <- merge(estimates, x$bootstraps$varlevel)
    return(bs_varlevel)
  }

  estimates
}
