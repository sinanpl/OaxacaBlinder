#' @export
print.OaxacaBlinderDecomp <- function(x) {
  cat("Oaxaca Blinder Decomposition model")
  cat("\n----------------------------------")
  cat("\nType:", x$meta$type)
  cat("\nFormula:", x$meta$formula)
  cat("\nData:", x$meta$data)
  invisible(x)
}
#' @export
summary.OaxacaBlinderDecomp <- function(x) {
  print(x)
  fml_comp = x$meta$formula_components
  dep_var = fml_comp$dep_var
  group_var = fml_comp$group_var
  group1 = x$meta$group_levels[1]
  group2 = x$meta$group_levels[2]
  
  cat(sep = "", "\n\nmean(", dep_var, "|", group_var, "==", group1, ") = ", round(x$gaps$EY_a, digits = 2))
  cat(sep = "",   "\nmean(", dep_var, "|", group_var, "==", group2, ") = ", round(x$gaps$EY_b, digits = 2))
  cat("\n\nGap:", round(x$gaps$gap, digits = 2))
  cat("\n% Diff:", sprintf("%.2f%%", 100 * x$gaps$pct_gap))
  cat("\n")

  overall_res <- unlist(x$overall)
  out = data.frame(values = overall_res, pct = 100*overall_res / x$gaps$gap)
  out$values = round(out$values, 2)
  out$pct = round(out$pct, 3)
  out$pct = sprintf("%.1f%%", out$pct)
  colnames(out) = c("coefficient", "%")
  print(out)
  invisible(x)
}
#' @export
coef.OaxacaBlinderDecomp <- function(x) {
  x$varlevel
}
