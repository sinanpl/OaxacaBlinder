% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oaxaca.R
\name{OaxacaBlinderDecomp}
\alias{OaxacaBlinderDecomp}
\title{Run a Blinder-Oaxaca decomposition}
\usage{
OaxacaBlinderDecomp(
  formula,
  data,
  type = "twofold",
  pooled = "neumark",
  baseline_invariant = FALSE,
  n_bootstraps = NULL,
  conf_probs = c(0.025, 0.975)
)
}
\arguments{
\item{formula}{A formula specifying the model as: \code{dependent_var ~
x_var1 + x_var1 + ... + x_varK | group_var}.}

\item{data}{A data frame.}

\item{type}{Type of decomposition to run: either "twofold" (the default) or
"threefold".}

\item{pooled}{\code{neumark} (the default) to exclude the group variable from
the model, or \code{jann} to include the group variable.}

\item{baseline_invariant}{Correct for the omitted baseline bias for all
factor variables?}

\item{n_bootstraps}{Bootstrap repetitions to use when calculating standard
errors.}

\item{conf_probs}{CI boundaries for bootstrapped standard errors.}
}
\value{
A list with elements \code{overall}, \code{varlevel}, \code{gaps},
  \code{meta}, and \code{bootstraps}, which can be queried with
  \code{summary()} and \code{coef()}.
}
\description{
Run a Blinder-Oaxaca decomposition
}
\examples{
twofold <- OaxacaBlinderDecomp(
  formula = real_wage ~ age + education | female,
  data = chicago_long,
  type = "twofold",
  baseline_invariant = TRUE,
  n_bootstraps = 100
)
summary(twofold)
coef(twofold)
coef(twofold, ci = TRUE)

threefold <- OaxacaBlinderDecomp(
  real_wage ~ age + education | female, chicago_long,
  type = "threefold",
  pooled = "jann",
  baseline_invariant = TRUE
)
summary(threefold)
coef(threefold)
}
