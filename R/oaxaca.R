#' Create a \code{.do} file to run a command and save estimates
#'
#' Internal function to write a \code{.do} file that will run a
#' command and export a file suitable for reading with
#' \code{read_stata_estimates}.
#'
#' @param command String of Stata estimation command.
#' @param do_path Path to \code{.do} file to save.
#' @param dta_file Data file for Stata to use in same directory as
#'   \code{.do} file.
#' @param est_file Filename (without extension) Stata to save estimate results in same
#'   directory as \code{.do} file.
#'
#' @return Nothing, called only for its side effect of writing the
#'   \code{.do} file.
make_decomp_dofile <- function(command, do_path, dta_file, est_file) {
  do_file_text <-
    c(
      "clear all",
      paste("use", basename(dta_file)),
      command,
      paste0(
        "etable, cstat(_r_b, nformat(%8.0g)) export(",
        basename(est_file),
        ".xlsx, replace)"
      )
    )

  cat(do_file_text, file = do_path, sep = "\n")

  message(
    paste0(
      basename(do_path), " saved to ", dirname(do_path),
      ".\nRun `do ", basename(do_path),
      "` in Stata to produce new test baselines."
    )
  )
  invisible(command)
}

#' Read estimates from Stata's Excel export
#'
#' This internal function reads the estimates produced by running
#' Jann's \code{oaxaca} command in Stata.  The estimates should be
#' exported to a file with extension \code{xlsx} or \code{xls}
#' using Stata using the \code{etable} command, with the option
#' \code{cstat(_r_b)} to ensure that standard errors are not
#' included.
#'
#' @param path The path to the Excel file.
#'
#' @return A data frame with the same rows and columns as would be
#'   produced in the \code{varlevel} element of the output of
#'   \code{OaxacaBlinderDecomp} (though the rows might be in a
#'   different order).
read_stata_estimates <- function(path) {
  stata_estimates <-
    readxl::read_excel(
      path = path,
      col_names = c("name", "value"),
      col_types = c("text", "numeric"),
      skip = 1
    )

  drops <-
    c(
      "group_1", "group_2", "difference", "endowments",
      "coefficients", "interaction", "N"
    )

  estimates <-
    stata_estimates[!(stata_estimates$name %in% drops), ]
  n_x <- (nrow(estimates) - 1) / 3
  endowments <- estimates[1:n_x, ]
  coefficients <- estimates[(n_x + 1):(2 * n_x + 1), ]
  interactions <- estimates[(2 * n_x + 2):nrow(estimates), ]

  endowments_padded <- rbind(endowments, list("(Intercept)", 0))
  interactions_padded <- rbind(interactions, list("(Intercept)", 0))

  varlevel <-
    cbind(
      endowments_padded[2],
      coefficients[2],
      interactions_padded[2]
    )
  rownames(varlevel) <- endowments_padded[[1]]
  colnames(varlevel) <-
    c("endowments", "coefficients", "interaction")
  varlevel_intfirst <-
    varlevel[c(nrow(varlevel), 1:(nrow(varlevel) - 1)), ]
  varlevel_intfirst
}

parse_formula <- function(formula) {
  # convert to character and split in depvar, indepvar and groupvar
  fml_str <- as.character(formula)
  lhs <- fml_str[2]
  rhs <- fml_str[3]
  rhs <- strsplit(
    x = gsub("\\s+", "", rhs),
    split = "|",
    fixed = TRUE
  )[[1]]
  group_var <- rhs[2]
  rhs <- rhs[1]

  # returns list of character strings
  list(
    dep_var = lhs,
    indep_var = rhs,
    group_var = group_var
  )
}

modify_group_var_to_dummy <- function(data, formula) {
  # parse fml for group/dep var
  fml_comp <- parse_formula(formula)
  group_var <- fml_comp$group_var
  dep_var <- fml_comp$dep_var

  stopifnot("Grouping variable should have 2 unique values" = {
    g <- data[[group_var]]
    length(unique(g)) == 2
  })

  # modify group var such that group0 (reference) is the group that has a higher dep_var avg
  dep_var_avgs <- aggregate(data[[dep_var]], list(gr = data[[group_var]]),
    FUN =
      mean, na.rm = TRUE
  )
  dep_var_avgs <- dep_var_avgs[order(dep_var_avgs$x, decreasing = TRUE), ]

  group1 <- dep_var_avgs$gr[1] # higher dep_var avg
  group2 <- dep_var_avgs$gr[2] # lower dep_var avg

  # modify data; 0 represent the reference (higher depvar group)
  data[[group_var]] <- ifelse(data[[group_var]] == group1, 0, 1)

  # return with levels specification for metainfo
  list(
    data = data,
    group_levels = c(group1, group2)
  )
}

calculate_gap <- function(y_a, y_b) {
  EY_a <- mean(y_a, na.rm = TRUE)
  EY_b <- mean(y_b, na.rm = TRUE)

  gap <- EY_a - EY_b
  pct_gap <- gap / EY_a

  list(
    gap = gap,
    pct_gap = pct_gap,
    EY_a = EY_a,
    EY_b = EY_b
  )
}

assemble_model <- function(formula, data) {
  fml_comp <- parse_formula(formula)
  # Get DV as it will be in model
  y <- model.frame(formula, data)[[fml_comp$dep_var]]
  # Expand matrix manually to keep all factor levels
  modmat <- model.matrix(formula, data)
  # Save original formula terms
  terms <- terms(formula)
  # Fit w/ all levels and clean names except for intercepts
  fit <- lm(y ~ . - 1, data = data.frame(y, modmat))

  list(y = y, modmat = modmat, terms = terms, fit = fit)
}

fit_models <- function(formula, data) {
  # get formula components
  fml_comp <- parse_formula(formula)

  # Convert character cols to factors
  data <-
    lapply(
      data,
      function(x) if (is.character(x)) as.factor(x) else x
    ) |>
    data.frame()

  # filter datasets for group a/b
  idx <- data[[fml_comp$group_var]] == 0
  data_a <- data[idx, ]
  data_b <- data[!idx, ]

  # construct formulas
  fml_reg <- paste(fml_comp$dep_var, "~", fml_comp$indep_var)

  # currently; pooled reg without group ind as suggested by Neumark (1988)
  fml_reg_pooled_neumark1988 <-
    paste(fml_comp$dep_var, "~", fml_comp$indep_var)
  fml_reg_pooled_jann2008 <-
    paste(
      fml_comp$dep_var,
      "~",
      fml_comp$indep_var,
      "+",
      fml_comp$group_var
    )

  # convert to formula object
  fml_reg <- as.formula(fml_reg)
  fml_reg_pooled_neumark1988 <-
    as.formula(fml_reg_pooled_neumark1988)
  fml_reg_pooled_jann2008 <- as.formula(fml_reg_pooled_jann2008)

  model_args <- list(
    group_a = list(fml_reg, data_a),
    group_b = list(fml_reg, data_b),
    pooled_neumark1988 = list(fml_reg_pooled_neumark1988, data),
    pooled_jann2008 = list(fml_reg_pooled_jann2008, data)
  )
  models <-
    lapply(model_args, function(x) assemble_model(x[[1]], x[[2]]))
  models
}

extract_betas_EX <- function(mod, baseline_invariant) {
  modmat_orig <- mod$modmat
  modmat <- model.matrix(mod$fit)
  betas <- coef(mod$fit)
  betas[is.na(betas)] <- 0

  # if baseline variant;
  # identify factor variables and associated dummy indicators
  # apply gardeazabal2004 ommitted baseline correction per set of dummy variables
  if (baseline_invariant) {
    # identify factor terms
    factor_variables <- names(attr(modmat_orig, "contrasts"))

    terms <- attr(mod$terms, "term.labels")
    term_assignments_i <- attr(modmat_orig, "assign") # intercept = 0; gets removed
    term_assignments <- terms[term_assignments_i]

    # for each dummy encoded term; adjust the betas; save and add a baseline coef to beta and modmat
    for (factor_var in factor_variables) {
      # beta adjustment
      dummy_index_in_beta <- 1 + which(term_assignments == factor_var)
      k <- length(dummy_index_in_beta) + 1
      c <- sum(betas[dummy_index_in_beta]) / k
      betas[1] <- betas[1] + c
      betas[dummy_index_in_beta] <- betas[dummy_index_in_beta] - c

      # add baseline level
      betas[length(betas) + 1] <- -c
      baseline_name <- paste(factor_var, ".baseline", sep = "")
      names(betas)[length(betas)] <- baseline_name

      # add baseline indicator to modmat
      baseline_indicator <- ifelse(rowSums(modmat[, dummy_index_in_beta,
        drop =
          FALSE
      ]) == 0, 1, 0)
      modmat <- cbind(modmat, baseline_indicator)
      colnames(modmat)[ncol(modmat)] <- baseline_name
    }
  }

  # Fix intercept renaming
  colnames(modmat)[1] <- colnames(model.matrix(mod$fit))[1]
  EX <- apply(modmat, mean, MARGIN = 2)

  return(list(
    betas = betas,
    EX = EX
  ))
}

join_terms <-
  function(
      x,
      y) {
    x_and_y <-
      merge(x = x, y = y, by = "row.names", all = TRUE)
    rownames(x_and_y) <- x_and_y$Row.names
    x_and_y <- x_and_y[-1]

    x_and_y
  }

calculate_coefs <-
  function(fitted_models,
           type,
           pooled = "neumark",
           baseline_invariant) {
    r <- lapply(fitted_models, extract_betas_EX, baseline_invariant)

    # extract model matrix averages
    EX_a <- r$group_a$EX
    EX_b <- r$group_b$EX

    # extract betas
    B_a <- r$group_a$betas
    B_b <- r$group_b$betas

    if (pooled == "neumark") {
      EX_pool <- r$pooled_neumark1988$EX
      B_pool <- r$pooled_neumark1988$betas
    } else {
      EX_pool <- r$pooled_jann2008$EX[names(EX_a)] # drops groupvar col
      B_pool <- r$pooled_jann2008$betas[names(B_a)]
    }

    # join terms properly
    term_types <-
      list(
        EX_pool = EX_pool,
        B_pool = B_pool,
        EX_a = EX_a,
        B_a = B_a,
        EX_b = EX_b,
        B_b = B_b
      )
    terms <-
      Reduce(
        join_terms,
        mapply( # set nice column names
          function(terms, nm) setNames(data.frame(terms), nm),
          term_types,
          names(term_types),
          SIMPLIFY = FALSE
        )
      )

    rownames(terms)[which(rownames(terms) == "X.Intercept.")] <-
      "(Intercept)"

    # calculate
    if (type == "threefold") {
      terms$endowments <- (terms$EX_a - terms$EX_b) * terms$B_b
      terms$coefficients <- terms$EX_b * (terms$B_a - terms$B_b)
      terms$interaction <-
        (terms$EX_a - terms$EX_b) *
          (terms$B_a - terms$B_b)


      OVERALL_ENDOW <- sum(terms$endowments)
      OVERALL_COEFF <- sum(terms$coefficients)
      OVERALL_INTER <- sum(terms$interaction)

      variable_level_results <-
        terms[c("endowments", "coefficients", "interaction")]

      overall_results <- list(
        endowments = OVERALL_ENDOW,
        coefficients = OVERALL_COEFF,
        interaction = OVERALL_INTER
      )
    } else if (type == "twofold") {
      # results by variable
      terms$explained <- (terms$EX_a - terms$EX_b) * terms$B_pool
      terms$unexplained_a <- terms$EX_a * (terms$B_a - terms$B_pool)
      terms$unexplained_b <- terms$EX_b * (terms$B_pool - terms$B_b)
      terms$unexplained <- terms$unexplained_a + terms$unexplained_b

      # overall results
      OVERALL_EXPL <- sum(terms$explained)
      OVERALL_UNEXPL_a <- sum(terms$unexplained_a)
      OVERALL_UNEXPL_b <- sum(terms$unexplained_b)
      OVERALL_UNEXPL <- sum(terms$unexplained)

      variable_level_results <-
        terms[
          c(
            "explained", "unexplained",
            "unexplained_a", "unexplained_b"
          )
        ]

      overall_results <- list(
        explained = OVERALL_EXPL,
        unexplained = OVERALL_UNEXPL,
        unexplained_a = OVERALL_UNEXPL_a,
        unexplained_b = OVERALL_UNEXPL_b
      )
    }

    # return overall & varlevel
    list(
      overall = overall_results,
      varlevel = variable_level_results
    )
  }

get_bootstrap_ci <- function(formula,
                             data,
                             n_bootstraps,
                             type,
                             pooled,
                             baseline_invariant,
                             conf_probs = conf_probs) {
  bs <- replicate(n_bootstraps,
    simplify = FALSE,
    {
      idx <- sample.int(
        n = nrow(data),
        size = nrow(data),
        replace = TRUE
      )
      sample_data <- data[idx, ]
      fitted_models <- fit_models(formula, sample_data)
      calculate_coefs(
        fitted_models,
        type = type,
        pooled = pooled,
        baseline_invariant = baseline_invariant
      )
    }
  )

  overall_level_list <- lapply(bs, `[[`, "overall")
  varlevel_list <- lapply(bs, `[[`, "varlevel")

  coef_types <- names(overall_level_list[[1]])
  varlevel_coef_names <- rownames(varlevel_list[[1]])

  CI_overall <- do.call(rbind, {
    lapply(coef_types, function(coeftype) {
      estimates <- sapply(overall_level_list, `[[`, coeftype)
      c(
        se = sd(estimates, na.rm = TRUE),
        quantile(estimates, probs = conf_probs)
      )
    }) |> setNames(coef_types)
  })

  # helper function
  rbind_list <- function(L) {
    do.call(rbind, L)
  }

  CI_varlevel <- lapply(coef_types, function(cftype) {
    lapply(varlevel_coef_names, function(coefname) {
      estimates <- sapply(varlevel_list, `[`, coefname, cftype)
      c(
        se = sd(estimates, na.rm = TRUE),
        quantile(estimates, probs = conf_probs)
      )
    }) |> setNames(varlevel_coef_names)
  }) |>
    setNames(coef_types) |>
    lapply(rbind_list)

  CI_varlevel <- lapply(coef_types, function(cf_type) {
    x <- CI_varlevel[[cf_type]]
    x <- as.data.frame(x)
    x["coef_type"] <- cf_type
    x["term"] <- rownames(x)
    rownames(x) <- NULL
    x[c(4, 5, 1, 2, 3)]
  }) |>
    rbind_list()

  return(list(
    overall = CI_overall,
    varlevel = CI_varlevel
  ))
}


#' Run a Blinder-Oaxaca decomposition
#'
#' @param formula A formula specifying the model as: \code{dependent_var ~
#'   x_var1 + x_var1 + ... + x_varK | group_var}.
#' @param data A data frame.
#' @param type Type of decomposition to run: either "twofold" (the default) or
#'   "threefold".
#' @param pooled \code{neumark} (the default) to exclude the group variable from
#'   the model, or \code{jann} to include the group variable.
#' @param baseline_invariant Correct for the omitted baseline bias for all
#'   factor variables?
#' @param n_bootstraps Bootstrap repetitions to use when calculating standard
#'   errors.
#' @param conf_probs CI boundaries for bootstrapped standard errors.
#'
#' @return A list with elements \code{overall}, \code{varlevel}, \code{gaps},
#'   \code{meta}, and \code{bootstraps}, which can be queried with
#'   \code{summary()} and \code{coef()}.
#' @export
#'
#' @examples
#' twofold <- OaxacaBlinderDecomp(
#'   formula = real_wage ~ age + education | female,
#'   data = chicago_long,
#'   type = "twofold",
#'   baseline_invariant = TRUE,
#'   n_bootstraps = 100
#' )
#' summary(twofold)
#' coef(twofold)
#' coef(twofold, ci = TRUE)
#'
#' threefold <- OaxacaBlinderDecomp(
#'   real_wage ~ age + education | female, chicago_long,
#'   type = "threefold",
#'   pooled = "jann",
#'   baseline_invariant = TRUE
#' )
#' summary(threefold)
#' coef(threefold)
OaxacaBlinderDecomp <-
  function(formula,
           data,
           type = "twofold",
           pooled = "neumark",
           baseline_invariant = FALSE,
           n_bootstraps = NULL,
           conf_probs = c(.025, .975)) {
    dataset_name <- deparse(substitute(data))
    input_data <- data
    gvar_to_num <- modify_group_var_to_dummy(input_data, formula)
    data <- gvar_to_num$data
    fitted_models <- fit_models(formula, data)
    results <-
      calculate_coefs(fitted_models, type, pooled, baseline_invariant)


    # collect descriptives
    results$gaps <- calculate_gap(
      fitted_models$group_a$y,
      fitted_models$group_b$y
    )
    results$meta <- list(
      type = type,
      group_levels = gvar_to_num$group_levels,
      formula = deparse(formula),
      formula_components = parse_formula(formula),
      dataset_name = dataset_name,
      data = input_data
    )


    if (!is.null(n_bootstraps)) {
      bootstrap_results <- get_bootstrap_ci(
        formula,
        data,
        n_bootstraps,
        type = type,
        pooled = pooled,
        baseline_invariant = baseline_invariant,
        conf_probs = conf_probs
      )
      results$bootstraps <- bootstrap_results
    }

    class(results) <- "OaxacaBlinderDecomp"
    results
  }
