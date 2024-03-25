parse_formula <- function(data, formula) {
  # convert to character and split in depvar, indepvar and groupvar
  fml_str <- as.character(formula)
  lhs <- fml_str[2]
  rhs <- fml_str[3]
  rhs <- strsplit(x = gsub("\\s+", "", rhs),
                  split = "|",
                  fixed = TRUE)[[1]]
  group_var <- rhs[2]
  rhs <- rhs[1]

  # save reference level
  ref_level = levels(data[[group_var]])[2]

  # returns list of character strings
  list(
    dep_var = lhs,
    indep_var = rhs,
    group_var = group_var,
    ref_level = ref_level
  )
}

validate_columns = function(data, formula) {
  fml_comp <- parse_formula(data, formula)
  dep_var = fml_comp$dep_var
  indep_var = fml_comp$indep_var
  group_var = fml_comp$group_var

  # stop if data / variable are not right format
  stopifnot(
    "`data` should be a data.frame" = inherits(data, "data.frame"),
    "dependent variable should be a numeric" = inherits(data[[dep_var]], "numeric"),
    "grouping variable should be a factor with 2 levels" = inherits(data[[group_var]], "factor") &&
      length(levels(data[[group_var]])) == 2
  )
}

calculate_gap <- function(formula, data_a, data_b) {
  fml_comp <- parse_formula(data_a, formula)

  EY_a <- mean(data_a[[fml_comp$dep_var]], na.rm = TRUE)
  EY_b <- mean(data_b[[fml_comp$dep_var]], na.rm = TRUE)

  gap <- EY_a - EY_b
  pct_gap <- gap / EY_a

  list(
    gap = gap,
    pct_gap = pct_gap,
    EY_a = EY_a,
    EY_b = EY_b
  )
}

fit_models <- function(formula, data) {
  # get formula components
  fml_comp <- parse_formula(data, formula)

  # update contrast for model fit (jann2008, including groupvar)
  contrasts(data[[fml_comp$group_var]])  = contr.SAS(levels(data[[fml_comp$group_var]]))

  idx <- data[[fml_comp$group_var]] == fml_comp$ref_level
  data_a <- data[idx,]
  data_b <- data[!idx,]

  # construct formulas
  fml_reg <- paste(fml_comp$dep_var, "~", fml_comp$indep_var)

  # currently; pooled reg without group ind as suggested by Neumark (1988)
  fml_reg_pooled_neumark1988 <-
    paste(fml_comp$dep_var, "~", fml_comp$indep_var)
  fml_reg_pooled_jann2008 <-
    paste(fml_comp$dep_var,
          "~",
          fml_comp$indep_var,
          "+",
          fml_comp$group_var)

  # convert to formula object
  fml_reg <- as.formula(fml_reg)
  fml_reg_pooled_neumark1988 <-
    as.formula(fml_reg_pooled_neumark1988)
  fml_reg_pooled_jann2008 <- as.formula(fml_reg_pooled_jann2008)

  mod_a = lm(fml_reg, data = data_a)
  mod_b = lm(fml_reg, data = data_b)
  mod_pooled_neumark1988 = lm(fml_reg_pooled_neumark1988, data = data)
  mod_pooled_jann2008 = lm(fml_reg_pooled_jann2008, data = data, contrasts = )

  return(
    list(
      mod_a = mod_a,
      mod_b = mod_b,
      mod_pooled_neumark1988 = mod_pooled_neumark1988,
      mod_pooled_jann2008 = mod_pooled_jann2008
    )
  )
}

extract_betas_EX = function(mod, baseline_invariant) {
  modmat = model.matrix(mod)
  betas = coef(mod)

  # if baseline variant;
  # identify factor variables and associated dummy indicators
  # apply gardeazabal2004 ommitted baseline correction per set of dummy variables
  if (baseline_invariant) {
    # identify factor terms
    factor_variables = names(attr(modmat, "contrasts"))

    terms = attr(mod$terms, "term.labels")
    term_assignments_i = attr(modmat, "assign")  # intercept = 0; gets removed
    term_assignments = terms[term_assignments_i]

    # for each dummy encoded term; adjust the betas; save and add a baseline coef to beta and modmat
    for (factor_var in factor_variables) {
      # beta adjustment
      dummy_index_in_beta = 1 + which(term_assignments == factor_var)
      k = length(dummy_index_in_beta) + 1
      c = sum(betas[dummy_index_in_beta]) / k
      betas[1] = betas[1] + c
      betas[dummy_index_in_beta] = betas[dummy_index_in_beta] - c

      # add baseline level
      betas[length(betas) + 1] = -c
      baseline_name = paste(factor_var, ".baseline", sep = "")
      names(betas)[length(betas)] = baseline_name

      # add baseline indicator to modmat
      baseline_indicator = ifelse(rowSums(modmat[, dummy_index_in_beta, drop =
                                                   FALSE]) == 0, 1, 0)
      modmat = cbind(modmat, baseline_indicator)
      colnames(modmat)[ncol(modmat)] = baseline_name
    }
  }

  EX = apply(modmat, mean, MARGIN = 2)

  return(list(betas = betas,
              EX = EX))
}

calculate_coefs <-
  function(fitted_models,
           type,
           pooled = "neumark",
           baseline_invariant) {
    r = lapply(fitted_models, extract_betas_EX, baseline_invariant)

    # extract model matrix averages
    EX_a <- r$mod_a$EX
    EX_b <- r$mod_b$EX

    # extract betas
    B_a <- r$mod_a$betas
    B_b <- r$mod_b$betas

    if (pooled == "neumark") {
      EX_pool = r$mod_pooled_neumark1988$EX
      B_pool = r$mod_pooled_neumark1988$betas
    } else{
      EX_pool = r$mod_pooled_jann2008$EX  [names(EX_a)] # drops groupvar col
      B_pool = r$mod_pooled_jann2008$betas[names(B_a)]
    }

    if (type == "threefold") {
      ENDOW = (EX_a - EX_b) * B_b
      COEFF = EX_b * (B_a - B_b)
      INTER = (EX_a - EX_b) * (B_a - B_b)

      OVERALL_ENDOW = sum(ENDOW)
      OVERALL_COEFF = sum(COEFF)
      OVERALL_INTER = sum(INTER)

      variable_level_results <- data.frame(
        endowments = ENDOW,
        coefficients = COEFF,
        interaction = INTER
      )

      overall_results <- list(
        endowments = OVERALL_ENDOW,
        coefficients = OVERALL_COEFF,
        interaction = OVERALL_INTER
      )
    } else if (type == "twofold") {
      # results by variable
      EXPL <- (EX_a - EX_b) * B_pool
      UNEXPL_a <- EX_a * (B_a - B_pool)
      UNEXPL_b <- EX_b * (B_pool - B_b)
      UNEXPL <- UNEXPL_a + UNEXPL_b

      # overall results
      OVERALL_EXPL <- sum(EXPL)
      OVERALL_UNEXPL_a <- sum(UNEXPL_a)
      OVERALL_UNEXPL_b <- sum(UNEXPL_b)
      OVERALL_UNEXPL <- sum(UNEXPL)

      variable_level_results <- data.frame(
        explained = EXPL,
        unexplained = UNEXPL,
        unexplained_a = UNEXPL_a,
        unexplained_b = UNEXPL_b
      )

      overall_results <- list(
        explained = OVERALL_EXPL,
        unexplained = OVERALL_UNEXPL,
        unexplained_a = OVERALL_UNEXPL_a,
        unexplained_b = OVERALL_UNEXPL_b
      )
    }

    # return overall & varlevel
    list(overall = overall_results,
         varlevel = variable_level_results)
  }

get_bootstrap_ci = function(formula,
                            data,
                            n_bootstraps,
                            type,
                            pooled,
                            baseline_invariant,
                            conf_probs = conf_probs) {
  bs = replicate(n_bootstraps,
                 simplify = FALSE, {
                   idx = sample.int(n = nrow(data),
                                    size = nrow(data),
                                    replace = TRUE)
                   sample_data = data[idx,]
                   fitted_models = fit_models(formula, sample_data)
                   calculate_coefs(
                     fitted_models,
                     type = type,
                     pooled = pooled,
                     baseline_invariant = baseline_invariant
                   )
                 })

  overall_level_list = lapply(bs, `[[`, "overall")
  varlevel_list = lapply(bs, `[[`, "varlevel")

  coef_types = names(overall_level_list[[1]])
  varlevel_coef_names = rownames(varlevel_list[[1]])

  CI_overall = do.call(rbind, {
    lapply(coef_types, function(coeftype) {
      estimates <- sapply(overall_level_list, `[[`, coeftype)
      c(
        se = sd(estimates, na.rm = TRUE),
        quantile(estimates, probs = conf_probs)
      )
    }) |> setNames(coef_types)
  })

  # helper function
  rbind_list = function(L)
    do.call(rbind, L)

  CI_varlevel = lapply(coef_types, function(cftype) {
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

  CI_varlevel = lapply(coef_types, function(cf_type) {
    x = CI_varlevel[[cf_type]]
    x = as.data.frame(x)
    x["coef_type"] = cf_type
    x["term"] = rownames(x)
    rownames(x) = NULL
    x[c(4, 5, 1, 2, 3)]
  }) |>
    rbind_list()

  return(list(overall = CI_overall,
              varlevel = CI_varlevel))
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
#' twofold = OaxacaBlinderDecomp(
#'   formula = real_wage ~ age + education | female,
#'   data = chicago_long,
#'   type = "twofold",
#'   baseline_invariant = TRUE,
#'   n_bootstraps = 100
#' )
#' summary(twofold)
#' coef(twofold)
#' coef(twofold, ci=TRUE)
#'
#' threefold = OaxacaBlinderDecomp(
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
    validate_columns(data, formula)

    dataset_name = deparse(substitute(data))
    fitted_models <- fit_models(formula, data)
    results <-
      calculate_coefs(fitted_models, type, pooled, baseline_invariant)

    # collect descriptives
    results$gaps <- calculate_gap(
      formula,
      model.frame(fitted_models$mod_a),
      model.frame(fitted_models$mod_b)
    )
    results$meta <- list(
      type = type,
      formula = deparse(formula),
      formula_components = parse_formula(data, formula),
      dataset_name = dataset_name,
      data = data,
      fitted_models = fitted_models
    )


    if (!is.null(n_bootstraps)) {
      bootstrap_results = get_bootstrap_ci(
        formula,
        data,
        n_bootstraps,
        type = type,
        pooled = pooled,
        baseline_invariant = baseline_invariant,
        conf_probs = conf_probs
      )
      results$bootstraps = bootstrap_results
    }

    class(results) <- "OaxacaBlinderDecomp"
    results
  }
