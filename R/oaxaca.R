
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

modify_group_var_to_dummy = function(data, formula){

  # parse fml for group/dep var
  fml_comp = parse_formula(formula)
  group_var = fml_comp$group_var
  dep_var = fml_comp$dep_var

  stopifnot("Grouping variable should have 2 unique values" = {
    g = data[[group_var]]
    length(unique(g)) == 2
  })

  # modify group var such that group0 (reference) is the group that has a higher dep_var avg
  dep_var_avgs = aggregate(data[[dep_var]], list(gr=data[[group_var]]), FUN=mean, na.rm=TRUE) 
  dep_var_avgs = dep_var_avgs[order(dep_var_avgs$x, decreasing = TRUE), ]

  group1 = dep_var_avgs$gr[1] # higher dep_var avg
  group2 = dep_var_avgs$gr[2] # lower dep_var avg

  # modify data; 0 represent the reference (higher depvar group)
  data[[group_var]] = ifelse(data[[group_var]] == group1, 0, 1)

  # return with levels specification for metainfo
  list(
    data = data, 
    group_levels = c(group1, group2)
  )

}

calculate_gap <- function(formula, data) {
  fml_comp <- parse_formula(formula)

  idx <- data[[fml_comp$group_var]] == 0

  EY_a <- mean(data[idx, ][[fml_comp$dep_var]], na.rm = TRUE)
  EY_b <- mean(data[!idx, ][[fml_comp$dep_var]], na.rm = TRUE)

  gap <- EY_a - EY_b
  pct_gap <- gap / EY_a

  list(
    gap = gap,
    pct_gap = pct_gap,
    EY_a = EY_a,
    EY_b = EY_b
  )
}

make_model_frames <- function(formula, data) {
  # get formula components
  fml_comp <- parse_formula(formula)

  # construct formulas
  fml_reg <- paste(fml_comp$dep_var, "~", fml_comp$indep_var)
  fml_reg_pooled <- paste(
    fml_comp$dep_var,
    "~",
    fml_comp$indep_var,
    "+",
    fml_comp$group_var
  )

  # convert to formula object
  fml_reg <- as.formula(fml_reg)
  fml_reg_pooled <- as.formula(fml_reg_pooled)

  # filter datasets for group a/b
  idx <- data[[fml_comp$group_var]] == 0
  data_a <- data[idx, ]
  data_b <- data[!idx, ]

  # create model matrices
  mod_frame_a <- model.frame(fml_reg, data = data_a)
  mod_frame_b <- model.frame(fml_reg, data = data_b)
  mod_frame_pooled <- model.frame(fml_reg_pooled, data = data)

  # return model frames
  model_frames <- list(
    a = mod_frame_a,
    b = mod_frame_b,
    pooled = mod_frame_pooled
  )
  model_frames
}

fit_models <- function(design_matrices, dep_var) {
  lapply(design_matrices, function(frame) {
    lm(as.formula(paste(dep_var, "~ .")), data = frame)
  })
}

extract_betas_X_averages <- function(fitted_models) {
  inner_return <- lapply(fitted_models, function(model) {
    betas <- coef(model)
    modmat <- model.matrix(model)
    EX <- apply(modmat, mean, MARGIN = 2)

    list(betas = betas, EX = EX)
  })
  # reorganise by betas / EX, instead of a, b and pooled
  betas <- lapply(inner_return, function(a) a$betas)
  EX <- lapply(inner_return, function(a) a$EX)

  # outer return
  list(betas = betas, EX = EX)
}

calculate_coefs <- function(X_averages, betas, type) {

  # extract model matrix averages
  EX_a <- X_averages$a
  EX_b <- X_averages$b
  EX_pool <- X_averages$pooled
  EX_pool_wo_gr <- X_averages$pooled[names(EX_a)]

  # extract betas
  B_a <- betas$a
  B_b <- betas$b
  B_pool <- betas$pooled
  B_pool_wo_gr <- betas$pooled[names(B_a)] # drops gender from cols

  if (type == "threefold"){

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
    EXPL <- (EX_a - EX_b) * B_pool_wo_gr
    UNEXPL_a <- EX_a * (B_a - B_pool_wo_gr)
    UNEXPL_b <- EX_b * (B_pool_wo_gr - B_b)
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
  list(
    overall = overall_results,
    varlevel = variable_level_results
  )
}

#' @title OaxacaBlinderDecomp
#' @param formula: formula object specifying the model. The formula should be 
#' specified as: dep_var ~ x_var1 + x_var2 + ... x_varK | grouping_variable
#' factor variable are supported by default and can be inserted in the formula
#' without encoding manually
#' @param data: a data.frame
#' @param type: the decomposition type: `twofold` or `threefold`
#' @export 
OaxacaBlinderDecomp <- function(formula, data, type = "twofold") {
  
  dataset_name = deparse(substitute(data))
  gvar_to_num = modify_group_var_to_dummy(data, formula)
  data = gvar_to_num$data
  model_frames <- make_model_frames(formula, data)
  fitted_models <- fit_models(model_frames, dep_var = as.character(formula)[2])
  B_and_Xavg <- extract_betas_X_averages(fitted_models)
  X_averages <- B_and_Xavg$EX
  betas <- B_and_Xavg$betas
  results <- calculate_coefs(X_averages, betas, type)
  results$gaps <- calculate_gap(formula, data)
  results$meta <- list(
    type = type,
    group_levels = gvar_to_num$group_levels,
    formula = deparse(formula),
    formula_components = parse_formula(formula), 
    data = dataset_name
  )

  class(results) <- "OaxacaBlinderDecomp"
  results
}
