#' Function that checks for non-varying variables
#' @param data the dataset
#' @param x the name of the x variable of the model
#' @param y the name of the y variable of the model
#' @return returns TRUE if all variables vary. Returns FALSE if x or y vary. Else returns the names of the non-varying variables.
#'
#' @import data.table
#' 
#' @export
#' 
check_varying = function(data, x, y, type = "lm") {

  names_coef = lapply(colnames(data), function(col) {
    data.table("name.col" = col,
               "name.coef" = c(col, paste0(col, names(table(data[[col]])))))
  })
  names_coef = rbindlist(names_coef)
  
  mod = run_full_model(data, x = x, y = y, type = type)
  na_coef = data.table(name.coef = names(mod$coefficients)[is.na(mod$coefficients)])
  
  cols = merge(names_coef, na_coef, all.y = F, all.x = F, by = "name.coef")$name.col

  if (!length(cols)) { # all variables vary
    return(TRUE)
  } else if (x %in% cols | y %in% cols) { # x or y do not vary
    return(FALSE) 
  } else {
    return(cols)
  }
}


#' Function to run a saturated linear model
#'
#' @param data the data set
#' @param x character vector, the x variable
#' @param y character vector, the y variable
#'
#' @return the linear model object
#'
#' @export
#' 
run_full_model = function(data, x, y, type = "lm") {

  covar = setdiff(colnames(data), c(y, x))
  form = as.formula(paste(y, "~", x, '+',
                          paste(covar, collapse = '+')))

  if (type == "lm") {
    mod = lm(form, data = data, x = TRUE, y = TRUE)
  } else if (type == "glm") {
    mod = glm(form, data = data, x = TRUE, y = TRUE, family = "binomial")
  }
}


#' Check a list of sampled datasets for non-varying variables. A mclapply wrapper around check_varying()
#'
#' @param dt the initial dataset
#' @param samples which rows to subset from dt. A list of list, with at least a samples$rows containing the rows indices to subset. samples$rows can be greater than nrow(dt).
#' @param x character vector, the x variable
#' @param y character vector, the y variable
#'
#' @return The samples list with added samples$use = FALSE if the sampled dt should not be used, samples$use = TRUE otherwise. If not null, a samples$to_remove character vector of non-varying variables to remove from the model for the given sample.
#'
#' @export
check_datasets = function(dt, samples, x, y, type = "lm") {

  res = parallel::mclapply(mc.cores = cores, samples, function(s) {

    cols = check_varying(dt[s$rows], x, y, type = type)
    
    if (cols[[1]] == FALSE) {
     
      s$use = FALSE
    } else if (cols[[1]] == TRUE) {
      s$use = TRUE
    } else {
      ## print(paste0(length(cols), " non-varying cols on the dataset: ", paste(cols, collapse = ",")))
      s$use = TRUE
      s$to_remove = cols
    }
    return(s)
  })
}

#' Make multiple clones of the dataset, and rbind them to create an artificial dataset
#'
#' @param data the data.table
#' @param i the approximate number of clones to create.
#'
#' @details For optimization purposes, the function is not row-binding the dataset i times, but rather row-binding the dataset to itself on each iteration. So the length of the dataset is doubling at each iteration. To have a final dataset of approximately 'i' times the original, we must double it 'n = log(i, base = 2)' times.
#'
#' @export
genere = function(data, i) {

  n = round(log(i, base = 2))
  
  for(i in 1:n) {
    data = rbind(data, data)
  }

  return(data)
}

#' Obtenir le nombre de fois ou l'ic contient l'estimation#####
#'
#' @export
#' 
IC <- function(obs, estimate) {

  moy  <- obs$v_exp
  ecar <- obs$v_exp_se

  retest = data.table("valeur" = estimate,
                      "up" = moy + (qnorm(0.975)*ecar),
                      "lo" = moy - (qnorm(0.975)*ecar))
  
  covered = between(retest$valeur, retest$lo, retest$up)
  over  = retest$valeur < retest$lo
  under = retest$valeur > retest$up
  
  dt = data.table("type" = c("covered", "over", "under"),
                  "n" = c(sum(covered), sum(over), sum(under)),
                  "prop" = c(mean(covered), mean(over), mean(under)))
  return(dt)
}

#' Get covariables names included in the model
#'
#' @param model the model
#' @param x character vector, the x variable
#' @param y character vector, the y variable
#' 
#' @export
get_covar = function(model, x, y) {

  form = as.character(model$terms[[3]]) %>% paste0(collapse = " + ")
  cols = strsplit(form, " + ", fixed = T) %>% unlist()
  cols = subset(cols, !cols %in% c("+", x))
  return(cols)
}

#' Compute proportion of covar inclusion in the model
#'
#' @param samples the list returned by check_datasets()
#' @param res the list of models
#' @param covars character of covariables names
#' @param x character vector, the x variable
#' @param y character vector, the y variable
#'
#' @export
compute_inclusion = function(res, covars, x, y, size) {

  inclu = lapply(covars, function(col) {

    ## denom = sum(sapply(samples[[size]], function(s) !(col %in% s$to_remove)))
    denom = length(res)
    num = sum(sapply(res, function(mod) col %in% get_covar(mod, x, y)))
    dt = data.table("var" = col,
                    "prop" = num/denom) %>% setnames("prop", paste0("prop", size))
  })
  return(rbindlist(inclu))
}
