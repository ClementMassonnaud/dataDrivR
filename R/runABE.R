# date: Tue Dec 15 14:10:22 2020
# author: cmasso6
#------------------------------------

#' The function
#'
#' @import abe data.table
#' @export
#' 
run = function(params, samples, tau, cores, size) {

  ## PARAMS
  data = params$dt
  x = params$x
  y = params$y
  estimate_KB = params$estimate_KB
  estimate_ABE = params$estimate_ABE
  estimate_BE = params$estimate_BE
  type = params$type

  covar = setdiff(colnames(data), c(y, x))

  ## RUN ABE ON SAMPLES =============================

  res = parallel::mclapply(mc.cores = cores, samples, function(s) {
    
    if (!s$use) {
      ##print("Not using dataset because non-varying x or y")
      return(0)
    } else {
      subd = data[s$rows]
      if (!is.null(s$to_remove)) {
        ##print(paste0("Removing cols: ", paste(s$to_remove, collapse = ',')))
        subd[, (s$to_remove) := NULL]
      }
      
      mod = run_full_model(subd, x, y, type = type)
      exp.beta = ifelse(type == "lm", FALSE, TRUE)
      resultabe = abe(fit = mod,
                      exp.beta = exp.beta,
                      data = subd,
                      tau = tau,
                      include = x,
                      criterion = "BIC",
                      type.factor = "factor",
                      exact = TRUE,
                      type.test="Chisq",
                      verbose = FALSE)
      
      return(resultabe)
    }
  })

  ## Count and remove unused samples
  used = which(unlist(lapply(res, function(r) any(class(r) == "lm"))))
  res = res[used]
  
  # Function to extract results
  extract = function(results) {
    out = lapply(results, function(mod) {
      sum = summary(mod)
      dataf = as.data.frame(sum$coefficients)
      v_exp = dataf[x, "Estimate"]
      v_exp_se = dataf[x, "Std. Error"]
      data.table(v_exp = v_exp,
                 v_exp_se = v_exp_se)
    })
    rbindlist(out)
  }


  ## ============================================================
  ##                          COMPARE CI
  ## ============================================================
  obs = extract(res)

  if(identical(tau, Inf)) {
    comp_ABE_ABE = IC(obs, estimate_BE)
  } else {
    comp_ABE_ABE = IC(obs, estimate_ABE)
  }
  comp_ABE_KB  = IC(obs, estimate_KB)

  make_table_IC = function(comp, vs, size) {
    dt = data.table("size" = comp$type)    
    dt[, paste0(size, ".vs.", vs) := comp$prop * 100]    
  }

  cover = merge(make_table_IC(comp_ABE_ABE, vs = "ABE", size = size),
                make_table_IC(comp_ABE_KB, vs = "KB", size = size),
                by = "size")

  ## ============================================================
  ##                 COMPUTE VARIABLE INCLUSION
  ## ============================================================

  inc = compute_inclusion(res, covar, x, y, size)
  
  return(list(cover = cover,
              inc = inc,
              used = used))

}
