# date: Fri Dec 18 17:31:39 2020
# author: cmasso6
#------------------------------------

#' The function for KB
#'
#' @import abe data.table
#' @export
#' 
runKB = function(params, samples, cores, size) {
  
  dt = params$dt
  x = params$x
  y = params$y
  covar = params$prior
  estimate_KB = params$estimate_KB
  type = params$type
  
  res = parallel::mclapply(mc.cores = cores, samples, function(s) {

    if (!s$use) {
      ##print("Not using dataset because non-varying x or y")
      return(0)
    } else {
      subd = dt[s$rows]
      if (!is.null(s$to_remove)) {
        ##print(paste0("Removing cols: ", paste(s$to_remove, collapse = ',')))
        subd[, (s$to_remove) := NULL]
      }
      
      form = as.formula(paste(y, "~", x, "+", paste(covar, collapse = '+')))
      if (type == "lm") {
        mod = lm(form, data = subd)
      } else if (type == "glm") {
        mod = glm(form, data = subd, family = "binomial")
      }
      
      return(mod)
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
  
  comp_KB_KB = IC(obs, estimate_KB)

  make_table_IC = function(comp, vs, size) {
    dt = data.table("size" = comp$type)    
    dt[, paste0(size, ".vs.", vs) := comp$prop * 100]    
  }

  cover = make_table_IC(comp_KB_KB, vs = "KB", size = size)

  ## ============================================================
  ##                 COMPUTE VARIABLE INCLUSION
  ## ============================================================

  inc = compute_inclusion(res, covar, x, y, size)
  
  return(list(cover = cover,
              inc = inc,
              used = used))

}
