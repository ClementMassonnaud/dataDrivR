# date: Mon Dec 14 18:43:55 2020
# author: cmasso6
#------------------------------------
# CLEAR
rm(list = ls())
# REQUIRES
library(data.table)
library(abe)
library(magrittr)
library(bitalR)


## PARAMS ===========================
args = commandArgs(trailingOnly = T)
eval(parse(text = args))

## size = "S"
## cores = 10
if (set == 1) {
  params = data1
  samples = readRDS("samples1.rds")[[size]]
} else if (set == 2) {
  params = data2
  samples = readRDS("samples2.rds")[[size]]
} else if (set == 3) {
  params = data3
  samples = readRDS("samples3.rds")[[size]]
}

##============================================================================
##                                ABE
##============================================================================

resABE = run(params = params,
             samples = samples,
             tau = 0.05,
             cores = cores,
             size = size)

saveRDS(resABE, paste0("resABE_dt", set, "_", size, ".rds"))

