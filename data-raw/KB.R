# date: Fri Dec 18 17:44:45 2020
# author: cmasso6
#------------------------------------
# CLEAR
rm(list = ls())
# REQUIRES
library(data.table)
library(abe)
library(bitalR)
library(magrittr)

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

resKB = runKB(params = params,
              samples = samples,
              cores = cores,
              size = size)

saveRDS(resKB, paste0("resKB_dt", set, "_", size, ".rds"))

