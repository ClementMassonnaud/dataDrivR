# date: Fri Dec 18 15:23:07 2020
# author: cmasso6
#------------------------------------
# CLEAR
rm(list = ls())
# REQUIRES
library(data.table)
library(bitalR)

## PARAMS ===========================

sizes = c('S' = 75, 'M' = 300, 'L' = 3000)
N = 10000
cores = 10

samples = make_samples(params = data2, sizes, N, cores)
saveRDS(samples, "samples2.rds")
