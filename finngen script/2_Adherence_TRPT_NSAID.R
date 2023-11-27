
###############################################################################
#
# 1) import of Adherence functions to compute individual trajectories on purch dataset
#      
# 2) compute individual trajectories for TRIPTANS and NSAIDs and write them
#
#
###############################################################################

library(ggplot2)
library(data.table)
library(dplyr)

#############
#     1     #
#############

setwd('/home/ivm/')

source('drugs/scripts/0_Adherence_func.R')

purch <- fread('drugs/data/finngen_R12_purch_vnr_98_alive_2010.gz') 

#############
#     2     #
#############

trpt <- getTrajectoriesDates(purch,'^N02CC')
fwrite(trpt, 'drugs/data/R12_triptans_98_23.txt', sep = '\t', quote = F)

AF <- getTrajectoriesDates(purch,'^M01')
fwrite(AF, 'drugs/data/R12_NSAIDs_98_23.txt', sep = '\t', quote = F)
