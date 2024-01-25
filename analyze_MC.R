## ---------------------------------------------------------------------------##
## Test
## Developer Ian Danilevicz
## ---------------------------------------------------------------------------##

# read functions

# read results
dir_f ="/\\172.27.137.244\\i_danilevicz$\\Paper2\\Article_2_Simulation\\Functions_3" 
setwd(dir_f)

source(file = "f_IS_IV.R")
source(file = "f_TP.R")
source(file = "f_DFA.R")
source(file = "f_MC.R")
source(file = "read_MC.R")

mat_true = ni_c3_d7 # mc7_d4_ni

tab_bias = mc_table(mat_true, 
                    ni_c1_d3,ni_c1_d4,ni_c1_d5,ni_c1_d6,ni_c1_d7,
                    ni_c2_d3,ni_c2_d4,ni_c2_d5,ni_c2_d6,ni_c2_d7,
                    ni_c3_d3,ni_c3_d4,ni_c3_d5,ni_c3_d6,ni_c3_d7,                    
                    ibm_c1_d3,ibm_c1_d4,ibm_c1_d5,ibm_c1_d6,ibm_c1_d7,
                    ibm_c2_d3,ibm_c2_d4,ibm_c2_d5,ibm_c2_d6,ibm_c2_d7,
                    ibm_c3_d3,ibm_c3_d4,ibm_c3_d5,ibm_c3_d6,ibm_c3_d7,
                    criteria="bias", col=1:8,
                    names = c("IS","IS","IV","IV","TP1","TP1","TP2","TP2","TP3","TP3",
                              "TP4","TP4","SSP","SSP","ABI","ABI"))
tab_mse = mc_table(mat_true, 
                   ni_c1_d3,ni_c1_d4,ni_c1_d5,ni_c1_d6,ni_c1_d7,
                   ni_c2_d3,ni_c2_d4,ni_c2_d5,ni_c2_d6,ni_c2_d7,
                   ni_c3_d3,ni_c3_d4,ni_c3_d5,ni_c3_d6,ni_c3_d7,                    
                   ibm_c1_d3,ibm_c1_d4,ibm_c1_d5,ibm_c1_d6,ibm_c1_d7,
                   ibm_c2_d3,ibm_c2_d4,ibm_c2_d5,ibm_c2_d6,ibm_c2_d7,
                   ibm_c3_d3,ibm_c3_d4,ibm_c3_d5,ibm_c3_d6,ibm_c3_d7,
                    criteria="mse", col=1:8,
                    names = c("IS","IS","IV","IV","TP1","TP1","TP2","TP2","TP3","TP3",
                              "TP4","TP4","SSP","SSP","ABI","ABI"))
tab_mae = mc_table(mat_true, 
                   ni_c1_d3,ni_c1_d4,ni_c1_d5,ni_c1_d6,ni_c1_d7,
                   ni_c2_d3,ni_c2_d4,ni_c2_d5,ni_c2_d6,ni_c2_d7,
                   ni_c3_d3,ni_c3_d4,ni_c3_d5,ni_c3_d6,ni_c3_d7,                    
                   ibm_c1_d3,ibm_c1_d4,ibm_c1_d5,ibm_c1_d6,ibm_c1_d7,
                   ibm_c2_d3,ibm_c2_d4,ibm_c2_d5,ibm_c2_d6,ibm_c2_d7,
                   ibm_c3_d3,ibm_c3_d4,ibm_c3_d5,ibm_c3_d6,ibm_c3_d7,
                    criteria="mae", col=1:8,
                    names = c("IS","IS","IV","IV","TP1","TP1","TP2","TP2","TP3","TP3",
                              "TP4","TP4","SSP","SSP","ABI","ABI"))
tab_mape = mc_table(mat_true, 
                    ni_c1_d3,ni_c1_d4,ni_c1_d5,ni_c1_d6,ni_c1_d7,
                    ni_c2_d3,ni_c2_d4,ni_c2_d5,ni_c2_d6,ni_c2_d7,
                    ni_c3_d3,ni_c3_d4,ni_c3_d5,ni_c3_d6,ni_c3_d7,                    
                    ibm_c1_d3,ibm_c1_d4,ibm_c1_d5,ibm_c1_d6,ibm_c1_d7,
                    ibm_c2_d3,ibm_c2_d4,ibm_c2_d5,ibm_c2_d6,ibm_c2_d7,
                    ibm_c3_d3,ibm_c3_d4,ibm_c3_d5,ibm_c3_d6,ibm_c3_d7,
                    criteria="mape", col=1:8,
                    names = c("IS","IS","IV","IV","TP1","TP1","TP2","TP2","TP3","TP3",
                              "TP4","TP4","SSP","SSP","ABI","ABI"))
tab_icc = mc_table(mat_true, 
                   ni_c1_d3,ni_c1_d4,ni_c1_d5,ni_c1_d6,ni_c1_d7,
                   ni_c2_d3,ni_c2_d4,ni_c2_d5,ni_c2_d6,ni_c2_d7,
                   ni_c3_d3,ni_c3_d4,ni_c3_d5,ni_c3_d6,ni_c3_d7,                    
                   ibm_c1_d3,ibm_c1_d4,ibm_c1_d5,ibm_c1_d6,ibm_c1_d7,
                   ibm_c2_d3,ibm_c2_d4,ibm_c2_d5,ibm_c2_d6,ibm_c2_d7,
                   ibm_c3_d3,ibm_c3_d4,ibm_c3_d5,ibm_c3_d6,ibm_c3_d7,
                    criteria="icc", col=1:8,
                    names = c("IS","IS","IV","IV","TP1","TP1","TP2","TP2","TP3","TP3",
                              "TP4","TP4","SSP","SSP","ABI","ABI"))

round(tab_bias, 3)
round(tab_mse, 3)
round(tab_mae, 3)
round(tab_mape, 3)
round(tab_icc, 3)

tab1 = round(tab_bias, 3)
tab2 = round(tab_mse, 3)
tab3 = round(tab_mae, 3)
tab4 = round(tab_mape, 3)
tab5 = round(tab_icc, 3)

dir_tab ="/\\172.27.137.244\\i_danilevicz$\\Paper2\\Article_2_Simulation\\Tables_3" 
setwd(dir_tab)

write.table(tab1, file="bias_old_TP.csv", row.names = F, col.names = T)
write.table(tab2, file="mse_old_TP.csv", row.names = F, col.names = T)
write.table(tab3, file="mae_old_TP.csv", row.names = F, col.names = T)
write.table(tab4, file="mape_old_TP.csv", row.names = F, col.names = T, dec=".")
write.table(tab5, file="icc_old_TP.csv", row.names = F, col.names = T, dec=".")

saveRDS(tab_bias, "tab_bias_old_TP.rds")
saveRDS(tab_mse, "tab_mse_old_TP.rds")
saveRDS(tab_mae, "tab_mae_old_TP.rds")
saveRDS(tab_mape, "tab_mape_old_TP.rds")
saveRDS(tab_icc, "tab_icc_old_TP.rds")






