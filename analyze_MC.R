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

tab_bias = readRDS(file = "mc7_d4_iba.rds")

## figures 
setwd("C:\\Users\\i_danilevicz\\Documents\\i_danilevicz\\GGIR\\Sim\\Results\\Figures")
save.image(file = "Env_05-10-23.RData")

names8= c("IS", "IV", expression(tilde(pi)[arw]),expression(tilde(pi)[ars]), expression(tilde(pi)[raw]),expression(tilde(pi)[ras]),
          expression(hat(alpha)), "ABI")
names8= c("IS", "IV", 
          substitute(list(TP[x]), list(TP="TP", x="ar,w")),
          substitute(list(TP[x]), list(TP="TP", x="ar,s")),
          substitute(list(TP[x]), list(TP="TP", x="ra,w")),
          substitute(list(TP[x]), list(TP="TP", x="ra,s")),          
          expression(hat(alpha)), "ABI")
cols8= c("IS", "IV", "Parw", "Pars","Praw", "Pras", "alpha", "ABI")

# MAPE replace entire day
plot_ts_1pair(tab4, 1,range=1, "MAPE (%)", title =  names8[1])
plot_ts_1pair(tab4, 2,range=1, "MAPE (%)", title =  names8[2])
plot_ts_1pair(tab4, 3,range=1, "MAPE (%)", title =  names8[3])
plot_ts_1pair(tab4, 4,range=1, "MAPE (%)", title =  names8[4])
plot_ts_1pair(tab4, 5,range=1, "MAPE (%)", title =  names8[5])
plot_ts_1pair(tab4, 6,range=1, "MAPE (%)", title =  names8[6])
plot_ts_1pair(tab4, 7,range=1, "MAPE (%)", title =  names8[7])
plot_ts_1pair(tab4, 8,range=1, "MAPE (%)", title =  names8[8])

# MAPE replace until 8h
plot_ts_2pair(tab4, 1,range=1, "MAPE (%)", title =  names8[1])
plot_ts_2pair(tab4, 2,range=1, "MAPE (%)", title =  names8[2])
plot_ts_2pair(tab4, 3,range=1, "MAPE (%)", title =  names8[3])
plot_ts_2pair(tab4, 4,range=1, "MAPE (%)", title =  names8[4])
plot_ts_2pair(tab4, 5,range=1, "MAPE (%)", title =  names8[5])
plot_ts_2pair(tab4, 6,range=1, "MAPE (%)", title =  names8[6])
plot_ts_2pair(tab4, 7,range=1, "MAPE (%)", title =  names8[7])
plot_ts_2pair(tab4, 8,range=1, "MAPE (%)", title =  names8[8])

## --------------------------------------------------------------------------##
# intro 1
plot_ts_unique(tab4, 1,range=1, "MAPE (%)", title =  names8[1], NI=TRUE)
plot_ts_unique(tab4, 1,range=1, "MAPE (%)", title =  names8[1], NI=FALSE)

# intro 4
plot_ts_simple(tab4, 1,range=1, "MAPE (%)", title =  names8[1], NI=TRUE)
plot_ts_simple(tab4, 1,range=1, "MAPE (%)", title =  names8[1], NI=FALSE)

# true results 
plot_ts(tab4, 1,range=1, "MAPE (%)", title =  names8[1])
plot_ts(tab4, 2,range=1, "MAPE (%)", title =  names8[2])
plot_ts(tab4, 3,range=1, "MAPE (%)", title =  names8[3])
plot_ts(tab4, 4,range=1, "MAPE (%)", title =  names8[4])
plot_ts(tab4, 5,range=1, "MAPE (%)", title =  names8[5])
plot_ts(tab4, 6,range=1, "MAPE (%)", title =  names8[6])
plot_ts(tab4, 7,range=1, "MAPE (%)", title =  names8[7])
plot_ts(tab4, 8,range=1, "MAPE (%)", title =  names8[8])

plot_ts(tab1, 1,range=2, "Bias", title =  names8[1])
plot_ts(tab1, 2,range=2, "Bias", title =  names8[2])
plot_ts(tab1, 3,range=2, "Bias", title =  names8[3])
plot_ts(tab1, 4,range=2, "Bias", title =  names8[4])
plot_ts(tab1, 5,range=2, "Bias", title =  names8[5])
plot_ts(tab1, 6,range=2, "Bias", title =  names8[6])
plot_ts(tab1, 7,range=2, "Bias", title =  names8[7])
plot_ts(tab1, 8,range=2, "Bias", title =  names8[8])

# unbalanced Day Night - balanced Day Night
box_diff(tab_mape, diff="DN", title=names8)
# unbalanced Weekdays - balanced week weekend
box_diff(tab_mape, diff="WE", title=names8)
# not imput - imputed
box_diff(tab_mape, diff="IBM", title=names8)






