## ---------------------------------------------------------------------------##
## Test
## Developer Ian Danilevicz
## ---------------------------------------------------------------------------##
library(parallel)
library(pbapply)
library(resample)	

#library(chron) # to check weekend 

#library(xts) # cluster parallel 

#setwd("D:\\Ian\\GGIR\\Sim\\Functions")
dir1 ="C:\\Users\\i_danilevicz\\Desktop\\Ian\\Functions_3"
dir2 ="C:\\Users\\i_danilevicz\\Desktop\\Ian\\Data"
dir3 ="C:\\Users\\i_danilevicz\\Desktop\\Ian\\Results_3"

setwd(dir1)
source(file = "f_IS_IV.R")
source(file = "f_TP.R")
source(file = "f_DFA.R")
source(file = "f_MC.R")

setwd(dir2)
load("TS_2859_4D.RData")
dim(TS_2859)


## --------------------------------------------------------------------------##
## IBM and NI imputation by average and no imputation  
## --------------------------------------------------------------------------##

setwd(dir3)

m1 = mc(Array=TS_2859, nday=3, day_33=0, night_33=0, daynight_33=0, minutes=60*25, name_ni="ni_c3_d3.rds", name_ibm="ibm_c3_d3.rds")
m2 = mc(Array=TS_2859, nday=4, day_33=0, night_33=0, daynight_33=0, minutes=60*25, name_ni="ni_c3_d4.rds", name_ibm="ibm_c3_d4.rds")
m3 = mc(Array=TS_2859, nday=5, day_33=0, night_33=0, daynight_33=0, minutes=60*25, name_ni="ni_c3_d5.rds", name_ibm="ibm_c3_d5.rds")
m4 = mc(Array=TS_2859, nday=6, day_33=0, night_33=0, daynight_33=0, minutes=60*25, name_ni="ni_c3_d6.rds", name_ibm="ibm_c3_d6.rds")
m5 = mc(Array=TS_2859, nday=7, day_33=0, night_33=0, daynight_33=0, minutes=60*25, name_ni="ni_c3_d7.rds", name_ibm="ibm_c3_d7.rds")

m6 = mc(Array=TS_2859, nday=3, day_33=0, night_33=0, daynight_33=1, minutes=60*8, name_ni="ni_c2_d3.rds", name_ibm="ibm_c2_d3.rds")
m7 = mc(Array=TS_2859, nday=4, day_33=0, night_33=0, daynight_33=1, minutes=60*8, name_ni="ni_c2_d4.rds", name_ibm="ibm_c2_d4.rds")
m8 = mc(Array=TS_2859, nday=5, day_33=0, night_33=0, daynight_33=1, minutes=60*8, name_ni="ni_c2_d5.rds", name_ibm="ibm_c2_d5.rds")
m9 = mc(Array=TS_2859, nday=6, day_33=0, night_33=0, daynight_33=1, minutes=60*8, name_ni="ni_c2_d6.rds", name_ibm="ibm_c2_d6.rds")
m10 = mc(Array=TS_2859, nday=7, day_33=0, night_33=0, daynight_33=1, minutes=60*8, name_ni="ni_c2_d7.rds", name_ibm="ibm_c2_d7.rds")

m11 = mc(Array=TS_2859, nday=3, day_33=1, night_33=1, daynight_33=0, minutes=60*8, name_ni="ni_c1_d3.rds", name_ibm="ibm_c1_d3.rds")
m12 = mc(Array=TS_2859, nday=4, day_33=1, night_33=1, daynight_33=0, minutes=60*8, name_ni="ni_c1_d4.rds", name_ibm="ibm_c1_d4.rds")
m13 = mc(Array=TS_2859, nday=5, day_33=1, night_33=1, daynight_33=0, minutes=60*8, name_ni="ni_c1_d5.rds", name_ibm="ibm_c1_d5.rds")
m14 = mc(Array=TS_2859, nday=6, day_33=1, night_33=1, daynight_33=0, minutes=60*8, name_ni="ni_c1_d6.rds", name_ibm="ibm_c1_d6.rds")
m15 = mc(Array=TS_2859, nday=7, day_33=1, night_33=1, daynight_33=0, minutes=60*8, name_ni="ni_c1_d7.rds", name_ibm="ibm_c1_d7.rds")
  
## --------------------------------------------------------------------------##


## --------------------------------------------------------------------------##
