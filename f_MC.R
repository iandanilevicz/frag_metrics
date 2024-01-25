## --------------------------------------------------------------------------##
## Detrended Fluctuation Analysis (DFA)
## author: Ian Meneghel Danilevicz 
## orcid: 0000-0003-4541-0524
## email: ian.meneghel-danilevicz @ inserm.fr
## --------------------------------------------------------------------------##


#library(irr)

trans_pos = function(y, target=c(0,1)){
  n = length(y)
  vec = NULL
  for(i in 2:n){
    if(!is.na(y[i-1]) && !is.na(y[i])){
      if(y[i-1]==target[1] && y[i]==target[2]) vec = c(vec,i)      
    }
  }
  return(vec)
}

create_missing = function(acc, awake, nday, day_33, night_33, daynight_33){  
  nday  = nday - 1
  x     = as.vector(acc)
  awake = as.vector(awake) 
  tpp01 = trans_pos(as.numeric(awake))
  tpp10 = trans_pos(as.numeric(awake), c(1,0))
  n = length(x)
  days = sample(1:6, nday, replace = FALSE)
  for(i in 1:6){
    if(any(days==i) == TRUE){
      if(day_33 == 1) x = remove_day(x,i, tpp01, tpp10)
      if(night_33 == 1) x = remove_night(x,i, tpp01, tpp10) 
      if(daynight_33 == 1) x = remove_daynight(x,i, tpp01, tpp10)  
    }else{
      x = remove_full(x,i, tpp01, tpp10)
    }
  }
  return(x)
}

remove_full = function(x,i, tpp01, tpp10){
  n = length(x)
  if(tpp01[1] < tpp10[1]){
    # start by day == 0 
    if(i == 1) x[1:tpp10[1]] = NA
    else{
      if(length(tpp10) >= i){
        x[tpp10[i-1]:tpp10[i]] = NA
      }else{
        if(length(tpp10) >= (i-1)) x[tpp10[i-1]:n] = NA
      }    
    }
  }else{
    # start by night == 1
    if(i == 1) x[1:tpp01[1]] = NA
    else{
      if(length(tpp01) >= i){
        x[tpp01[i-1]:tpp01[i]] = NA
      }else{
        if(length(tpp01) >= (i-1)) x[tpp01[i-1]:n] = NA
      }    
    }
  }
  return(x)
}

remove_daynight = function(x,i, tpp01, tpp10){
  n = length(x)
  if(tpp01[1] < tpp10[1]){
    # start by day == 0 
    if(i == 1){
      n1 = length(x[1:tpp10[1]])  
      n2 = round(n1/3, 0)
      n3 = sample(1:n2, 1)
      start = sample(1:(2*n2), 1) 
      x[start:(start+n3)] = NA
    }else{
      if(length(tpp10) >= i){
        n1 = length(x[tpp10[i-1]:tpp10[i]])  
        n2 = round(n1/3, 0)
        n3 = sample(1:n2, 1)
        start = sample(tpp10[i-1]:(tpp10[i-1]+(2*n2)), 1)  
        x[start:(start+n3)] = NA
      }else{
        if(length(tpp10) >= (i-1)){
          n1 = length(x[tpp10[i-1]:n])  
          n2 = round(n1/3, 0)
          n3 = sample(1:n2, 1)
          start = sample(tpp10[i-1]:(tpp10[i-1]+(2*n2)), 1)  
          x[start:(start+n3)] = NA          
        } 
      }    
    }
  }else{
    # start by night == 1
    if(i == 1){
      n1 = length(x[1:tpp01[1]])  
      n2 = round(n1/3, 0)
      n3 = sample(1:n2, 1)
      start = sample(1:(2*n2), 1) 
      x[start:(start+n3)] = NA
    }else{
      if(length(tpp01) >= i){
        n1 = length(x[tpp01[i-1]:tpp01[i]])  
        n2 = round(n1/3, 0)
        n3 = sample(1:n2, 1)
        start = sample(tpp01[i-1]:(tpp01[i-1]+(2*n2)), 1)  
        x[start:(start+n3)] = NA
      }else{
        if(length(tpp10) >= (i-1)){
          n1 = length(x[tpp01[i-1]:n])  
          n2 = round(n1/3, 0)
          n3 = sample(1:n2, 1)
          start = sample(tpp01[i-1]:(tpp01[i-1]+(2*n2)), 1)  
          x[start:(start+n3)] = NA          
        } 
      }    
    }
  }
  return(x)
}

remove_day = function(x,i, tpp01, tpp10){
  n = length(x)
  if(tpp01[1] < tpp10[1]){
    # start by day == 0 
    if(i == 1){
      n1 = length(x[1:tpp01[1]])  
      n2 = round(n1/3, 0)
      n3 = sample(1:n2, 1)
      start = sample(1:(2*n2), 1) 
      x[start:(start+n3)] = NA
    }else{
      if(length(tpp01) >= i){
        n1 = length(x[tpp10[i-1]:tpp01[i]])  
        n2 = round(n1/3, 0)
        n3 = sample(1:n2, 1)
        start = sample(tpp10[i-1]:(tpp10[i-1]+(2*n2)), 1)  
        x[start:(start+n3)] = NA
      }else{
        if(length(tpp01) >= (i-1)){
          n1 = length(x[tpp01[i-1]:n])  
          n2 = round(n1/3, 0)
          n3 = sample(1:n2, 1)
          start = sample(tpp10[i-1]:(tpp10[i-1]+(2*n2)), 1)  
          x[start:(start+n3)] = NA          
        } 
      }    
    }
  }else{
    # start by night == 1
    if(i == 1){
      n1 = length(x[1:tpp10[1]])  
      n2 = round(n1/3, 0)
      n3 = sample(1:n2, 1)
      start = sample(1:(2*n2), 1) 
      x[start:(start+n3)] = NA
    }else{
      if(length(tpp10) >= i){
        n1 = length(x[tpp01[i-1]:tpp10[i]])  
        n2 = round(n1/3, 0)
        n3 = sample(1:n2, 1)
        start = sample(tpp01[i-1]:(tpp01[i-1]+(2*n2)), 1)  
        x[start:(start+n3)] = NA
      }else{
        if(length(tpp10) >= (i-1)){
          n1 = length(x[tpp10[i-1]:n])  
          n2 = round(n1/3, 0)
          n3 = sample(1:n2, 1)
          start = sample(tpp01[i-1]:(tpp01[i-1]+(2*n2)), 1)  
          x[start:(start+n3)] = NA          
        } 
      }    
    }
  }
  return(x)
}

remove_night = function(x,i, tpp01, tpp10){
  n = length(x)
  if(tpp01[1] < tpp10[1]){
    # start by day == 0 
    if(i <= length(tpp10)){
      n1 = length(x[tpp01[i]:tpp10[i]])  
      n2 = round(n1/3, 0)
      n3 = sample(1:n2, 1)
      start = sample(tpp01[i]:(tpp01[i]+(2*n2)), 1) 
      x[start:(start+n3)] = NA
    }
  }
  return(x)
}

search_neighborhood = function(p, x){
  x = as.vector(x)
  n = length(x)
  n1 = 0
  n2 = 0 
  if(p+1 < n) n1 = search_right(x[(p+1):n])
  if(p-1 > 1) n2 = search_left(x[1:(p-1)])
  return(n1+n2)
}

search_right = function(x){
  x = as.vector(x)
  n = length(x)
  c = 0
  i = 1
  stop = 0 
  while(i < n && stop < 1){
    if(is.na(x[i])==TRUE){
      i = i + 1
    }else{
      stop = 1
    }
  }
  return(i)
}

search_left = function(x){
  x = as.vector(x)
  n = length(x)
  x = x[n:1]
  c = 0
  i = 1
  stop = 0 
  while(i < n && stop < 1){
    if(is.na(x[i])==TRUE){
      i = i + 1
    }else{
      stop = 1
    }
  }
  return(i)
}

imput_missing = function(acc, method="IBM", minutes=60*8){
  x = as.numeric(acc)
  x = as.vector(x)
  n = length(x)
  if(method=="IBM"){ # imputation by the average
    for(i in 1:n){
      if(is.na(x[i]) && search_neighborhood(i, x) < minutes){
        j = round(i/1440, 0)+1
        if(j==1) x[i] = mean(x[c(i+1440*2,i+1440*3,i+1440*4,i+1440*5,i+1440*6,i+1440*7)], na.rm=TRUE) 
        if(j==2) x[i] = mean(x[c(i+1440*1,i+1440*3,i+1440*4,i+1440*5,i+1440*6,i+1440*7)], na.rm=TRUE) 
        if(j==3) x[i] = mean(x[c(i+1440*2,i+1440*1,i+1440*4,i+1440*5,i+1440*6,i+1440*7)], na.rm=TRUE) 
        if(j==4) x[i] = mean(x[c(i+1440*2,i+1440*3,i+1440*1,i+1440*5,i+1440*6,i+1440*7)], na.rm=TRUE) 
        if(j==5) x[i] = mean(x[c(i+1440*2,i+1440*3,i+1440*4,i+1440*1,i+1440*6,i+1440*7)], na.rm=TRUE)
        if(j==6) x[i] = mean(x[c(i+1440*2,i+1440*3,i+1440*4,i+1440*5,i+1440*1,i+1440*7)], na.rm=TRUE) 
        if(j==7) x[i] = mean(x[c(i+1440*2,i+1440*3,i+1440*4,i+1440*5,i+1440*6,i+1440*1)], na.rm=TRUE) 
      } 
    }
  }
  return(x)
}

clean_na = function(x,y){
  x = as.numeric(x)
  x = as.vector(x)
  y = as.numeric(y)
  y = as.vector(y)
  n = length(x)
  mat = matrix(ncol=2, nrow=n)
  mat[1:n,1] = x[1:n]
  mat[1:n,2] = y[1:n]
  mat = mat[complete.cases(mat), ]
  return(mat)
}

clean_na_3v = function(x,y,z){
  x = as.numeric(x)
  x = as.vector(x)
  y = as.numeric(y)
  y = as.vector(y)
  z = as.numeric(z)
  z = as.vector(z)
  n = length(x)
  mat = matrix(ncol=3, nrow=n)
  mat[1:n,1] = x[1:n]
  mat[1:n,2] = y[1:n]
  mat[1:n,3] = z[1:n]
  mat = mat[complete.cases(mat), ]
  return(mat)
}

mc = function(Array, nday, day_33, night_33, daynight_33, minutes = 60*8, threshold=40, N=2859, name_ni="ni_1.rds", name_ibm="ibm_1.rds", start=1, end=2859){
  metrics_ibm = matrix(NA,ncol=8,nrow=N)
  metrics_ni  = matrix(NA,ncol=8,nrow=N)  
  colnames(metrics_ibm) = c("IS", "IV", "tp_arw", "tp_ars", "tp_raw", "tp_ras", "alpha", "ABI")
  colnames(metrics_ni) = c("IS", "IV", "tp_arw", "tp_ars", "tp_raw", "tp_ras", "alpha", "ABI")  
  for(i in start:end){
    awake = as.numeric(Array[1:10080,2,i]) 
    awake = awake[!is.na(awake)]
    n = length(awake)
    acc = as.numeric(Array[,1,i])
    acc = as.vector(acc)[1:n]
    sleep = as.numeric(Array[,3,i])
    sleep = as.vector(sleep)[1:n]
    sleep = ifelse(sleep==0,0,1) # opposite of sam's code: 0 means sleep and 1 means not sleep 
      if(n > 10){
        tpp01 = trans_pos(as.numeric(awake))
        tpp10 = trans_pos(as.numeric(awake), c(1,0))
        if(length(tpp01)>=2 && length(tpp10)>=2){
          print(i)
          acc_mis = create_missing(acc, awake, nday, day_33, night_33, daynight_33)
          acc_imp = imput_missing(acc_mis, method="IBM", minutes=minutes)
          tmp1 = clean_na_3v(acc_mis, awake, sleep)
          tmp2 = clean_na_3v(acc_imp, awake, sleep)      
          acc_mis_clean = tmp1[,1] 
          awk_mis_clean = tmp1[,2]
          slp_mis_clean = tmp1[,3]
          acc_imp_clean = tmp2[,1] 
          awk_imp_clean = tmp2[,2]
          slp_imp_clean = tmp2[,3]
          metrics_ibm[i,1:2] = est_isiv(acc_imp, threshold=threshold)
          #metrics_ibm[i,3:6] = trans_prob(acc_imp_clean, slp_imp_clean, 0.5)
          aux1_imp = trans_prob(acc_imp_clean, awk_imp_clean, threshold)
          aux2_imp = trans_prob(slp_imp_clean, awk_imp_clean, 0.5)          
          metrics_ibm[i,3] = aux1_imp[1] # TP ar,w
          metrics_ibm[i,4] = aux2_imp[2] # TP ar,s
          metrics_ibm[i,5] = aux1_imp[3] # TP ra,w
          metrics_ibm[i,6] = aux2_imp[4] # TP ra,s 
          #metrics_ibm[i,7] = SSP(acc_imp_clean, scale = 2^(1/8), box_size = 4, m = 1)
          #metrics_ibm[i,8] = ABI(metrics_ibm[i,7])
          metrics_ni[i,1:2] = est_isiv(acc_mis, threshold=threshold)
          #metrics_ni[i,3:6] = trans_prob(acc_mis_clean, awk_mis_clean, threshold)
          aux1_ni = trans_prob(acc_mis_clean, awk_mis_clean, threshold)
          aux2_ni = trans_prob(slp_mis_clean, awk_mis_clean, 0.5)          
          metrics_ni[i,3] = aux1_ni[1] # TP ar,w
          metrics_ni[i,4] = aux2_ni[2] # TP ar,s
          metrics_ni[i,5] = aux1_ni[3] # TP ra,w
          metrics_ni[i,6] = aux2_ni[4] # TP ra,s 
          #metrics_ni[i,7] = SSP(acc_mis_clean, scale = 2^(1/8), box_size = 4, m = 1)
          #metrics_ni[i,8] = ABI(metrics_ni[i,7])
          saveRDS(metrics_ni, file = name_ni)
          saveRDS(metrics_ibm, file = name_ibm) 
          }
      }
  }
  saveRDS(metrics_ni, file = name_ni)
  saveRDS(metrics_ibm, file = name_ibm)  
  return(list(metrics_ni,metrics_ibm)) 
}



evaluate = function(mat_hat, mat_true, criteria="bias", col=1:8){
  mat_hat = as.matrix(mat_hat[,col])
  mat_true = as.matrix(mat_true[,col])
  n = dim(mat_hat)[1]
  if(any(is.na(rbind(mat_hat, mat_true)))==TRUE) print("warning, there is NA")
  y = rep(0, length(col))
  if(criteria=="bias"){
    mat_dif = mat_hat - mat_true
    y = colMeans(mat_dif)
  }
  if(criteria=="mse"){
    mat_dif = mat_hat - mat_true
    mat_dif = (mat_dif)^2
    y = colMeans(mat_dif)
  }
  if(criteria=="mae"){
    mat_dif = mat_hat - mat_true
    mat_dif = abs(mat_dif)
    y = colMeans(mat_dif)
  }  
  if(criteria=="mape"){
    mat_dif = mat_hat - mat_true
    mat_dif = abs(mat_dif/mat_true)
    y = colMeans(mat_dif, na.rm = TRUE)*100
  }    
  if(criteria=="sd"){
    y = colStdevs(mat_hat)
  }    
  if(criteria=="var"){
    y = colVars(mat_hat)
  }
  if(criteria=="icc"){
    ncol = length(col)
    y = rep(0,ncol)
    for(i in 1:ncol){
      hat   = mat_hat[,i]
      theta = mat_true[,i]
      xbar  = mean(c(hat,theta), na.rm=TRUE)
      var   = var(c(hat,theta), na.rm=TRUE)
      y[i]  = sum( (hat-xbar) * (theta-xbar), na.rm=TRUE)/(var*length(theta)) 
      #y[i] = icc(cbind(hat, theta), model = "twoway", type = "agreement", unit = "average")$value
    }
  }
  return(y)
}

cross_cbind = function(mat_a, mat_b){
  mat_a = t(as.matrix(mat_a))
  mat_b = t(as.matrix(mat_b)) 
  n = dim(mat_a)[1]
  p = dim(mat_a)[2]
  y = matrix(ncol = 2*p, nrow=n)
  for(j in 1:p){
    y[,c(j*2-1,j*2)] = cbind(mat_a[,j],mat_b[,j])
  }
  return(y)
}

mc_table = function(mat_true, 
                    m1_c1_d3, m1_c1_d4, m1_c1_d5, m1_c1_d6, m1_c1_d7, 
                    m1_c2_d3, m1_c2_d4, m1_c2_d5, m1_c2_d6, m1_c2_d7,
                    m1_c3_d3, m1_c3_d4, m1_c3_d5, m1_c3_d6, m1_c3_d7,
                    m2_c1_d3, m2_c1_d4, m2_c1_d5, m2_c1_d6, m2_c1_d7, 
                    m2_c2_d3, m2_c2_d4, m2_c2_d5, m2_c2_d6, m2_c2_d7,
                    m2_c3_d3, m2_c3_d4, m2_c3_d5, m2_c3_d6, m2_c3_d7,                    
                    criteria="bias", col=1:8,
                    names = c("IS","IS","IV","IV","TP1","TP1","TP2","TP2","TP3","TP3",
                              "TP4","TP4","SSP","SSP","ABI","ABI")){
  mat_true = as.matrix(mat_true[,col])
  n = dim(mat_true)[1]
  p = length(col)
  m1_c1_d1 = m1_c1_d3[,col] 
  m1_c1_d2 = m1_c1_d4[,col] 
  m1_c1_d3 = m1_c1_d5[,col] 
  m1_c1_d4 = m1_c1_d6[,col]
  m1_c1_d5 = m1_c1_d7[,col]
  m1_c2_d1 = m1_c2_d3[,col] 
  m1_c2_d2 = m1_c2_d4[,col] 
  m1_c2_d3 = m1_c2_d5[,col] 
  m1_c2_d4 = m1_c2_d6[,col]
  m1_c2_d5 = m1_c2_d7[,col]
  m1_c3_d1 = m1_c3_d3[,col] 
  m1_c3_d2 = m1_c3_d4[,col] 
  m1_c3_d3 = m1_c3_d5[,col] 
  m1_c3_d4 = m1_c3_d6[,col]
  m1_c3_d5 = m1_c3_d7[,col]  
  m2_c1_d1 = m2_c1_d3[,col] 
  m2_c1_d2 = m2_c1_d4[,col] 
  m2_c1_d3 = m2_c1_d5[,col] 
  m2_c1_d4 = m2_c1_d6[,col]
  m2_c1_d5 = m2_c1_d7[,col]
  m2_c2_d1 = m2_c2_d3[,col] 
  m2_c2_d2 = m2_c2_d4[,col] 
  m2_c2_d3 = m2_c2_d5[,col] 
  m2_c2_d4 = m2_c2_d6[,col]
  m2_c2_d5 = m2_c2_d7[,col]
  m2_c3_d1 = m2_c3_d3[,col] 
  m2_c3_d2 = m2_c3_d4[,col] 
  m2_c3_d3 = m2_c3_d5[,col] 
  m2_c3_d4 = m2_c3_d6[,col]
  m2_c3_d5 = m2_c3_d7[,col]  
  r1_c1_d1 = evaluate(m1_c1_d1, mat_true, criteria = criteria, col=col)
  r1_c1_d2 = evaluate(m1_c1_d2, mat_true, criteria = criteria, col=col)
  r1_c1_d3 = evaluate(m1_c1_d3, mat_true, criteria = criteria, col=col)
  r1_c1_d4 = evaluate(m1_c1_d4, mat_true, criteria = criteria, col=col)
  r1_c1_d5 = evaluate(m1_c1_d5, mat_true, criteria = criteria, col=col)
  r1_c2_d1 = evaluate(m1_c2_d1, mat_true, criteria = criteria, col=col)
  r1_c2_d2 = evaluate(m1_c2_d2, mat_true, criteria = criteria, col=col)
  r1_c2_d3 = evaluate(m1_c2_d3, mat_true, criteria = criteria, col=col)
  r1_c2_d4 = evaluate(m1_c2_d4, mat_true, criteria = criteria, col=col)  
  r1_c2_d5 = evaluate(m1_c2_d5, mat_true, criteria = criteria, col=col)  
  r1_c3_d1 = evaluate(m1_c3_d1, mat_true, criteria = criteria, col=col)
  r1_c3_d2 = evaluate(m1_c3_d2, mat_true, criteria = criteria, col=col)
  r1_c3_d3 = evaluate(m1_c3_d3, mat_true, criteria = criteria, col=col)
  r1_c3_d4 = evaluate(m1_c3_d4, mat_true, criteria = criteria, col=col)
  r1_c3_d5 = evaluate(m1_c3_d5, mat_true, criteria = criteria, col=col)
  r2_c1_d1 = evaluate(m2_c1_d1, mat_true, criteria = criteria, col=col)
  r2_c1_d2 = evaluate(m2_c1_d2, mat_true, criteria = criteria, col=col)
  r2_c1_d3 = evaluate(m2_c1_d3, mat_true, criteria = criteria, col=col)
  r2_c1_d4 = evaluate(m2_c1_d4, mat_true, criteria = criteria, col=col)
  r2_c1_d5 = evaluate(m2_c1_d5, mat_true, criteria = criteria, col=col)
  r2_c2_d1 = evaluate(m2_c2_d1, mat_true, criteria = criteria, col=col)
  r2_c2_d2 = evaluate(m2_c2_d2, mat_true, criteria = criteria, col=col)
  r2_c2_d3 = evaluate(m2_c2_d3, mat_true, criteria = criteria, col=col)
  r2_c2_d4 = evaluate(m2_c2_d4, mat_true, criteria = criteria, col=col)  
  r2_c2_d5 = evaluate(m2_c2_d5, mat_true, criteria = criteria, col=col)  
  r2_c3_d1 = evaluate(m2_c3_d1, mat_true, criteria = criteria, col=col)
  r2_c3_d2 = evaluate(m2_c3_d2, mat_true, criteria = criteria, col=col)
  r2_c3_d3 = evaluate(m2_c3_d3, mat_true, criteria = criteria, col=col)
  r2_c3_d4 = evaluate(m2_c3_d4, mat_true, criteria = criteria, col=col)
  r2_c3_d5 = evaluate(m2_c3_d5, mat_true, criteria = criteria, col=col)
  cross_c1_d1 = cross_cbind(r1_c1_d1, r2_c1_d1)
  cross_c1_d2 = cross_cbind(r1_c1_d2, r2_c1_d2)
  cross_c1_d3 = cross_cbind(r1_c1_d3, r2_c1_d3)
  cross_c1_d4 = cross_cbind(r1_c1_d4, r2_c1_d4)
  cross_c1_d5 = cross_cbind(r1_c1_d5, r2_c1_d5)
  cross_c2_d1 = cross_cbind(r1_c2_d1, r2_c2_d1)
  cross_c2_d2 = cross_cbind(r1_c2_d2, r2_c2_d2)
  cross_c2_d3 = cross_cbind(r1_c2_d3, r2_c2_d3)
  cross_c2_d4 = cross_cbind(r1_c2_d4, r2_c2_d4)
  cross_c2_d5 = cross_cbind(r1_c2_d5, r2_c2_d5)
  cross_c3_d1 = cross_cbind(r1_c3_d1, r2_c3_d1)
  cross_c3_d2 = cross_cbind(r1_c3_d2, r2_c3_d2)
  cross_c3_d3 = cross_cbind(r1_c3_d3, r2_c3_d3)
  cross_c3_d4 = cross_cbind(r1_c3_d4, r2_c3_d4)
  cross_c3_d5 = cross_cbind(r1_c3_d5, r2_c3_d5)
  y = rbind(cross_c1_d1,cross_c1_d2,cross_c1_d3,cross_c1_d4,cross_c1_d5,
            cross_c2_d1,cross_c2_d2,cross_c2_d3,cross_c2_d4,cross_c2_d5,
            cross_c3_d1,cross_c3_d2,cross_c3_d3,cross_c3_d4,cross_c3_d5)
  colnames(y) = names
  return(y)
}

plot_ts = function(X, metric=1, range=1, name="MAPE", title=""){
  col1 = metric*2-1
  col2 = metric*2
  lim1 = c(1:5)
  lim2 = lim1+5  
  lim3 = lim1+10  
  x = 3:7
  y_ni_1 = X[lim1,col1]
  y_ni_2 = X[lim2,col1]
  y_ni_3 = X[lim3,col1]
  y_ibm_1 = X[lim1,col2] 
  y_ibm_2 = X[lim2,col2] 
  y_ibm_3 = X[lim3,col2] 
  mm = max(X)
  mi = min(X)
  plot(x, c(mm, mi,rep(0,3)),type="n", xlab="N Days", ylab = name, main=title, frame.plot=FALSE)
  abline(h=10, lty=3)
  lines(x,y_ni_1, lwd=2, col="pink")
  lines(x,y_ni_2, lwd=2, col="red")
  lines(x,y_ni_3, lwd=2, col="brown")
  lines(x,y_ibm_1, lty=2, col="lightblue", lwd=2)
  lines(x,y_ibm_2, lty=2, col="blue", lwd=2)
  lines(x,y_ibm_3, lty=2, col="darkblue", lwd=2)
}

plot_ts_1pair = function(X, metric=1, range=1, name="MAPE", title=""){
  col1 = metric*2-1
  col2 = metric*2
  lim1 = c(1:5)
  lim2 = lim1+5  
  lim3 = lim1+10  
  x = 3:7
#  y_ni_1 = X[lim1,col1]
#  y_ni_2 = X[lim2,col1]
  y_ni_3 = X[lim3,col1]
#  y_ibm_1 = X[lim1,col2] 
#  y_ibm_2 = X[lim2,col2] 
  y_ibm_3 = X[lim3,col2] 
  mm = max(X)
  mi = min(X)
  plot(x, c(mm, mi,rep(0,3)),type="n", xlab="N Days", ylab = name, main=title, frame.plot=FALSE)
  abline(h=10, lty=3)
#  lines(x,y_ni_1, lwd=2, col="pink")
#  lines(x,y_ni_2, lwd=2, col="red")
  lines(x,y_ni_3, lwd=2, col="brown")
#  lines(x,y_ibm_1, lty=2, col="lightblue", lwd=2)
#  lines(x,y_ibm_2, lty=2, col="blue", lwd=2)
  lines(x,y_ibm_3, lty=2, col="darkblue", lwd=2)
}

plot_ts_2pair = function(X, metric=1, range=1, name="MAPE", title=""){
  col1 = metric*2-1
  col2 = metric*2
  lim1 = c(1:5)
  lim2 = lim1+5  
  lim3 = lim1+10  
  x = 3:7
  y_ni_1 = X[lim1,col1]
  y_ni_2 = X[lim2,col1]
  #y_ni_3 = X[lim3,col1]
  y_ibm_1 = X[lim1,col2] 
  y_ibm_2 = X[lim2,col2] 
  #y_ibm_3 = X[lim3,col2] 
  mm = max(X)
  mi = min(X)
  plot(x, c(mm, mi,rep(0,3)),type="n", xlab="N Days", ylab = name, main=title, frame.plot=FALSE)
  abline(h=10, lty=3)
  lines(x,y_ni_1, lwd=2, col="pink")
  lines(x,y_ni_2, lwd=2, col="red")
  #lines(x,y_ni_3, lwd=2, col="brown")
  lines(x,y_ibm_1, lty=2, col="lightblue", lwd=2)
  lines(x,y_ibm_2, lty=2, col="blue", lwd=2)
  #lines(x,y_ibm_3, lty=2, col="darkblue", lwd=2)
}

plot_ts_simple = function(X, metric=1, range=1, name="MAPE", title="", NI=TRUE){
  col1 = metric*2-1
  col2 = metric*2
  lim1 = c(1,5,13,21,25)
  lim2 = lim1+1  
  lim3 = lim1+2  
  lim4 = lim1+3  
  x = 3:7
  y_ni_1 = X[lim1,col1]
  y_ni_2 = X[lim2,col1]
  y_ni_3 = X[lim3,col1]
  y_ni_4 = X[lim4,col1]
  y_ibm_1 = X[lim1,col2] 
  y_ibm_2 = X[lim2,col2] 
  y_ibm_3 = X[lim3,col2] 
  y_ibm_4 = X[lim4,col2] 
  mm = max(X)
  mi = min(X)
  plot(x, c(mm, mi,rep(0,3)),type="n", xlab="N Days", ylab = name, main=title, frame.plot=FALSE)
  if(range==1){ # for MAPE
    polygon(c(0,0,8,8,0),c(-20,20,20,-20,-20), col="gray90", border = NA)
    polygon(c(0,0,8,8,0),c(-10,10,10,-10,-10), col="gray75", border = NA)
    abline(h=0, lty=3)
    abline(h=50, lty=3)    
  }
  if(range==2){ # for Bias
    con = 0.15
    polygon(c(0,0,8,8,0),c(-con,con,con,-con,-con), col="gray90", border = NA)
    abline(h=0, lty=3)
  }
  if(NI == TRUE){
    lines(x,y_ni_1, lwd=2, col="pink")
    lines(x,y_ni_2, lwd=2, col="coral")
    lines(x,y_ni_3, lwd=2, col="red")
    lines(x,y_ni_4, lwd=2, col="brown")
  }else{
    lines(x,y_ibm_1, lty=2, col="cyan", lwd=2)
    lines(x,y_ibm_2, lty=2, col="aquamarine", lwd=2)
    lines(x,y_ibm_3, lty=2, col="blue", lwd=2)
    lines(x,y_ibm_4, lty=2, col="darkblue", lwd=2)    
  }
}

plot_ts_unique = function(X, metric=1, range=1, name="MAPE", title="", NI=TRUE){
  col1 = metric*2-1
  col2 = metric*2
  lim1 = c(1,5,13,21,25)
  lim2 = lim1+1  
  lim3 = lim1+2  
  lim4 = lim1+3  
  x = 3:7
  y_ni_1 = X[lim1,col1]
  y_ni_2 = X[lim2,col1]
  y_ni_3 = X[lim3,col1]
  y_ni_4 = X[lim4,col1]
  y_ibm_1 = X[lim1,col2] 
  y_ibm_2 = X[lim2,col2] 
  y_ibm_3 = X[lim3,col2] 
  y_ibm_4 = X[lim4,col2] 
  mm = max(X)
  mi = min(X)
  plot(x, c(mm, mi,rep(0,3)),type="n", xlab="N Days", ylab = name, main=title, frame.plot=FALSE)
  if(range==1){ # for MAPE
    polygon(c(0,0,8,8,0),c(-20,20,20,-20,-20), col="gray90", border = NA)
    polygon(c(0,0,8,8,0),c(-10,10,10,-10,-10), col="gray75", border = NA)
    abline(h=0, lty=3)
    abline(h=50, lty=3)    
  }
  if(range==2){ # for Bias
    con = 0.15
    polygon(c(0,0,8,8,0),c(-con,con,con,-con,-con), col="gray90", border = NA)
    abline(h=0, lty=3)
  }
  if(NI == TRUE){
    #lines(x,y_ni_1, lwd=2, col="pink")
    #lines(x,y_ni_2, lwd=2, col="coral")
    #lines(x,y_ni_3, lwd=2, col="red")
    lines(x,y_ni_4, lwd=2, col="brown")
  }else{
    #lines(x,y_ibm_1, lty=2, col="cyan", lwd=2)
    #lines(x,y_ibm_2, lty=2, col="aquamarine", lwd=2)
    #lines(x,y_ibm_3, lty=2, col="blue", lwd=2)
    lines(x,y_ibm_4, lty=2, col="darkblue", lwd=2)    
  }
}

box_diff = function(X, diff="DN", title="", summary=FALSE){
  if(diff=="DN"){ # day night
    lim1 = c(2,6,10,14,18,22,26)
    lim2 = c(3,7,11,15,19,23,27)
    x = X[lim2,] - X[lim1,]
    y = matrix(ncol = 8, nrow = 14)
    y[,1] = c(x[,1:2])
    y[,2] = c(x[,3:4])
    y[,3] = c(x[,5:6])
    y[,4] = c(x[,7:8])
    y[,5] = c(x[,9:10])
    y[,6] = c(x[,11:12])
    y[,7] = c(x[,13:14])
    y[,8] = c(x[,15:16])
    if(is.vector(title)==TRUE){
      #tt = c(title[1],"",title[2],"",title[3],"",title[4],"",title[5],"",title[6],"",title[7],"",title[8],"")
      #boxplot(x, frame.plot = FALSE, names=tt, col=c("coral","aquamarine"))
      boxplot(y, frame.plot = FALSE, names=title, col="aquamarine", ylab = "MAPE (%)")
      box(bty="l")
      if(summary==TRUE) print(boxplot(y, plot=FALSE)$stats)
    }else boxplot(x)      
  }
  if(diff=="WE"){
    lim1 = c(5,6,7,8,13,14,15,16)
    lim2 = c(9,10,11,12,17,18,19,20)
    x = X[lim1,] - X[lim2,]
    y = matrix(ncol = 8, nrow = 16)
    y[,1] = c(x[,1:2])
    y[,2] = c(x[,3:4])
    y[,3] = c(x[,5:6])
    y[,4] = c(x[,7:8])
    y[,5] = c(x[,9:10])
    y[,6] = c(x[,11:12])
    y[,7] = c(x[,13:14])
    y[,8] = c(x[,15:16])
    if(is.vector(title)==TRUE){
#      tt = c(title[1],"",title[2],"",title[3],"",title[4],"",title[5],"",title[6],"",title[7],"",title[8],"")
#      boxplot(x, frame.plot = FALSE, names=tt, col=c(2,4))
      boxplot(y, frame.plot = FALSE, names=title, col="bisque", ylab = "MAPE (%)")
      box(bty="l")
      if(summary==TRUE) print(boxplot(y, plot=FALSE)$stats)
    }else boxplot(x)      
  }
  if(diff=="IBM"){
    col1 = (1:8)*2-1
    col2 = (1:8)*2
    x = X[,col1] - X[,col2]
    if(is.vector(title)==TRUE){
      boxplot(x, frame.plot = FALSE, names=title, col="darkorchid", ylab = "MAPE (%)")
      box(bty="l")
      if(summary==TRUE) print(boxplot(x, plot=FALSE)$stats)  
    }else boxplot(x)      
  }
  abline(h=0, lty=3)
 # abline(h=10, lty=3)
#  abline(h=-10, lty=3)
}


# unbalanced Day Night - balanced Day Night
#box_diff(tab_mape, diff="DN", title=names8, summary = TRUE)
# unbalanced Weekdays - balanced week weekend
#box_diff(tab_mape, diff="WE", title=names8, summary = TRUE)
# not imput - imputed
#box_diff(tab_mape, diff="IBM", title=names8, summary = TRUE)


