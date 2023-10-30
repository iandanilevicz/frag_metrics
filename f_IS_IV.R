## --------------------------------------------------------------------------##
## Detrended Fluctuation Analysis (DFA)
## see
## --------------------------------------------------------------------------##
#library(nonlinearTseries) # dfa

min_2_hour = function(x, threshold=40){
  x = as.vector(x)
  x = ifelse(x > threshold, 1, 0)
  n = length(x)
  n_days = round(n/(24*60), 0) #floor(n/(24*60)) # warning! 
  n_hour = 24*n_days
  y = rep(0, 24*n_days)
  for(i in 0:(n_hour-1)){
    if(all(is.na(x[(i*60+1):((i+1)*60)]))==TRUE){
      y[i+1] = NA
    }else{
      y[i+1] = mean(x[(i*60+1):((i+1)*60)], na.rm=TRUE)
    }
  }
  y = ifelse(is.nan(y), NA, y) 
  y = matrix(y, ncol = 24, nrow=n_days, byrow = TRUE)
  return(y)
}

count_cols = function(mat){
  c = 0
  p = dim(mat)[2]
  for(j in 1:p){
    if(any(!is.na(mat[,j]))==TRUE){
      c = c + 1
    }  
  }
  return(c)  
}

estimate_is = function(x, threshold=40){
    x = min_2_hour(x, threshold)
    p = count_cols(x)
    xh = colMeans(x, na.rm = TRUE)
    v = c(t(x))
    n = length(v[!is.na(v)])
    numerator = sum((xh - mean(v, na.rm = TRUE))^2, na.rm = TRUE)/p
    denominator = sum((v - mean(v, na.rm = TRUE))^2, na.rm = TRUE)/n
    y = ifelse(denominator>0, numerator/denominator,0)
    return(y)
}

estimate_iv = function(x, threshold=40){
    x = min_2_hour(x, threshold)
    v = c(t(x))
    n = length(v[!is.na(v)])
    mean.counts = mean(v, na.rm=TRUE)
    numerator = sum(diff(v)^2, na.rm=TRUE)/(n - 1)
    denominator = sum((v - mean.counts)^2, na.rm=TRUE)/n
    y = numerator/denominator
    return(y)
}

estimate_phi = function(x, threshold=40){
  x = min_2_hour(x, threshold)
  v = c(t(x))
  v = v[!is.na(v)]
  model = arima(v, order=c(1,0,0))
  phi = model$coef[[1]]
  return(phi)
}

est_isiv = function(x, threshold=40){
  isiv = rep(NA,2)
  isiv[1] = estimate_is(x, threshold)
  isiv[2] = estimate_iv(x, threshold)    
  return(isiv)
}

est_isivphi = function(x, threshold=40){
    isiv = rep(NA,3)
    isiv[1] = estimate_is(x, threshold)
    isiv[2] = estimate_iv(x, threshold)    
    isiv[3] = estimate_phi(x, threshold)    
  return(isiv)
}



