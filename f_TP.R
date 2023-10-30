## ---------------------------------------------------------------------------##
## Transition probability
## See Lim2011 and Di2017
## Developer Ian Danilevicz
## ---------------------------------------------------------------------------##

f_slice = function(x,w){
  n = length(x)
  y = rep(0,n)
  for(i in 1:n){
    if(w[i]==1) y[i] = x[i]
  }
  return(y)
}

f_rad_act_awake = function(x,w,delta=40, epsilon=0.5){
  # reciprocal average duration RAD of activity during awake period 
  n = length(x)
  y = ifelse(x>delta,1,0)
  w = ifelse(w==1,0,1)
  y = f_slice(y,w) 
  total = sum(y)+epsilon
  nbout = epsilon
  for(i in 1:(n-1)){
    if(y[i]==1 && y[i+1]==0) nbout = nbout + 1
  }
  r = nbout/total 
  return(r) 
}

f_rad_act_sleep = function(x,w,delta=40, epsilon=0.5){
  # reciprocal average duration RAD of activity during sleep period 
  n = length(x)
  y = ifelse(x>delta,1,0)
  y = f_slice(y,w) 
  total = sum(y)+epsilon
  nbout = epsilon
  for(i in 1:(n-1)){
    if(y[i]==1 && y[i+1]==0) nbout = nbout + 1
  }
  r = nbout/total 
  return(r) 
}

f_rad_rest_awake = function(x,w,delta=40, epsilon=0.5){
  # reciprocal average duration RAD of rest during awake period 
  n = length(x)
  y = ifelse(x<=delta,1,0)
  w = ifelse(w==1,0,1)
  y = f_slice(y,w) 
  total = sum(y)+epsilon
  nbout = epsilon
  for(i in 1:(n-1)){
    if(y[i]==1 && y[i+1]==0) nbout = nbout + 1
  }
  r = nbout/total 
  return(r) 
}

f_rad_rest_sleep = function(x,w,delta=40, epsilon=0.5){
  # reciprocal average duration RAD of rest during sleep period 
  n = length(x)
  y = ifelse(x<=delta,1,0)
  y = f_slice(y,w) 
  total = sum(y)+epsilon
  nbout = epsilon
  for(i in 1:(n-1)){
    if(y[i]==1 && y[i+1]==0) nbout = nbout + 1
  }
  r = nbout/total 
  return(r) 
}

trans_prob = function(x,sleepperiod,delta=40){
    rad4 = rep(NA,4)
    rad4[1] = f_rad_act_awake(x,sleepperiod, delta)
    rad4[2] = f_rad_act_sleep(x,sleepperiod, delta)
    rad4[3] = f_rad_rest_awake(x,sleepperiod, delta)
    rad4[4] = f_rad_rest_sleep(x,sleepperiod, delta)     
  return(rad4)
}


