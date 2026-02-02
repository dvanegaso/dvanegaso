

# primer método

call_binom = function(n,u,d,p,q, S_t, K, r_f, tau){
  temp = numeric(n+1)
  for (i in 0:n) {
    temp[i+1] = choose(n,i) * (p^(n - i)) * (q^i) * max(((u^(n - i)) * (d^i) * S_t) - K,0)
  }
  return(c_t = sum(temp)/(exp(r_f*tau)))
}

# colocar tau, n, S_t, K



binom1 = function(r_f,tau,n,volatilidad,S_t,K){
  sg = volatilidad
  dt = tau/n
  M = (1+r_f)^(dt)
  V = sg^2 * dt
  u = ((V + M^2 + 1) + sqrt((V + M^2 + 1)^2 - 4*M^2))/(2*M)
  d = ((V + M^2 + 1) - sqrt((V + M^2 + 1)^2 - 4*M^2))/(2*M)
  p = (M-d)/(u-d); q = 1-p
  c_t = call_binom(n,u,d,p,q,S_t, K, r_f, tau)
  cat("Método 2 ----","\n")
  cat(" p_1",p,"\n")
  cat(" p_2",q,"\n")
  cat("call",c_t,"\n")
  cat("-------------","\n")
}

# segundo método

binom2 = function(r_f,tau,n,volatilidad,S_t,K){
  sg = volatilidad
  dt = tau/n
  M = dt * log(1+r_f); V = sg^2 * dt
  u = exp(sg * sqrt(dt)); d = 1/u
  p = (log(u) + M)/(2*log(u)); q = 1-p
  c_t = call_binom(n,u,d,p,q,S_t, K, r_f, tau)
  cat("Método 2 ----","\n")
  cat(" p_1",p,"\n")
  cat(" p_2",q,"\n")
  cat("call",c_t,"\n")
  cat("-------------","\n")
}
binom1(r_f = 0.12, tau = 1/4, n = 10, volatilidad = 0.1, S_t = 100, K = 100)
binom2(r_f = 0.12, tau = 1/4, n = 10, volatilidad = 0.1, S_t = 100, K = 100)

# arbol trinomial

# uso la expresión 6.97 

call_trinom = function(n,u,h,d,p_1,p_2,p_3,S_t,K, r_f, tau){
  temp = 0
  for (i in 0:n) {
    for (j in 0:(n-i)) {
      k = n - j - i
      temp = temp + choose(n,i) * choose((n-i),j) * (p_1^i) * (p_2^j) * (p_3^k) * max(((u^i)*(h^j)*(d^k)*S_t) - K,0)
    }
  }
  return(c_t = sum(temp)/exp(r_f*tau))
}


# primer metodo


# metodo BIN

BINOM = function(r_f,tau,n,volatilidad,S_t,K){
  
  dt = tau/n
  sg = volatilidad
  M = exp(r_f*dt); w = exp(sg^2 * dt); V = (M^2)*(w-1);
  z1 = sqrt((w-1)*(w+3));
  (h = w*M)
  (u = (h/2)*(w+1+z1))
  (d = (h/2)*(w+1-z1))
  (p_1 = (1/2) - ((w+2)/(2*w))*sqrt((w-1)/(w+3)))
  (p_2 = 0)
  (p_3 = (1/2) + ((w+2)/(2*w))*sqrt((w-1)/(w+3)))
  c_t = call_trinom(n,u,h,d,p_1,p_2,p_3, S_t= S_t, K = K, r_f = r_f, tau = tau)
  return(c(p_1,p_2,p_3,c_t))
}


# modelo Boyle

BOYLE = function(r_f,tau,n,volatilidad,S_t,K){
  
  (dt = tau/n)
  sg = volatilidad
  M = exp(r_f*dt); w = exp(sg^2 * dt); V = (M^2)*(w-1); dt = tau/n
  z1 = sqrt((w-1)*(w+3));z2 = sqrt((w-1)*(w+3)*((w^2) + w + 2))
  (h = 1)
  (a = ((M*w)^2 + 1 + M*(w-1))/M)
  gamma = ((M*w-1)^2)/M
  zeta = sqrt((gamma + 3*w - 3)*(gamma + 3*w + 1))
  (u = (gamma + 3*w - 1 + zeta)/2)
  (d = (gamma + 3*w - 1 - zeta)/2)
  (p_2 = ((M*w + 1)*V)/((M*w - 1)^2 + 3*((w-1)*M)))
  (p_3 = (u-M)/(u-d) - p_2/(d+h))
  (p_1 = 1 - p_2 - p_3)
  c_t = call_trinom(n,u,h,d,p_1,p_2,p_3, S_t= S_t, K = K, r_f = r_f, tau = tau)
  
  return(c(p_1,p_2,p_3,c_t))
}

# modelo TIAN1

TIAN1 = function(r_f,tau,n,volatilidad,S_t,K){
  
  dt = tau/n
  sg = volatilidad
  M = exp(r_f*dt); w = exp(sg^2 * dt); V = (M^2)*(w-1); dt = tau/n
  z11 = sqrt((w-1)*(w+2)*((w^2) + w + 2))
  h = M*w
  u = (h/2)*(((w+1)*w) + z11)
  d = (h/2)*(((w+1)*w) - z11)
  p_1 = d/((u+h)*w*(w+2))
  p_2 = (w+1)/(w*(w+2))
  p_3 = u/((h+d)*w*(w+2))
  c_t = call_trinom(n,u,h,d,p_1,p_2,p_3, S_t= S_t, K = K, r_f = r_f, tau = tau)
  
  return(c(p_1,p_2,p_3,c_t))
}


# modelo TIAN2

TIAN2 = function(r_f,tau,n,volatilidad,S_t,K){
  
  dt = tau/n
  sg = volatilidad
  M = exp(r_f*dt); w = exp(sg^2 * dt); V = (M^2)*(w-1); dt = tau/n
  z22 = sqrt(((w^2)-1)*(w+(2*sqrt(w))+3))
  h = M*(w*sqrt(w))
  u = (h/2)*((w+1)*(sqrt(w)+1)-2+z22)
  d = (h/2)*((w+1)*(sqrt(w)+1)-2-z22)
  a = 1/(h-(M*w))
  p_1 = (((w+(sqrt(w))+1)*d)-M*w)/((u-d)*(u-h)*w*a)
  p_2 = ((sqrt(w)+1)^2)/((w^2)*(w+(2*sqrt(w))+3))
  p_3 = (((w+(sqrt(w))+1)*u)-M*w)/((u-d)*(h-d)*w*a)
  c_t = call_trinom(n = n,u = u,h = h,d = d,p_1 = p_1,p_2 = p_2,p_3 = p_3, S_t= S_t, K = K, r_f = r_f, tau = tau)
  
  return(c(p_1,p_2,p_3,c_t))
}



# modelo TIAN3

TIAN3 = function(r_f,tau,n,volatilidad,S_t,K){
  
  dt = tau/n
  sg = volatilidad
  M = exp(r_f*dt); w = exp(sg^2 * dt); V = (M^2)*(w-1); dt = tau/n
  z11 = sqrt((w-1)*(w+2)*((w^2) + w + 2))
  h = M*(w^2)
  u = (h/2)*(((w+1)*w) + z11)
  d = (h/2)*(((w+1)*w) - z11)
  p_1 = (((w+1)*d)-M*w)/((w^2)*(w+2)*(u+h))
  p_2 = (w+1)/((w^3)*(w+2))
  p_3 = (((w+1)*u)-M*w)/((w^2)*(w+2)*(h+d))
  c_t = call_trinom(n,u,h,d,p_1,p_2,p_3, S_t= S_t, K = K, r_f = r_f, tau = tau)

  return(c(p_1,p_2,p_3,c_t))
}

BINOM = t(matrix(BINOM(r_f = 0.05, n = 10, tau = 4/12, volatilidad = 0.3, S_t = 100, K = 100)))
BOYLE = t(matrix(BOYLE(r_f = 0.05, n = 10, tau = 4/12, volatilidad = 0.3, S_t = 100, K = 100)))
TIAN1 = t(matrix(TIAN1(r_f = 0.05, n = 10, tau = 4/12, volatilidad = 0.3, S_t = 100, K = 100)))
TIAN2 = t(matrix(TIAN2(r_f = 0.05, n = 10, tau = 4/12, volatilidad = 0.3, S_t = 100, K = 100)))
TIAN3 = t(matrix(TIAN3(r_f = 0.05, n = 10, tau = 4/12, volatilidad = 0.3, S_t = 100, K = 100)))

tabla = rbind(BINOM,BOYLE,TIAN1,TIAN2, TIAN3); colnames(tabla) = c("P_1", "P_2","P_3", "C_t")
rownames(tabla) = c("BINOM","BOYLE","TIAN1","TIAN2","TIAN3")

knitr::kable(tabla)


#r_f = 0.05; n = 10; tau = 4/12; volatilidad = 0.3; S_t = 100; K = 100