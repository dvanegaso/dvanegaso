

val_cont = function(t,T,S_t,r_f,K,sg,D_t){
  
  tau = T - t
  S_T = exp(r_f * tau) * S_t
  
  d = log( S_T / K )/(sg * sqrt(tau)) + c(1,-1) * (( sg * sqrt(tau) )/2)
  
  c_t = S_t * pnorm( d[1] ) - K * exp(-r_f * tau) * pnorm( d[2] )
  
  (c_t)
  
  (p_t = K * exp(-r_f * tau) + c_t - S_t + D_t)
  
  (delta_c = pnorm(d[1]))
  
  (delta_p = -pnorm(-d[1]))
  
  (gamma = dnorm(d[1])/(sg * sqrt(tau) * S_t))
  
  (theta_c = -(S_t * sg)/(2 * sqrt(tau)) * dnorm(d[1]) - r_f * K * exp(-r_f * tau) * pnorm(d[2]))
  
  (theta_p = theta_c + r_f * K * exp(-r_f * tau))
  
  (rho_c = tau * K * exp(-r_f * tau) * pnorm(d[2]))
  
  (rho_p = - tau * K * exp(-r_f * tau) * pnorm(-d[2]))
  
  (vega = S_t * sqrt(tau) * dnorm(d[1])) # no vaya a ser el dr hayden ajsjsajsajasj
  
  cat("Call        =", c_t, "\n")
  cat("Put         =", p_t, "\n")
  cat("Delta(Call) =", delta_c, "\n")
  cat("Delta(Put)  =", delta_p, "\n")
  cat("Gamma       =", gamma, "\n")
  cat("Theta(Call) =", theta_c, "\n")
  cat("Theta(Put)  =", theta_p, "\n")
  cat("Rho(Call)   =", rho_c, "\n")
  cat("Rho(Put)    =", rho_p, "\n")
  cat("Vega        =", vega, "\n")
}


val_cont(t = 0, T = 10/12, S_t = 160, r_f = 0.15,K = 158, sg = 0.0408, D_t = 0)
