library(GA)

portfolio_returns_i = function(data = final_assets_returns, x) {#, data1 = dt_optimal_returns) {
  # browser()
  port.returns = 0
  # port.returns = data1[nrow(data1)]$optimal_returns_i

  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + data[,i] * x[i]
  }

  return (port.returns)
}

portfolio_returns = function(x) {
  
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + final_assets_returns[,i] * x[i]
  }
  
  return (port.returns)
}

portfolio_returns_const = function(x = rep(0.1,10)) {
  # browser()
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + a[,i] * x[i]
  }
  
  return (port.returns)
}

sharpe = function(x) {
  # browser()
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}

constraint = function(x) {
  # browser()
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  # browser()
  return(-sharpe(x)+100*constraint(x))
}

ga_optimize <- function(final_assets_returns){
  ga_res = ga(
    # Tell the genetic algorithm that the 
    # weights are real variables
    type="real-valued",
    
    # "ga" function performs maximization, so we must
    # multiply the objective function by -1
    fitness = function(x){-obj(x)}, 
    
    # x_i >= 0
    lower = rep(0,ncol(final_assets_returns)), 
    
    # x_i <= 1
    upper = rep(1,ncol(final_assets_returns)), 
    
    # Maximum number of iterations 
    maxiter = 50000, 
    
    # If the maximum fitness remains the same for 50
    # consecutive transactions, stop the algorithm
    run=50, 
    
    # Exploit multi-core properties of your CPU
    parallel=TRUE,
    
    # We want to see the partial results of the process
    # while it performs
    monitor=TRUE, 
    
    # Seed useful for replicating the results
    seed=1
  )
}
# # Store the resulting weights in a vector
# sol = as.vector(summary(ga_res)$solution)
# 
# optimal_returns = portfolio_returns(x = sol)
# 
# plot(cumsum(optimal_returns),type="l",lwd=5)
# lines(cumsum(a[,1]),col="blue")
# lines(cumsum(a[,2]),col="red")
# lines(cumsum(a[,3]),col="green")
# lines(cumprod(a[,4]),col="violet")
# lines(cumsum(a[,5]),col="peru")
# lines(cumsum(a[,6]),col="pink")
# legend(0,1.5,legend=c("Weighted portfolio",names(a)),
#        col = c("black","blue","red","green","violet","peru","pink"),lty=1)
# 
# # jezeli nie chce brac pc1 to mozna teraz dorobic maszynke zeby wybieralo wg wyjasnionej wariancji rzedu 0.6-0.8 i zrobic cos w rodzaju sumy rang ale wydaje mi sie ze kazdy wymiar ma inne cechy jak klastry wiec to nie jest jednolite
# # zrobic symulacje portfela
# x <- data.table(R = optimal_returns)
# start = 10000
# 
# for(i in 1:nrow(x)){
#   start = start + start * x[i,]$R
# }
# 
# #TODO
# ## zrobic analize w czasie
# # np za ostatni rok czyli ustawiam date pocz Sys.Date -365
# # obliczam co ma sie znalezc w portfelu i udzial za okres 1
# # wyznaczam nowe aktywa(zmieniam co miesiac nawet aktywa)/vs te same za okres 2-12
# # porownuje
# ###### SYMULACJA ######
# # table with asset name and % to invest
# asset_weights <- data.table(asset = names(a), to_invest = sol)
# # create table from prices ly with period id
# n_for_id = nrow(prices_last_year)/12
# prices_last_year$id <- rep(1:ceiling(nrow(prices_last_year)/12), each = n_for_id, length.out = nrow(prices_last_year))
# prices_last_year[id <= 12]
# # 
# dt_optimal_returns <- data.table()
# 
# i = 1
# prices_month_i <- prices_last_year[id == i, ..pc1_10]
# returns_month_i <- ror_daily(prices_month_i)
# optimal_returns_i <- portfolio_returns(data = data.frame(returns_month_i), asset_weights$to_invest)
# dt_optimal_returns <- rbind(dt_optimal_returns, data.table(month = i, optimal_returns_i))
# # nastepny prices month i tylko tak zeby nie bral pierwszych tylu dni ile mamy w prices_month_i
# prices_to_sim1 <- rbind(prices_to_sim[nrow(prices_month_i):nrow(prices_to_sim)], prices_last_year[id == i,])
# #i nastepna iteracja
# 
# plot(cumsum(optimal_returns_i),type="l",lwd=5)