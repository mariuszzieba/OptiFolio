#ROR daily
ror_daily <- function(daily_prices){
  daily_prices <- data.table(daily_prices)
  #do it for every column
  dt1 <- data.table()
  for(h in colnames(daily_prices)){
    # calculate ror in the column
    i = 2
    dt <- data.table()
    while(i < nrow(daily_prices)){
      x <- daily_prices[i, get(h)]/daily_prices[i-1, get(h)]-1
      dt <- rbind(dt, x)
      i <- i + 1
    }
    colnames(dt) <- h
    dt1 <- cbind(dt1, dt)
    print(h)
  }
  return(dt1)
}

# y <- prices#[,1:10]   y <- dplyr::select(prices_to_sim, -c(DATE, id))
# y <- ror_daily(y)
# z <- cor(y)

# library(factoextra)
# install.packages("ggbiplot")
prepare_assets <- function(cor_matrix){
  res.pca <- prcomp(cor_matrix, center = TRUE, scale = TRUE)
  # summary(res.pca)
  dt <- data.table(res.pca$rotation)
  dt$name <- rownames(res.pca$rotation)
  
  # sprawdz korelacje miedzy 10 
  pc1_10 <- dt[,.(PC1), by = name][order(PC1)]
  pc1_10 <- tail(pc1_10, 10)[,1]$name 
  final_assets_returns <- prices[, pc1_10]
  final_assets_returns <- ror_daily(final_assets_returns)
  final_assets_returns <- data.frame(final_assets_returns)
  
  return(final_assets_returns)
}

asset_optimizer <- function(asset_prices){
  # browser()
  if(!exists("prices"))
    prep_data(stock_ls)
  
  ror <- ror_daily(asset_prices[,-c("DATE", "id")])
  
  ror[is.na(ror)] <- 0.0001
  ror[ror == Inf] <- 0.0001
  
  final_assets_returns <<- prepare_assets(cor_matrix = cor(ror))
  
  ga_res <- ga_optimize(final_assets_returns)
  
  # Store the resulting weights in a vector
  sol <- as.vector(summary(ga_res)$solution)
  
  # for checking model performance
  optimal_returns <- portfolio_returns(x = sol)
  
  return(list(final_assets_returns = final_assets_returns, sol = sol))
}
# res.pca <- prcomp(z, center = TRUE, scale = TRUE)
# summary(res.pca)
# dt <- data.table(res.pca$rotation)
# dt$name <- rownames(res.pca$rotation)
# 
# # sprawdz korelacje miedzy 10 
# pc1_10 <- dt[,.(PC1), by = name][order(PC1, decreasing = T)][1:10]$name
# a <- prices[, pc1_10]
# a <- ror_daily(a)
# a <- data.frame(a)
# b <- cor(a)
# sum(b) # sprawdenie na ile nisko skorelowane
# 
# # mean ror and viz
# a$x <- rowMeans(a, na.rm = T)
# a$y <- cumsum(a$x)
# plot(a$y, type = "l")
# 
# 
# pc1_10 <- dt[,.(PC1), by = name][order(PC1, decreasing = F)][1:10]$name
# c <- prices[,pc1_10]
# c <- ror_daily(c)
# d <- cor(c)
# # mean ror and viz
# c$x <- rowMeans(c, na.rm = T)
# c$y <- cumsum(c$x)
# plot(c$y, type = "l")
# 
# plot(c$y,type="l",col="red")
# lines(a$y,col="green")