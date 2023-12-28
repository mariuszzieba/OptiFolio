prep_data(stock_ls)

dt_optimal_returns <- data.table(month = 0, optimal_returns_i = 0)
asset_weights_all <- data.table()
prices_to_simulate <- prices_to_sim
for(i in 1:12){
  message(paste0("Simulation for month: ", i))
  opt <- asset_optimizer(asset_prices = prices_to_simulate)
  
  # table with asset name and % to invest
  asset_weights <- data.table(asset = names(opt$final_assets_returns), 
                              to_invest = opt$sol)
  asset_weights_all <- rbind(asset_weights_all, asset_weights)
  
  asset_names <- colnames(opt$final_assets_returns)
  prices_month_i <- prices_last_year[id == i, ..asset_names]
  returns_month_i <- ror_daily(prices_month_i)
  optimal_returns_i <- portfolio_returns_i(data = data.frame(returns_month_i), 
                                           x = asset_weights$to_invest)
  ##
  last_val <- dt_optimal_returns[nrow(dt_optimal_returns)]$optimal_returns_i
  optimal_returns_i <- cumsum(c(last_val, optimal_returns_i))[2:length(optimal_returns_i)]
  ## 
  dt_optimal_returns <- rbind(dt_optimal_returns, 
                              data.table(month = i, optimal_returns_i))
  
  plot(dt_optimal_returns$optimal_returns_i,type="l",lwd=5)
  
  # nastepny prices month i tylko tak zeby nie bral pierwszych tylu dni ile mamy w prices_month_i
  prices_to_simulate <- rbind(prices_to_sim[nrow(prices_last_year[id %in% c(1:i),]):nrow(prices_to_sim)],
                              prices_last_year[id %in% c(1:i),])
  
}

plot(dt_optimal_returns$optimal_returns_i,type="l",lwd=5)
# porownac mozna teraz z indeksem gieldowym lub wybranymi akcjami