library(data.table)
library(readxl)
library(quantmod)
library(purrr)
library(Quandl)
# library(myc)
options(scipen = 999)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

############
# TODO: wybor akcji do optymalizacji portfeka
api_key <- "uANbGacS6uLeVnceyxbP"
Quandl.api_key(api_key)
# https://data.nasdaq.com/databases/SFA/usage/quickstart/r
Quandl.datatable('NDAQ/RTAT10', date='2023-12-12', ticker='ADB,TQQQ,SQQQ,AAPL,NVDIA,META,ADB,CDP')
############

# date of analysis
date_analysis <- Sys.Date()-1 #TODO: w przypadku soboty/niedzieli niech wezmie piatek

# read file with 
dt <- read_xlsx(path = "./data/nasdaq_nyse_quali100222.xlsx", sheet = "ranking") #TODO: automat do tworzenia aktualnego rankingu, zmiana algo, bo w tej chwili faworyzuje najwieksze spolki, moze wziac pod uwage wyniki finansowe z ostatnich 3 kwartalow i przyrost / spadek procentowy
dt <- data.table(dt)
stocks_vec <- dt[1:12,]$Symbol
stocks_vec[which(stocks_vec == "FB")] <- "META"

stock_ls <- getSymbols(Symbols = stocks_vec,
                       from = date_analysis - 365*3,
                       to = date_analysis, 
                       warnings = TRUE)
# stocks_fin <- getFinancials(Symbol = "TSLA")

prep_data <- function(stock_ls){ 
  prices <- map(stock_ls, function(x) Ad(get(x)))
  prices <- reduce(prices,merge)
  colnames(prices) <- stock_ls
  
  # rm(list = setdiff(ls(), c("prices", "stock_ls")))
  
  # for simulation
  prices <- tibble::rownames_to_column(data.frame(prices), "DATE")
  prices <- data.table(prices)
  
  # jezeli 1/3 pustych to elo
  to_filter <- prices[, lapply(.SD, function(x) sum(is.na(x))/nrow(prices))] > 1/3
  to_filter <- which(to_filter == TRUE)
  prices <- prices[,-to_filter, with = FALSE]
  prices[is.na(prices)] <- 0
  
  prices_last_year <- prices[DATE > Sys.Date() - 365]
  # create table from prices ly with period id
  n_for_id = nrow(prices_last_year)/12
  prices_last_year$id <- rep(1:ceiling(nrow(prices_last_year)/12), each = n_for_id, length.out = nrow(prices_last_year))
  prices_last_year <<- prices_last_year[id <= 12]
  
  prices <- prices[DATE <= Sys.Date() - 365]
  
  prices_to_sim <- prices
  prices_to_sim$id <- 1
  prices_to_sim <<- prices_to_sim

  prices <- prices[,-c("DATE")]
  prices <<- data.frame(prices)
  
}
