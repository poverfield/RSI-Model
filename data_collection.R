
# set up directory on rasbperry
#dir = '/home/pi/Desktop/files'
#setwd(dir)

name = c("ETH","XBT","XMR","XRP","ETC","LTC")
# determine if data table already exists. if it does not write out a new table. if it does, do nothing. 
initiate = function(name){
  file_name = paste(name,"_data.csv", sep = "")
  if(!file.exists(file_name)){ # if data table does not exist
    df = pull_data(name) # initiate data table
    write.csv(df, file = file_name) # write data table
  } else {
    print("data table already exists")
  }
}

# pull data from kraken.  used in initiate function
pull_data = function(name){
  # create dummy ohlc.out to compare lengths
  ohlc.out = 1
  while(length(ohlc.out) == 1){ # run until the data downloads
    Sys.sleep(1) # wait 1 second before re try download
    pair = paste("X",name,"ZUSD", sep="")
    interval = '30'
    base.url = "https://api.kraken.com/0/public/OHLC"
    url <- paste0(base.url, "?", "pair=", pair, "&interval=", interval)
    
    
    # interval = minute
    ohlc.out <- jsonlite::fromJSON(url) 
  }
  
  # create data table
  df_new = ohlc.out[[2]][[1]]
  df_new = as.numeric(df_new) # change to numeric
  df_new = matrix(data = df_new, ncol = 8, byrow = FALSE) # recreate data matrix
  colnames(df_new) = c("time","open","high","low","close","vwap","colume","count")
  df_new = df_new[,1:5]
  return(df_new)
}

# aggregates data
data = function(name){ # name = "ETH"
  # create dummy ohlc.out to compare lengths
  ohlc.out = 1
  while(length(ohlc.out) == 1){ # run until the data downloads
    Sys.sleep(1) # wait 1 second before re try download
    pair = paste("X",name,"ZUSD", sep="")
    pair = "XETHZUSD" 
    interval = '30'
    base.url = "https://api.kraken.com/0/public/OHLC"
    url <- paste0(base.url, "?", "pair=", pair, "&interval=", interval)
    
    
    # interval = minute
    ohlc.out <- jsonlite::fromJSON(url) 
  }
  
  # create data table
  df_new = ohlc.out[[2]][[1]]
  df_new = as.numeric(df_new) # change to numeric
  df_new = matrix(data = df_new, ncol = 8, byrow = FALSE) # recreate data matrix
  colnames(df_new) = c("time","open","high","low","close","vwap","colume","count")
  df_new = df_new[,1:5]
  
  
  # read in data
  file_name = paste(name,"_data.csv", sep = "")
  df = read.csv(file_name)[2:6]
  date = df[nrow(df),1] # last date in old data frame
  
  # add to df
  if(which(df_new[,1] == date) != 720){ # if there is a new row of data
    id = which(df_new[,1] == date) # get new date row
    df = rbind(df, df_new[(id+1):nrow(df_new),])
    write.csv(df, file = file_name)
    return(df)
  } else {
    result = "no new data"
    return(result)
  }
}

# Execute Script
for(i in name){
  print(i)
  
  initiate(i)
  
  df = data(i)
  print(dim(df)) # if NULL then there is no data table and no data was added
}
