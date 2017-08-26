#setworking directory
setwd("~/r-code/lovelived")

#if you don't like scientific notation
options(scipen=100)

#install and load packages you'll need
if (!require("RSQLite")) {
  install.packages("RSQLite")
library(RSQLite)
}

if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

if (!require("sqldf")) {
  install.packages("sqldf")
  library(sqldf)
}

if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}


#unzip the database
unzip(zipfile = "nqdb3.zip")

# load SQLite driver
sqlite <- dbDriver("SQLite")

# open database
nqdb <- dbConnect(sqlite, "nqdb3.db")

# list tables
dbListTables(nqdb)

# list fields
dbListFields(nqdb, "trds")
dbListFields(nqdb, "qts")

# query the tables
qts <- dbGetQuery(nqdb, statement = "select * from qts")

trds <- dbGetQuery(nqdb, statement = "select * from trds")

# quick look at the first and last 6 rows
head(trds)
tail(trds)

#================================================= Part 1 Start ================================================#
# sql to get data for symbol DAVE
naq_dave <- sqldf("
                  SELECT p.RefDate AS RefDate, p.Symbol AS Symbol, p1.Price AS OpeningPrice, p2.Price AS ClosingPrice,
                  min(p.Price) AS MinPrice, max(p.Price) AS MaxPrice, avg(p.Price) AS AvgPrice, 
                  ((p.Quantity * p.Price)/p.Quantity) as VWAP, count(*) as Trades, sum(p.Quantity) as Shares,
                  ((max(p.Price) - min(p.Price))/p2.Price) as PctRange
                  FROM trds p join
                  (select * from trds where Symbol = 'DAVE' and Pid = 'Q' and SaleCondition LIKE'@O%'
                  ) p1,
	                (select * from trds where Symbol = 'DAVE' and Pid = 'Q' and SaleCondition LIKE'@6%'
                  ) p2
                  WHERE p.Symbol = p1.Symbol")

# sql to get data for symbol AAPL
naq_aapl <- sqldf("
                  SELECT p.RefDate AS RefDate, p.Symbol AS Symbol, p1.Price AS OpeningPrice, p2.Price AS ClosingPrice,
                  min(p.Price) AS MinPrice, max(p.Price) AS MaxPrice, avg(p.Price) AS AvgPrice, 
                  ((p.Quantity * p.Price)/p.Quantity) as VWAP, count(*) as Trades, sum(p.Quantity) as Shares, 
                  ((max(p.Price) - min(p.Price))/p2.Price) as PctRange
                  FROM trds p join
                  (select * from trds where Symbol = 'AAPL' and Pid = 'Q' and SaleCondition LIKE'@O%'
                  ) p1,
	                (select * from trds where Symbol = 'AAPL' and Pid = 'Q' and SaleCondition LIKE'@6%'
                  ) p2
                  WHERE p.Symbol = p1.Symbol")

# sql to get data for symbol AMZN
naq_amzn <- sqldf("
                  SELECT p.RefDate AS RefDate, p.Symbol AS Symbol, p1.Price AS OpeningPrice, p2.Price AS ClosingPrice,
                  min(p.Price) AS MinPrice, max(p.Price) AS MaxPrice, avg(p.Price) AS AvgPrice, 
                  ((p.Quantity * p.Price)/p.Quantity) as VWAP, count(*) as Trades, sum(p.Quantity) as Shares, 
                  ((max(p.Price) - min(p.Price))/p2.Price) as PctRange
                  FROM trds p join
                  (select * from trds where Symbol = 'AMZN' and Pid = 'Q' and SaleCondition LIKE'@O%'
                  ) p1,
                  (select * from trds where Symbol = 'AMZN' and Pid = 'Q' and SaleCondition LIKE'@6%'
                  ) p2
                  WHERE p.Symbol = p1.Symbol")

# sql to get data for symbol MSFT
naq_msft <- sqldf("
                  SELECT p.RefDate AS RefDate, p.Symbol AS Symbol, p1.Price AS OpeningPrice, p2.Price AS ClosingPrice,
                   min(p.Price) AS MinPrice, max(p.Price) AS MaxPrice, avg(p.Price) AS AvgPrice, 
                  ((p.Quantity * p.Price)/p.Quantity) as VWAP, count(*) as Trades, sum(p.Quantity) as Shares,
                  ((max(p.Price) - min(p.Price))/p2.Price) as PctRange
                  FROM trds p join
                  (select * from trds where Symbol = 'MSFT' and Pid = 'Q' and SaleCondition LIKE'@O%'
                  ) p1,
                  (select * from trds where Symbol = 'MSFT' and Pid = 'Q' and SaleCondition LIKE'@6%'
                  ) p2
                  WHERE p.Symbol = p1.Symbol")

# create a empty data.frame to insert the data from the sql statements
df <- data.frame(RefDate=as.Date(character()),
                 Symbol=character(), 
                 OpeningPrice=as.numeric(), 
                 ClosingPrice=as.numeric(),
                 MinPrice=as.numeric(),
                 MaxPrice=as.numeric(),
                 AvgPrice=as.numeric(),
                 VWAP=as.numeric(),
                 Trades=as.numeric(),
                 Shares=as.numeric(),
                 PctRange=as.numeric(),
                 stringsAsFactors=FALSE) 

# insert the data into the data.frame
df <- rbind(naq_dave,naq_aapl,naq_amzn,naq_msft)

# write the file day summary to disk
write.csv(df, file = "~/r-code/lovelived/DaySummary_v02.csv")
#================================================= Part 1 End ================================================#

#================================================= Part 2 Start ================================================#

fivemin <- sqldf("select RefDate, symbol, cast(Timestamp/300e3 as int) as timeBucket, timestamp as startTime, max(price) AS MaxPrice, min(Price) AS MinPrice, avg(Price) AS AvgPrice, price AS LastPrice
FROM trds
where timestamp >= 34200000 AND timestamp <= 57600000 and Pid = 'Q' 
group by cast(Timestamp/300e3 as int), symbol
order by symbol")

# empty data.frame to hold the 5min Summary data
df1 <- data.frame(RefDate=as.Date(character()),
                  Symbol=character(), 
                  timeBucket=as.numeric(), 
                  startTime=as.numeric(),
                  MinPrice=as.numeric(),
                  MaxPrice=as.numeric(),
                  AvgPrice=as.numeric(),
                  LastPrice=as.numeric(),
                  stringsAsFactors = FALSE)

# insert data into data.frame
df1 <- rbind(fivemin)

# convert timestamp ms to hms
df1$startTime <- format( as.POSIXct(df1$startTime/1000 , origin=Sys.Date() ),
        format="%H:%M:%S")

# write file to disk
write.csv(df, file = "~/r-code/lovelived/DaySummary_v02.csv")
#================================================= Part 2 End ================================================#


#================================================= Part 3 Start ==============================================#

tradeandquotes <- sqldf("select t.RefDate, t.Symbol, t.timestamp, q.Bidprice, q.Askprice, t.price, t.quantity, t.pid
from trds t
                        join qts q on t.sequence = q.sequence
                        where t.symbol = 'MSFT' and max(t.timestamp, q.timestamp) >= 34200000 AND max(t.timestamp, q.timestamp) <= 57600000 limit '100'")

df3 <- data.frame(RefDate=as.Date(character()),
                  Symbol=character(), 
                  timestamp=as.numeric(), 
                  BidPrice=as.numeric(),
                  AskPrice=as.numeric(),
                  Price=as.numeric(),
                  Quantity=as.numeric(),
                  Pid=as.numeric(),
                  stringsAsFactors = FALSE)

# insert data into data.frame
df3 <- rbind(tradeandquotes)

# write file to disk
write.csv(df, file = "~/r-code/lovelived/TradesANDQuotes_v02.csv")

a <- mean(df3$BidPrice, 1) 
b <- mean(df3$AskPrice, 1)
BAM <- mean(a,b)
effectivespread <- abs(df3$Price-BAM)/BAM
print(effectivespread)

vol <- (df3$Price * df3$Quantity)
volspread <- abs(vol/BAM)/100
print(volspread)
#================================================= Part 3 End ================================================#
