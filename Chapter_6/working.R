install.packages("rmarkdown")
library(rmarkdown)

install.packages("quantmod")
# library(Quandl)
# mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL")

library(quantmod)
df = getSymbols("GOOG",auto.assign=FALSE)
nrow(df)
head(df, 2)
tail(df, 2)
chart_Series(df$GOOG.Close,name="Google Stock Price")
