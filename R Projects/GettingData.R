library(tidyquant)
help(tidyquant)

getSymbols("AAPL", from = '2017-01-01',
           to = "2022-06-01",warnings = FALSE,
           auto.assign = TRUE)
head(AAPL)
class(AAPL)
chart_Series(AAPL)
chart_Series(AAPL['2017-12/2018-03'])

tq_get_options()

getSymbols("^IXIC", from = '2017-01-01',
           to = "2022-06-01",warnings = FALSE,
           auto.assign = TRUE)


aapl_prices  <- tq_get("AAPL", get = "stock.prices", from = " 2015-01-01", to = " 2022-01-01")

Amazon <- tq_get("AMZN", get = "stock.prices", from = " 2015-01-01")

