#Once only — account setup (run in R console, never save in a file):

#install.packages(c("rsconnect", "bslib"))

#rsconnect::setAccountInfo(
#  name   = "taj771",
#  token  = "your_new_token",    # use new token after revoking
#  secret = "your_new_secret"
#)



# Every time want to deploy — just this:


library(rsconnect)

rsconnect::deployApp(
  appDir      = ".",
  appFiles    = c("app.R", "Data/spi_results.rds"),
  appName     = "argentina-spi-monitor",
  forceUpdate = TRUE
)





