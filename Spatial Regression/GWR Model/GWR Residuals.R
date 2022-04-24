data = read.csv('GWR_session_results.csv')
library(spdep)
library(Matrix)
a=Diagonal(3133,data$sumW)
W = mat2listw(a)
moran.mc(data$gwr_residual,W,500)

GWR.MSE = mean((data$y-data$gwr_yhat)^2)
GWR.MSE