options(max.print = 3000)
data = read.csv("MT10_scaled_lasso.csv")
nnz = as.matrix(which(data[,7:466]!=0, arr.ind = T))
colnames(data)[7-1+nnz[1,2]]
sum(data$X2020.02.06)
sum(data$X2020.02.05)  #So counts start in 2020.02.06

#2020.05.06
data$Three_M_Mortality_Rate = (data$X2020.05.06/ data$POP_ESTIMATE_2019)*100000

#2020.08.06
data$Six_M_Mortality_Rate = (data$X2020.08.06/ data$POP_ESTIMATE_2019)*100000

#2020.11.06
data$Nine_M_Mortality_Rate = (data$X2020.11.06/ data$POP_ESTIMATE_2019)*100000

#2021.02.06
data$Twelve_M_Mortality_Rate = (data$X2021.02.06/ data$POP_ESTIMATE_2019)*100000

write.csv(data, 'MT10_scaled_lasso_New.csv', row.names = FALSE)
