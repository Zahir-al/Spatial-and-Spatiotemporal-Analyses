######################## Importing Packages ########################
# Clear the workspace
rm(list = ls())
# Clear the console
cat("\014")
# Clear all plots
graphics.off()

options(scipen = 999)
options(max.print = 1000)

library(rgdal)
library(spdep)
library(rgeos)
library(spatialreg)
library(readxl)

######################## Data Manipulation ########################
T1 = read.csv("counties_only.csv")
T2 = read.csv("us_county_deaths_cases.csv")
T3 = read.csv("SVI2018_US_COUNTY.csv")
T4 = read_xlsx("Diseases_LifeExpectency.xlsx","Life expectancy")
T5 = read_xlsx("Diseases_LifeExpectency.xlsx","Cardiovascular diseases")
T6 = read_xlsx("Diseases_LifeExpectency.xlsx","Hypertensive heart disease")
T7 = read_xlsx("Diseases_LifeExpectency.xlsx","Leukemia")
T8 = read_xlsx("Diseases_LifeExpectency.xlsx","Chronic respiratory diseases")
T9 = read_xlsx("Diseases_LifeExpectency.xlsx","obstructive pulmonary disease")
T10 = read_xlsx("Diseases_LifeExpectency.xlsx","Asthma")
T11 = read.csv("PopulationEstimates.csv")
T12 = read.csv("us_county_sociohealth_data.csv")

#Extracting only specific columns of T1 table
T1New = T1[, grep("Jan.Precipitation...inch|Feb.Precipitation...inch|Mar.Precipitation...inch|Apr.Precipitation...inch|May.Precipitation...inch|Jun.Precipitation...inch|Jul.Precipitation...inch|Aug.Precipitation...inch|Sep.Precipitation...inch|Oct.Precipitation...inch|Nov.Precipitation...inch|Dec.Precipitation...inch|Jan.Temp.Min...F|Feb.Temp.Min...F|Mar.Temp.Min...F|Apr.Temp.Min...F|May.Temp.Min...F|Jun.Temp.Min...F|Jul.Temp.Min...F|Aug.Temp.Min...F|Sep.Temp.Min...F|Oct.Temp.Min...F|Nov.Temp.Min...F|Dec.Temp.Min...F|Jan.Temp.Max...F|Feb.Temp.Max...F|Mar.Temp.Max...F|Apr.Temp.Max...F|May.Temp.Max...F|Jun.Temp.Max...F|Jul.Temp.Max...F|Aug.Temp.Max...F|Sep.Temp.Max...F|Oct.Temp.Max...F|Nov.Temp.Max...F|Dec.Temp.Max...F|Total_Female|Total_age65plus|Density.per.square.mile.of.land.area...Population|Unemployment_rate_2018|Less.than.a.high.school.diploma.2014.18|Median_Household_Income_2018|Bachelor's.degree.or.higher.2014.18|POVALL_2018|WA_MALE|WA_FEMALE|BA_MALE|BA_FEMALE|AA_MALE|AA_FEMALE|H_MALE|H_FEMALE|Active.Physicians.per.100000.Population.2018..AAMC.|ICU.Beds|Total.nurse.practitioners..2019.|Total.physician.assistants..2019.|Total.Hospitals..2019.|Total.Primary.Care.Physicians..2019.|R_INTERNATIONAL_MIG_2018|FIPS",colnames(T1))]

#Calculating mean annual precipitation and mean range temp
T1New$Mean_Annual_Precipitation = (T1$Jan.Precipitation...inch+T1$Feb.Precipitation...inch+T1$Mar.Precipitation...inch+T1$Apr.Precipitation...inch+T1$May.Precipitation...inch+T1$Jun.Precipitation...inch+T1$Jul.Precipitation...inch+T1$Aug.Precipitation...inch+T1$Sep.Precipitation...inch+T1$Oct.Precipitation...inch+T1$Nov.Precipitation...inch+T1$Dec.Precipitation...inch)/12
T1New$Mean_Annual_Temp_Range = ((T1$Jan.Temp.Max-T1$Jan.Temp.Min)+(T1$Feb.Temp.Max-T1$Feb.Temp.Min)+(T1$Mar.Temp.Max-T1$Mar.Temp.Min)+(T1$Apr.Temp.Max-T1$Apr.Temp.Min)+(T1$May.Temp.Max-T1$May.Temp.Min)+(T1$Jun.Temp.Max-T1$Jun.Temp.Min)+(T1$Jul.Temp.Max-T1$Jul.Temp.Min)+(T1$Aug.Temp.Max-T1$Aug.Temp.Min)+(T1$Sep.Temp.Max-T1$Sep.Temp.Min)+(T1$Oct.Temp.Max-T1$Oct.Temp.Min)+(T1$Nov.Temp.Max-T1$Nov.Temp.Min)+(T1$Dec.Temp.Max-T1$Dec.Temp.Min))/12

#Merging male and female populations
T1New$WA_MALE_WA_FEMALE = T1New$WA_MALE + T1New$WA_FEMALE
T1New$BA_MALE_BA_FEMALE = T1New$BA_MALE + T1New$BA_FEMALE
T1New$AA_MALE_AA_FEMALE = T1New$AA_MALE + T1New$AA_FEMALE
T1New$H_MALE_H_FEMALE = T1New$H_MALE + T1New$H_FEMALE

#Removing unwanted features
T1New[,8:43] = list(NULL)
T1New[,11:32] = list(NULL)

#Extracting only specific columns of T3 table
T3New = T3[, grep("FIPS|E_UNINSUR",colnames(T3))]

#Extracting only specific columns of T4 table
T4New = T4[, grep("FIPS|Life expectancy, 2014*",colnames(T4))]

#Extracting only specific columns of T5 table
T5New = T5[, grep("FIPS|Cardiovascular diseases Mortality Rate, 2014*",colnames(T5))]

#Extracting only specific columns of T6 table
T6New = T6[, grep("FIPS|Hypertensive heart disease Mortality Rate, 2014*",colnames(T6))]

#Extracting only specific columns of T7 table
T7New = T7[, grep("FIPS|Leukemia Mortality Rate, 2014*",colnames(T7))]

#Extracting only specific columns of T8 table
T8New = T8[, grep("FIPS|Chronic respiratory diseases Mortality Rate, 2014*",colnames(T8))]

#Extracting only specific columns of T9 table
T9New = T9[, grep("FIPS|Chronic obstructive pulmonary disease Mortality Rate, 2014*",colnames(T9))]

#Extracting only specific columns of T10 table
T10New = T10[, grep("FIPS|Asthma Mortality Rate, 2014*",colnames(T10))]

#Extracting only specific columns of T11 table
T11New = T11[, grep("FIPStxt|POP_ESTIMATE_2019",colnames(T11))]

#Extracting only specific columns of T12 table
T12New = T12[, grep("fips|percent_fair_or_poor_health|percent_smokers|percent_adults_with_diabetes|percent_adults_with_obesity|food_environment_index|percent_physically_inactive|percent_excessive_drinking|num_hiv_cases|percent_insufficient_sleep|num_not_proficient_in_english",colnames(T12))]

#Removing leading zeros in fips column in T12New dataframe
#install.packages("stringr")
library(stringr)
T12New$fips = str_remove(T12New$fips, "^0+")

#Merging tables
MT0 = merge(x = T2, y = T1New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT1 = merge(x = MT0, y = T3New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT2 = merge(x = MT1, y = T4New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT3 = merge(x = MT2, y = T5New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT4 = merge(x = MT3, y = T6New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT5 = merge(x = MT4, y = T7New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT6 = merge(x = MT5, y = T8New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT7 = merge(x = MT6, y = T9New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT8 = merge(x = MT7, y = T10New, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
MT9 = merge(x = MT8, y = T11New, by.x = "COUNTY", by.y = "FIPStxt", all.x = TRUE, all.y = FALSE)
MT10 = merge(x = MT9, y = T12New, by.x = "COUNTY", by.y = "fips", all.x = TRUE, all.y = FALSE)

#Removing commas to safely convert to numeric type using gsub function
MT10$POP_ESTIMATE_2019 = as.numeric(gsub(",","", MT10$POP_ESTIMATE_2019))

MT10$Mortality_Rate = (MT10$X2021.04.25/ MT10$POP_ESTIMATE_2019)*100000

#Removing unwanted columns corresponding to 2010 data
c = c(498,500,502,504,506,508,510)
MT10[,c] = list(NULL)

#Removing spaces between variable names
colnames(MT10)[498] = "Life_expectancy_2014"
colnames(MT10)[499] = "Cardiovascular_diseases_Mortality_Rate_2014"
colnames(MT10)[500] = "Hypertensive_heart_disease_Mortality_Rate_2014"
colnames(MT10)[501] = "Leukemia_Mortality_Rate_2014"
colnames(MT10)[502] = "Chronic_respiratory_diseases_Mortality_Rate_2014"
colnames(MT10)[503] = "Chronic_obstructive_pulmonary_disease_Mortality_Rate_2014"
colnames(MT10)[504] = "Asthma_Mortality_Rate_2014"

#Removing dots between variable names
colnames(MT10)[477] = "Less_than_a_high_school_diploma_2014_18"
colnames(MT10)[482] = "Density_per_square_mile_of_land_area_Population"
colnames(MT10)[485] = "Active_Physicians_per_100000_Population_2018_AAMC"
colnames(MT10)[486] = "Total_nurse_practitioners_2019"
colnames(MT10)[487] = "Total_physician_assistants_2019"
colnames(MT10)[488] = "Total_Hospitals_2019"
colnames(MT10)[489] = "Total_Primary_Care_Physicians_2019"
colnames(MT10)[490] = "ICU_Beds"

#Removing unwanted columns corresponding to us_county_deaths_cases data set
c = c(3,5:15)
MT10[,c] = list(NULL)

#Correcting Mean_Annual_Temp_Range column
MT10[480] = MT10[480]*(-1)

#Number of missing values of each feature
as.matrix(sapply(MT10[,464:503], function(x) sum(is.na(x))))
MT10$num_hiv_cases = NULL
MT10$food_environment_index = NULL
#MT10$POP_ESTIMATE_2019 = NULL
MT10$PCTPOVALL_2018 = NULL

#Moving population column
MT10 = cbind(MT10[,1:2], MT10[,492], MT10[,3:491], MT10[,493:501])
colnames(MT10)[3] = "POP_ESTIMATE_2019"

#Writing the .csv file
#write.csv(MT10, 'MT10.csv', row.names = FALSE)

#We implemented a lasso regression

#Reading shape file
County.Map = readOGR(dsn = ".", layer = "CO_CARTO")

#Identifying missing values in MT10
missing.rows.columns = data.frame(which(is.na(MT10), arr.ind = TRUE))
#Rows with missing values in MT10
na.index = c(unique(missing.rows.columns[,1]))
#Omitting missing values in shape file and MT10
County.Map@data = County.Map@data[-c(na.index),]
County.Map@polygons = County.Map@polygons[-c(na.index)]
County.Map@plotOrder = County.Map@plotOrder[-c(na.index)]

#Reading Lasso file
MT10_scaled_lasso = read.csv("MT10_scaled_lasso_New.csv")[,-c(1:466)]

#Creating weights
Queen.nb = poly2nb(County.Map)
#Rook.nb = poly2nb(County.Map, queen = FALSE)
Queen.listw = nb2listw(Queen.nb, zero.policy = TRUE)
#Rook.listw = nb2listw(Rook.nb, zero.policy = TRUE)
listw1 = Queen.listw
#listw2 = Rook.listw

#Introducing regression formula
vars = names(MT10_scaled_lasso)[1:17]
formula = as.formula(paste("Mortality_Rate ~ ", paste(vars, collapse = "+")))

#Clearing unwanted objects
remove(MT0, MT1, MT2, MT3, MT4, MT5, MT6, MT7, MT8, MT9, T1, T2, T3, T4, T5, T6, T7,
       T8, T9, T10, T11, T12, T1New, T3New, T4New, T5New, T6New, T7New, T8New, T9New,
       T10New, T11New, c, vars, T12New, missing.rows.columns)

######################## Non-spatial Regression ########################

######################## OLS Model ########################
start.time.lm <- Sys.time()
reg.lm = lm(formula, MT10_scaled_lasso)
end.time.lm <- Sys.time()
time.taken.lm <- end.time.lm - start.time.lm
time.taken.lm

summary(reg.lm)
AIC(reg.lm)

#Calculating Mean squared error
OLS.MSE = mean(reg.lm$residuals^2)
OLS.MSE

library(regclass)  #To run VIF(Variance inflation factor)
VIFs = as.matrix(VIF(reg.lm))  #To see multicolinearity between explanatory variables
VIFs

#Spatial correlation for the residuals (Moran's I test),
#The null hypothesis states that there is no spatial correlation in the residuals
lm.morantest(reg.lm, listw1, zero.policy = TRUE)

#Lagrange multiplier tests state that how much the model will improve in fitting
#if we ran a specific spatial model that is included in the LM test name.
#In robust versions the interpretation is similar except that we filter out the
#false positives for that test model, cause and a lag model can be a false positive for
#an error model and vice versa, because the data generating procedure is similar
#for those type of models.
#Note that R is not capable of running the SARMA model.
lm.LMtests(reg.lm, listw1, test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA")
           ,zero.policy = TRUE)

######################## Spatial Regression ########################

######################## Manski Model ########################
#Manski All-inclusive Model: y=pWy+Xß+WXT+u,   u=LWu+e (not recommended)
start.time.Manski <- Sys.time()
reg.Manski = sacsarlm(formula, MT10_scaled_lasso, listw1, type = "sacmixed", zero.policy = TRUE)
end.time.Manski <- Sys.time()
time.taken.Manski <- end.time.Manski - start.time.Manski
time.taken.Manski

summary(reg.Manski)
AIC(reg.Manski)

#Calculating R^2
Manski.R_squared = 1-(sum(reg.Manski$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
Manski.adj.r.squared = 1 - (1 - Manski.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))

#Calculating Mean squared error
Manski.MSE = mean(reg.Manski$residuals^2)

#Explanation of this is in following sections!
summary(impacts(reg.Manski, listw = listw1, R = 500), zstats = TRUE)

Manski.R_squared
Manski.adj.r.squared
Manski.MSE

#Moran's I test for residuals
moran.mc(reg.Manski$residuals, listw1, 500, zero.policy = TRUE)

######################## SLX Model ########################
#p=rho, T=theta, and L=lambda
#SLX Spatially Lagged X y=Xß+WXT+e
start.time.SLX <- Sys.time()
reg.SLX = lmSLX(formula, MT10_scaled_lasso, listw1, zero.policy = TRUE)
end.time.SLX <- Sys.time()
time.taken.SLX <- end.time.SLX - start.time.SLX
time.taken.SLX

summary(reg.SLX)
AIC(reg.SLX)

#Calculating R^2
SLX.R_squared = 1-(sum(reg.SLX$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
SLX.adj.r.squared = 1 - (1 - SLX.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))


#Calculating Mean Squared error
SLX.MSE = mean(reg.SLX$residuals^2)

SLX.R_squared
SLX.adj.r.squared
SLX.MSE

#Anything with SLX in it: Also need to think about total impact (Direct+Indirect)
#Lags are indirect effects
#You must use the "impacts" Command
#To see if there total effects are statistically significant me must get a summary
#out of the impact function above.
#Add zstats,pvals; R=50 not needed for SLX but needed for a lagged y.
#R value option is for simulation repetitions
summary(impacts(reg.SLX, listw = listw1, R = 500), zstats = TRUE)

#Moran's I test for residuals
moran.mc(reg.SLX$residuals, listw1, 500, zero.policy = TRUE)

######################## SAR Model ########################
#SAR Spatial (Simultaneous) Lag (Autoregressive) Model y=pWy+Xß+e
start.time.SAR <- Sys.time()
reg.SAR = lagsarlm(formula, MT10_scaled_lasso, listw1, zero.policy = TRUE)
end.time.SAR <- Sys.time()
time.taken.SAR <- end.time.SAR - start.time.SAR
time.taken.SAR

summary(reg.SAR)
AIC(reg.SAR)

#Calculating R^2
SAR.R_squared = 1-(sum(reg.SAR$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
SAR.adj.r.squared = 1 - (1 - SAR.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))

#Calculating Mean squared error
SAR.MSE = mean(reg.SAR$residuals^2)

SAR.R_squared
SAR.adj.r.squared
SAR.MSE

#Anything with a Lag Y in it: You Cannot interpret Betas as marginal effects,
#that is because of global feedback effect. In another words, when one of our 
#X's change, this will effect our Y's and this will effect Y's of our neighboring
#regions and finally effects our Y's. And this infinite feedback will continue.
#You must use the "impacts" Command:
summary(impacts(reg.SAR,listw = listw1,R = 500), zstats = TRUE) #Add zstats for pvals
#Caution: These pvalues are simulated, and seem to vary a bit from run to run.
#So also consider running it multiple times.

#Moran's I test for residuals
moran.mc(reg.SAR$residuals, listw1, 500, zero.policy = TRUE)

######################## SEM Model ########################
#SEM Spatial Error Model  y=Xß+u, u=LWu+e
start.time.SEM <- Sys.time()
reg.SEM = errorsarlm(formula, MT10_scaled_lasso, listw1, zero.policy = TRUE)
end.time.SEM <- Sys.time()
time.taken.SEM <- end.time.SEM - start.time.SEM
time.taken.SEM

summary(reg.SEM)
AIC(reg.SEM)

#Calculating R^2
SEM.R_squared = 1-(sum(reg.SEM$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
SEM.adj.r.squared = 1 - (1 - SEM.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))

#Calculating Mean squared error
SEM.MSE = mean(reg.SEM$residuals^2)

SEM.R_squared
SEM.adj.r.squared
SEM.MSE

#Moran's I test for residuals
moran.mc(reg.SEM$residuals, listw1, 500, zero.policy = TRUE)

######################## Spatial Hausman Test ########################
#The spatial Hausman test alternative hypothesis states that neither the 
#underlying model nor OLS are good choice of models. In spatial context this
#indicates that there is some spatial correlation, but the underlying model
#is not capable of describing it and maybe other spatial models are good choices.
#The null hypothesis states the underlying model is a good choice of model.
#In fact the spatial Hausman test shows the divergence between estimates from
#OLS and the underlying model. Here reg.SEM is meant to be the underlying model.
#The main reason for using SEm is that it has a good structure of spatial noise.
Hausman.test(reg.SEM)

######################## SDEM Model ########################
#LeSage and Pace approach is to select either a spatial Durbin model or a spatial
#Durbin error model as a starting point as we are considering some model coefficients
#to be zero in our analysis to get simpler models.
#SDEM Spatial Durbin Error Model (add lag X to SEM)   y=Xß+WxT+u,   u=LWu+e
start.time.SDEM <- Sys.time()
reg.SDEM = errorsarlm(formula, MT10_scaled_lasso, listw1, etype = "emixed",
                      zero.policy = TRUE)
end.time.SDEM <- Sys.time()
time.taken.SDEM <- end.time.SDEM - start.time.SDEM
time.taken.SDEM

summary(reg.SDEM)
AIC(reg.SDEM)

#Calculating R^2
SDEM.R_squared = 1-(sum(reg.SDEM$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
SDEM.adj.r.squared = 1 - (1 - SDEM.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))

#Calculating Mean squared error
SDEM.MSE = mean(reg.SDEM$residuals^2)

SDEM.R_squared
SDEM.adj.r.squared
SDEM.MSE

summary(impacts(reg.SDEM,listw = listw1,R = 500), zstats = TRUE) #Add zstats for pvals

#Moran's I test for residuals
moran.mc(reg.SDEM$residuals, listw1, 500, zero.policy = TRUE)

######################## SDM Model ########################
#SDM Spatial Durbin Model (add lag X to SAR) y=pWy+Xß+WXT+e 
start.time.SDM <- Sys.time()
reg.SDM = lagsarlm(formula, MT10_scaled_lasso, listw1, type = "mixed",
                   zero.policy = TRUE)
end.time.SDM <- Sys.time()
time.taken.SDM <- end.time.SDM - start.time.SDM
time.taken.SDM

summary(reg.SDM)
AIC(reg.SDM)

#Calculating R^2
SDM.R_squared = 1-(sum(reg.SDM$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
SDM.adj.r.squared = 1 - (1 - SDM.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))


#Calculating Mean squared error
SDM.MSE = mean(reg.SDM$residuals^2)

SDM.R_squared
SDM.adj.r.squared
SDM.MSE

summary(impacts(reg.SDM,listw = listw1,R = 500), zstats = TRUE) #Add zstats for pvals

#Moran's I test for residuals
moran.mc(reg.SDM$residuals, listw1, 500, zero.policy = TRUE)

######################## SARAR Model ########################
#SARAR a.k.a. Kelejian-Prucha, Cliff-Ord, or SAC If all T=0,y=pWy+Xß+u, u=LWu+e
start.time.SARAR <- Sys.time()
reg.SARAR = sacsarlm(formula, MT10_scaled_lasso, listw1, type = "sac", zero.policy = TRUE)
end.time.SARAR <- Sys.time()
time.taken.SARAR <- end.time.SARAR - start.time.SARAR
time.taken.SARAR

summary(reg.SARAR)
AIC(reg.SARAR)

#Calculating R^2
SARAR.R_squared = 1-(sum(reg.SARAR$residuals^2)/sum((MT10_scaled_lasso$Mortality_Rate-mean(MT10_scaled_lasso$Mortality_Rate))^2))
SARAR.adj.r.squared = 1 - (1 - SARAR.R_squared) * ((nrow(MT10_scaled_lasso) - 1)/(nrow(MT10_scaled_lasso)-(ncol(MT10_scaled_lasso)-1)-1))

#Calculating Mean squared error
SARAR.MSE = mean(reg.SARAR$residuals^2)

SARAR.R_squared
SARAR.adj.r.squared
SARAR.MSE

summary(impacts(reg.SARAR,listw = listw1,R = 500), zstats = TRUE) #Add zstats for pvals

#Moran's I test for residuals
moran.mc(reg.SARAR$residuals, listw1, 500, zero.policy = TRUE)

######################## Model Restriction ########################
#To see which kind of models are suitable for our analysis first think of the
#set of models to be local or global.
#In case of global if the behavior of one county affects the entire state
#or, if local, behavior in one county only affects neighboring counties.
#In our case a global is more sensible.

#LR Tests: Test Model Restrictions: 
#The null hypothesis is that restricted coefficients = 0 and restricting the 
#model is OK!.
#The df(degrees of freedom) is the # of those variables we want to restrict and
#in this case is the lagged variables.
LR.Sarlm(reg.SEM, reg.SDEM) #likelihood ratio test to see if SDEM should be restricted to the SEM. 
#You can test any model to see if it should be restricted to a simpler, NESTED model. 
#SDM and SDEM are not nested, as one cannot be simplified into the other.
LR.Sarlm(reg.SDEM, reg.SEM) #order you put the models in doesn't matter

#The df here is 1 that is for lambda to be equal to zero
LR.Sarlm(reg.SDM, reg.SLX) #likelihood ratio test to see if SDM should be restricted to the SLX. 
#The df here is 5 that is the # of spatially lagged X's and 1 error multiplier.
LR.Sarlm(reg.SDM, reg.SAR) #likelihood ratio test to see if SDM should be restricted to SAR. 
#The same process can be done with the set of spatial global models.
LR.Sarlm(reg.SDM, reg.lm) #likelihood ratio test to see if SDM should be restricted to lm. 

######################## Heteroskedasticity Testing ########################
#Do a spatial Breusch-Pagan Test for Heteroskedasticity
#It is assumed that the error (a.k.a residual) of a regression model is
#homoscedastic across all values of the predicted value of the DV.
#Heteroskedasticity probably effects our error terms and p-values so we must be
#cautious if the alpha value and p-values are very close.
bptest.Sarlm(reg.SDEM, studentize = TRUE)
plot(reg.SDEM$fitted.values, reg.SDEM$residuals)

library(lmtest)
bptest(reg.SLX) # For not-sarlm objects
plot(reg.SLX$fitted.values, reg.SLX$residuals)

#Not correcting for heteroskedasticity may not be a grave sin. If the sample size
#is sufficiently large, the variance of your estimators may be small enough to
#still produce precise estimates. Heteroskedasticity is a common issue in
#cross-sectional data sets.

######################## Regression With Robust Standard Errors ########################
#If we're willing to accept the fact that ordinary least squares no longer
#produces the best linear unbiased estimators (BLUE), we can still perform our
#regression analysis to correct the issue of incorrect standard errors so that
#our interval estimates and hypothesis tests are valid. We do this by using
#heteroskedasticity-consistent standard errors or simply robust standard errors.
library(sandwich)
coeftest(reg.lm, vcov = vcovHC(reg.lm, "HC1"))  # HC1 gives us the White standard errors.
