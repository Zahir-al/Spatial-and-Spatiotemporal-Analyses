######################## Loading Packages ########################
options(max.print = 200000)
options(scipen = 999)  # Suppressing scientific notation

library(sp)   # Spatial points addition to regular data frames
library(gstat)  # Geostatistical methods by Edzer Pebesma
library(rgdal)  # Shape file manipulation
library(raster)  # For cropping maps
library(viridis)  # For creating color maps
library(RColorBrewer)  # For creating color maps

######################## Data Manipulation ########################
# Read the data table from a comma delimited file & a shape file
mydata1 = read.csv("MT10.csv")        # read in comma delimited data file
mydata2 = readOGR(dsn = ".", layer = "CO_CARTO")  # "." is for current folder
mydata1[,c(1:464)] = list(NULL)
mydata3 = cbind(coordinates(mydata2), mydata1)

colnames(mydata3)[1] = "x"
colnames(mydata3)[2] = "y"

# Identifying missing values in mydata3 and unwanted rows in shape file
missing.rows.columns = data.frame(which(is.na(mydata3), arr.ind = TRUE))
# Rows with missing values in mydata3
na.index = c(unique(missing.rows.columns[,1]))
na.index
# Removing them
mydata2@data = mydata2@data[-c(68:96,547:551, c(na.index)),]
mydata2@polygons = mydata2@polygons[-c(68:96,547:551, c(na.index))]
mydata2@plotOrder = mydata2@plotOrder[-c(68:96,547:551, c(na.index))]
mydata3 = mydata3[-c(68:96,547:551, c(na.index)),]

# Convert the dataframe to a spatial points dataframe
class(mydata3)                # confirms that it is a dataframe
coordinates(mydata3) = ~x+y   # indicate the X, Y spatial coordinates
summary(mydata3)              # confirms that it is now a spatial points dataframe
head(coordinates(mydata3))    # check the first several coordinates

# We can plot the Mortality_Rate data location map
spplot(mydata3, "Mortality_Rate", do.log = FALSE,
       key.space=list(x=0.85,y=0.95,corner=c(0,1)), scales=list(draw=T),
       xlab = "X (m)", ylab = "Y (m)",main ="Mortality Rate (Per 100,000)")

######################## Variogram Fitting ########################
# With nugget effect
# Compute directed (anistropic) empirical/ Sample/ Experimental semivariogram
TheVariogram1 = variogram(Mortality_Rate~1, data=mydata3, alpha=c(0,45,90,135))

plot(TheVariogram1)  #Experimental variogram

TheVariogramModel1 = vgm(psill=var(mydata3$Mortality_Rate)-8000, model="Sph",
                         nugget=8000, range = 12, anis = c(090, 0.2),
                         tol.hor = 22.5)  #Theoretical variogram

plot(TheVariogram1, model=TheVariogramModel1)

L1 = as.logical(c(TRUE, FALSE))
FittedModel1 = fit.variogram(TheVariogram1, model=TheVariogramModel1,
                             fit.sills = L1, fit.ranges= L1, fit.method = 1)

plot(TheVariogram1, model=FittedModel1, main='Fitted Variogram Models with
     Nugget Effect')

# Fitting separately
vn0 = TheVariogram1[1:15,1:6]
vn45 = TheVariogram1[16:30,1:6]
vn90 = TheVariogram1[31:45,1:6]
vn135 = TheVariogram1[46:60,1:6]

fn0 = fit.variogram(vn0, model=TheVariogramModel1, fit.sills = L1,
                    fit.ranges= L1, fit.method = 1)

fn45 = fit.variogram(vn45, model=TheVariogramModel1, fit.sills = L1,
                     fit.ranges= L1, fit.method = 1)

fn90 = fit.variogram(vn90, model=TheVariogramModel1, fit.sills = L1,
                     fit.ranges= L1, fit.method = 1)

fn135 = fit.variogram(vn135, model=TheVariogramModel1, fit.sills = L1,
                      fit.ranges= L1, fit.method = 1)

# Evaluating the quality of fitting using SSErr
sum((vn0$gamma-variogramLine(fn0, dist_vector =vn0$dist)$gamma)^2)
sum((vn45$gamma-variogramLine(fn45, dist_vector =vn45$dist)$gamma)^2)
sum((vn90$gamma-variogramLine(fn90, dist_vector =vn90$dist)$gamma)^2)  # The minimum
sum((vn135$gamma-variogramLine(fn135, dist_vector =vn135$dist)$gamma)^2)

# Without nugget effect
# Compute directed (anistropic) empirical/ Sample/ Experimental semivariogram
TheVariogram2 = variogram(Mortality_Rate~1, data=mydata3, alpha=c(0,45,90,135))

plot(TheVariogram2)  #Experimental variogram

TheVariogramModel2 = vgm(psill=var(mydata3$Mortality_Rate), model="Sph",
                         nugget = 0, range = 12 ,anis = c(090, 0.2),
                         tol.hor = 22.5)  #Theoretical variogram

plot(TheVariogram2, model=TheVariogramModel2)

L2 = as.logical(c(FALSE, TRUE))
FittedModel2 = fit.variogram(TheVariogram2, model=TheVariogramModel2,
                             fit.sills = L2, fit.ranges= L2, fit.method = 1)

plot(TheVariogram2, model=FittedModel2, main='Fitted Variogram Models without
     Nugget Effect')

# Fitting separately
v0 = TheVariogram2[1:15,1:6]
v45 = TheVariogram2[16:30,1:6]
v90 = TheVariogram2[31:45,1:6]
v135 = TheVariogram2[46:60,1:6]

f0 = fit.variogram(v0, model=TheVariogramModel2, fit.sills = L2,
                   fit.ranges= L2, fit.method = 1)

f45 = fit.variogram(v45, model=TheVariogramModel2, fit.sills = L2,
                    fit.ranges= L2, fit.method = 1)

f90 = fit.variogram(v90, model=TheVariogramModel2, fit.sills = L2,
                     fit.ranges= L2, fit.method = 1)

f135 = fit.variogram(v135, model=TheVariogramModel2, fit.sills = L2,
                     fit.ranges= L2, fit.method = 1)

# Evaluating the quality of fitting using SSErr
sum((v0$gamma-variogramLine(f0, dist_vector =v0$dist)$gamma)^2)
sum((v45$gamma-variogramLine(f45, dist_vector =v45$dist)$gamma)^2)
sum((v90$gamma-variogramLine(f90, dist_vector =v90$dist)$gamma)^2)  # The minimum
sum((v135$gamma-variogramLine(f135, dist_vector =v135$dist)$gamma)^2)

######################## Universal Kriging Variogram ########################
mydata4 = read.csv('MT10_scaled_lasso.csv')
vars = names(mydata4)[467:483]
formula = as.formula(paste("Mortality_Rate ~ ", paste(vars, collapse = "+")))

# With nugget effect
TheVariogram4 = variogram(formula, data=mydata3, alpha=c(0,45,90,135))
plot(TheVariogram4)  #Experimental variogram
TheVariogramModel4 = vgm(psill=var(mydata3$Mortality_Rate)-7500, model="Sph",
                         nugget=7500, tol.hor = 22.5, range = 20,
                         anis = c(090, 0.2))  #Theoretical variogram

L1 = as.logical(c(TRUE, FALSE))
FittedModel4 = fit.variogram(TheVariogram4, model=TheVariogramModel4,
                             fit.sills = L1, fit.ranges= L1, fit.method = 1)

plot(TheVariogram4, model=FittedModel4, main='Fitted Variogram Models with
     Nugget Effect')

# Fitting separately
vn0 = TheVariogram4[1:15,1:6]
vn45 = TheVariogram4[16:30,1:6]
vn90 = TheVariogram4[30:45,1:6]
vn135 = TheVariogram4[46:60,1:6]

fn0 = fit.variogram(vn0, model=TheVariogramModel4, fit.sills = L1,
                    fit.ranges= L1, fit.method = 1)

fn45 = fit.variogram(vn45, model=TheVariogramModel4, fit.sills = L1,
                     fit.ranges= L1, fit.method = 1)

fn90 = fit.variogram(vn90, model=TheVariogramModel4, fit.sills = L1,
                     fit.ranges= L1, fit.method = 1)

fn135 = fit.variogram(vn135, model=TheVariogramModel4, fit.sills = L1,
                      fit.ranges= L1, fit.method = 1)

# Evaluating the quality of fitting using SSErr
sum((vn0$gamma-variogramLine(fn0, dist_vector =vn0$dist)$gamma)^2)
sum((vn45$gamma-variogramLine(fn45, dist_vector =vn45$dist)$gamma)^2)
sum((vn90$gamma-variogramLine(fn90, dist_vector =vn90$dist)$gamma)^2)  # The minimum
sum((vn135$gamma-variogramLine(fn135, dist_vector =vn135$dist)$gamma)^2)

# Without nugget effect
# Compute undirected (isotropic) empirical/ Sample/ Experimental semivariogram
TheVariogram5 = variogram(formula, data=mydata3, alpha=c(0,45,90,135))

plot(TheVariogram5)  #Experimental variogram

TheVariogramModel5 = vgm(psill=var(mydata3$Mortality_Rate), model="Sph",
                         nugget = 0, range = 20 ,anis = c(090, 0.2),
                         tol.hor = 22.5)  #Theoretical variogram

plot(TheVariogram5, model=TheVariogramModel5)

L2 = as.logical(c(FALSE, TRUE))
FittedModel5 = fit.variogram(TheVariogram5, model=TheVariogramModel5,
                             fit.sills = L2, fit.ranges= L2, fit.method = 1)

plot(TheVariogram5, model=FittedModel5, main='Fitted Variogram Models without
     Nugget Effect')

# Fitting separately
v0 = TheVariogram5[1:15,1:6]
v45 = TheVariogram5[16:30,1:6]
v90 = TheVariogram5[31:45,1:6]
v135 = TheVariogram5[46:60,1:6]

f0 = fit.variogram(v0, model=TheVariogramModel5, fit.sills = L2,
                   fit.ranges= L2, fit.method = 1)

f45 = fit.variogram(v45, model=TheVariogramModel5, fit.sills = L2,
                    fit.ranges= L2, fit.method = 1)

f90 = fit.variogram(v90, model=TheVariogramModel5, fit.sills = L2,
                    fit.ranges= L2, fit.method = 1)

f135 = fit.variogram(v135, model=TheVariogramModel5, fit.sills = L2,
                     fit.ranges= L2, fit.method = 1)

# Evaluating the quality of fitting using SSErr
sum((v0$gamma-variogramLine(f0, dist_vector =v0$dist)$gamma)^2)
sum((v45$gamma-variogramLine(f45, dist_vector =v45$dist)$gamma)^2)  # The minimum
sum((v90$gamma-variogramLine(f90, dist_vector =v90$dist)$gamma)^2)
sum((v135$gamma-variogramLine(f135, dist_vector =v135$dist)$gamma)^2)

######################## Griding Locations ########################
# create sequences that represent the center of the columns of pixels
# change "by" to change the resolution of the object
Columns = seq(from=-125, to=-67, by=0.1)

# And the rows of pixels:
Rows = seq(from=25, to=50, by=0.1)

# Create a grid of "Pixels" using x as columns and y as rows
TheGrid = expand.grid(x=Columns,y=Rows)

# Convert Thegrid to a SpatialPixels class
# coordinates(TheGrid) = ~ x+y
# gridded(TheGrid) = TRUE
gridded(TheGrid) = ~x+y

# Plot the intepolation grid and points
plot(TheGrid, cex=1)
points(mydata3, pch=16, col='blue', cex=1)
title(paste('Interpolation Grid and Sample Points', 
            paste(nrow(TheGrid@coords), 'Pixels', sep = " "),
            sep = "\n"), cex.main = 2.5, line = -5)

# Cropping the grid map
# gc()
# TheGrid.Cropped = crop(TheGrid, mydata2)
# plot(TheGrid.Cropped)
# title("Cropped Grid Map")

######################## Universal Kriging Grid ########################
mydata2@data = mydata3@data
# grid = makegrid(mydata2, cellsize =0.2)  # cell size in map units!
# grid = SpatialPoints(grid, proj4string = CRS(proj4string(mydata2)))
# grid = SpatialPixels(grid, proj4string = CRS(proj4string(mydata2)))
grid = SpatialPixels(TheGrid, proj4string = CRS(proj4string(mydata2)))

# Using 'over' function of sp package for overlaying task
grid.over = over(grid, mydata2)  # grid is overlaid by mydata2
grid.over = cbind(grid.over, grid@coords)
coordinates(grid.over) = ~x+y
grid.over = SpatialPixelsDataFrame(grid.over, data = grid.over@data,
                                   proj4string = CRS(proj4string(mydata2)))

######################## Ordinary Kriging ########################
# Creating variogram model to be used in krige function
# With nugget effect
mor.vm.ani1 = vgm(psill = fn90$psill[2], fn90$model[2], fn90$range[2],
                  anis = c(fn90$ang1[2], fn90$anis1[2]), nugget = fn90$psill[1])

# Without nugget effect
mor.vm.ani2 = vgm(psill = f90$psill[2], f90$model[2], f90$range[2],
                  anis = c(f90$ang1[2], f90$anis1[2]), nugget = f90$psill[1])

# Let's try some ordinary kriging with unlimited search
Kriging.start.time = Sys.time()
Mortality_Rate.kriged = krige(Mortality_Rate ~ 1 , mydata3, TheGrid,
                              model = mor.vm.ani2, maxdist = Inf,
                              nmin = 0, omax = Inf)
Kriging.end.time = Sys.time()
Kriging.time.taken = Kriging.end.time - Kriging.start.time
Kriging.time.taken

summary(Mortality_Rate.kriged)

# Visualize the estimates
my.palette.bre = brewer.pal(n = 8, name = "GnBu")  # Creating a palette
my.palette.vir = viridis_pal(alpha = 0.6, option = 'A')  # Creating a palette

mydata2.poly = SpatialPolygons(mydata2@polygons)
poly = list("sp.polygons", mydata2.poly, col = "black")

# Kriging Estimations
# With viridis
spplot(Mortality_Rate.kriged["var1.pred"], colorkey = TRUE,
       main = "Mortality Rate kriged", col.regions = my.palette.vir,
       sp.layout = poly)

# With RColorBrewer
#Note that number of cuts is 1 minus the number of colors.
spplot(Mortality_Rate.kriged["var1.pred"], colorkey = TRUE, cuts = 7,
       main = "Mortality Rate kriged", col.regions = my.palette.bre,
       sp.layout = poly)

# Kriging Estimation Variance
# With viridis
spplot(Mortality_Rate.kriged["var1.var"], colorkey = TRUE,
       main = "Mortality Rate Kriging Variance",
       col.regions = my.palette.vir, sp.layout = poly)

# With RColorBrewer
spplot(Mortality_Rate.kriged["var1.var"], colorkey = TRUE, cuts = 7,
       main = "Mortality Rate Kriging Variance",
       col.regions = my.palette.bre, sp.layout = poly)

######################## Simple Kriging ########################
# Let's try some simple kriging with unlimited search
Kriging.start.time = Sys.time()
Mortality_Rate.kriged = krige(Mortality_Rate ~ 1 ,mydata3, TheGrid,
                              model = mor.vm.ani2, maxdist = Inf, nmin = 0,
                              omax = Inf, beta = mean(mydata3$Mortality_Rate))
Kriging.end.time = Sys.time()
Kriging.time.taken = Kriging.end.time - Kriging.start.time
Kriging.time.taken

summary(Mortality_Rate.kriged)

# Kriging Estimations
# With viridis
spplot(Mortality_Rate.kriged["var1.pred"], colorkey = TRUE,
       main = "Mortality Rate kriged", col.regions = my.palette.vir,
       sp.layout = poly)

# With RColorBrewer
spplot(Mortality_Rate.kriged["var1.pred"], colorkey = TRUE, cuts = 7,
       main = "Mortality Rate kriged", col.regions = my.palette.bre,
       sp.layout = poly)

# Kriging Estimation Variance
# With viridis
spplot(Mortality_Rate.kriged["var1.var"], colorkey = TRUE,
       main = "Mortality Rate Kriging Variance",
       col.regions = my.palette.vir, sp.layout = poly)

# With RColorBrewer
spplot(Mortality_Rate.kriged["var1.var"], colorkey = TRUE, cuts = 7,
       main = "Mortality Rate Kriging Variance",
       col.regions = my.palette.bre, sp.layout = poly)

######################## Universal Kriging ########################
# Creating variogram model to be used in krige function
# With nugget effect
mor.vm.ani4 = vgm(psill = fn90$psill[2], fn90$model[2], fn90$range[2],
                  anis = c(fn90$ang1[2], fn90$anis1[2]), nugget = fn90$psill[1])

# Without nugget effect
mor.vm.ani5 = vgm(psill = f45$psill[2], f45$model[2], f45$range[2],
                  anis = c(f45$ang1[2], f45$anis1[2]), nugget = f45$psill[1])

# Let's try some universal kriging with unlimited search
Kriging.start.time = Sys.time()
Mortality_Rate.kriged = krige(formula = formula, mydata3,
                              grid.over, model = mor.vm.ani4, maxdist = Inf,
                              nmin = 0, omax = Inf)
Kriging.end.time = Sys.time()
Kriging.time.taken = Kriging.end.time - Kriging.start.time
Kriging.time.taken

summary(Mortality_Rate.kriged)

# Kriging Estimations
# With viridis
spplot(Mortality_Rate.kriged["var1.pred"], colorkey = TRUE,
       main = "Mortality Rate kriged", col.regions = my.palette.vir,
       sp.layout = poly)  #xlim=(-127:-66), ylim=(24:50)

# With RColorBrewer
spplot(Mortality_Rate.kriged["var1.pred"], colorkey = TRUE, cuts = 7,
       main = "Mortality Rate kriged", col.regions = my.palette.bre,
       sp.layout = poly)

# Kriging Estimation Variance
# With viridis
spplot(Mortality_Rate.kriged["var1.var"], colorkey = TRUE,
       main = "Mortality Rate Kriging Variance",
       col.regions = my.palette.vir, sp.layout = poly)

# With RColorBrewer
spplot(Mortality_Rate.kriged["var1.var"], colorkey = TRUE, cuts = 7,
       main = "Mortality Rate Kriging Variance",
       col.regions = my.palette.bre, sp.layout = poly)

######################## Kriging Model Evaluation ########################
# Kriging model evaluation for universal kriging model
Kriged.object = cbind(Mortality_Rate.kriged$var1.pred, Mortality_Rate.kriged@coords)
Kriged.object = as.data.frame(Kriged.object)
Kriged.object = na.omit(Kriged.object)

coordinates(Kriged.object) = ~x+y
Kriged.object = SpatialPixelsDataFrame(Kriged.object, data = Kriged.object@data,
                                       proj4string = CRS(proj4string(mydata2)))

Kriged.object@proj4string@projargs = "+proj=longlat +ellps=GRS80 +no_defs"
Kriged.object.over = over(mydata2, Kriged.object, fn = mean)
Kriged.object.over = cbind(Kriged.object.over$V1, mydata3$Mortality_Rate)
Kriged.object.over = na.omit(Kriged.object.over)
Kriged.object.over = as.data.frame(Kriged.object.over)
colnames(Kriged.object.over)[1] = "Predicted"
colnames(Kriged.object.over)[2] = "Actual"

# Residual sum of squares
RSS = sum((Kriged.object.over$Actual - Kriged.object.over$Predicted)^2)
# Total sum of squares
TSS = sum((Kriged.object.over$Actual - mean(Kriged.object.over$Actual))^2)
# R-squared
R_squared = 1 - RSS/TSS
# Mean squared error
MSE = mean((Kriged.object.over$Actual - Kriged.object.over$Predicted)^2)
# Root mean squared error
RMSE = sqrt(MSE)
# Mean absolute error
MAE = mean(abs(Kriged.object.over$Actual - Kriged.object.over$Predicted))

# R-squared
R_squared
# Mean squared error
MSE
# Root mean squared error
RMSE
# Mean absolute error
MAE

# Items in mydata4 and not in Kriged.object.over
mydata4$Mortality_Rate[!(mydata4$Mortality_Rate %in% Kriged.object.over$Actual)]
