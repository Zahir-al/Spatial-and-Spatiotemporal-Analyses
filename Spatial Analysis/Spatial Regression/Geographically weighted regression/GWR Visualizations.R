library(rgdal)
library(usmap)
library(ggplot2)
library(viridis)

CountyMap = readOGR(dsn='D:/Thesis/Working Directory/Modified County Map',
                    layer='CountyMap')

data = data.frame(CountyMap@data$COUNTY, CountyMap@data$Mortality)

colnames(data)[1] = 'fips'
colnames(data)[2] = 'X'


# Plot of Mortality Rate (per 100,000) ----

plot_usmap(regions = "counties", data=data, values = 'X', color = "gray90",
           size = 0.5) +  # Set color = FALSE to remove border lines
  scale_fill_viridis(option = 'mako', alpha = .8)+
  labs(fill = "Mortality Rate", title = 'Mortality Rate',
       subtitle = 'Mortality Rate (per 100,000)') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 5),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

ggsave(filename = 'Mortality Rate (per 100,000).jpeg',device='jpeg', dpi=500)

# Plot of Local R2 till 2020-05-06 ----
data = read.csv('Three_M_Mortality_Rate_results.csv')
library(matrixStats)

colCounts(data[,65:82]<= 0.05)   #77,79,71
data = data.frame(CountyMap@data$COUNTY, data$localR2)
colnames(data)[1] = 'fips'
colnames(data)[2] = 'X'

plot_usmap(regions = "counties", data=data, values = 'X', color = FALSE) +
  scale_fill_viridis(option = 'magma', alpha = .8)+
  labs(fill = "Local R2", title = 'Local R2',
       subtitle = 'Local R2 till 2020-05-06') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

data[which(data[,2]<= 0, arr.ind = TRUE),]
data[which(data[,2]>= 0.9, arr.ind = TRUE),]

ggsave(filename = 'Local R2 till 2020-05-06.jpeg',device='jpeg', dpi=500)

# Plot of Local R2 till 2020-08-06 ----
data = read.csv('Six_M_Mortality_Rate_results.csv')
colCounts(data[,65:82]<= 0.05)  #78,79,76
data = data.frame(CountyMap@data$COUNTY, data$localR2)
colnames(data)[1] = 'fips'
colnames(data)[2] = 'X'

plot_usmap(regions = "counties", data=data, values = 'X', color = FALSE) +
  scale_fill_viridis(option = 'magma', alpha = .8)+
  labs(fill = "Local R2", title = 'Local R2',
       subtitle = 'Local R2 till 2020-08-06') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

data[which(data[,2]<= 0, arr.ind = TRUE),]
data[which(data[,2]>= 0.90, arr.ind = TRUE),]

ggsave(filename = 'Local R2 till 2020-08-06.jpeg',device='jpeg', dpi=500)

# Plot of Local R2 till 2020-11-06 ----
data = read.csv('Nine_M_Mortality_Rate_results.csv')
colCounts(data[,65:82]<= 0.05)  #78,1,79
data = data.frame(CountyMap@data$COUNTY, data$localR2)
colnames(data)[1] = 'fips'
colnames(data)[2] = 'X'

plot_usmap(regions = "counties", data=data, values = 'X', color = FALSE) +
  scale_fill_viridis(option = 'magma', alpha = .8)+
  labs(fill = "Local R2", title = 'Local R2',
       subtitle = 'Local R2 till 2020-11-06') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

data[which(data[,2]>= 0.88, arr.ind = TRUE),]

ggsave(filename = 'Local R2 till 2020-11-06.jpeg',device='jpeg', dpi=500)

# Plot of Local R2 till 2021-02-06 ----
data = read.csv('Twelve_M_Mortality_Rate_results.csv')
colCounts(data[,65:82]<= 0.05)  #69,73,1
data = data.frame(CountyMap@data$COUNTY, data$localR2)
colnames(data)[1] = 'fips'
colnames(data)[2] = 'X'

plot_usmap(regions = "counties", data=data, values = 'X', color = FALSE) +
  scale_fill_viridis(option = 'inferno', alpha = .8)+
  labs(fill = "Local R2", title = 'Local R2',
       subtitle = 'Local R2 till 2021-02-06') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

data[which(data[,2]>= 0.9, arr.ind = TRUE),]

ggsave(filename = 'Local R2 till 2021-02-06.jpeg',device='jpeg', dpi=500)

# Plot of Local R2 till 2021-04-25 ----
data = read.csv('GWR_session_results.csv')
colCounts(data[,65:82]<= 0.05)  #1,73,67
data = data.frame(CountyMap@data$COUNTY, data$localR2)
colnames(data)[1] = 'fips'
colnames(data)[2] = 'X'

plot_usmap(regions = "counties", data=data, values = 'X', color = FALSE) +
  scale_fill_viridis(option = 'magma', alpha = .8)+
  labs(fill = "Local R2", title = 'Local R2',
       subtitle = 'Local R2 till 2021-04-25') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

data[which(data[,2]>= 0.9, arr.ind = TRUE),]

ggsave(filename = 'Local R2 till 2021-04-25.jpeg',device='jpeg', dpi=500)

# Plot for Coefficients, VIFs and VDPs ----
data = read.csv('GWR_session_results_collinearity.csv')
data1 = data.frame(CountyMap@data$COUNTY, data$beta_percent_adults_with_diabetes)

colnames(data1)[1] = 'fips'
colnames(data1)[2] = 'X'

plot_usmap(regions = "counties", data=data1, values = 'X', color = FALSE) +
  scale_fill_viridis(option = 'plasma', alpha = .8)+
  labs(fill = 'VIF', title = 'Percentage of Adults with Diabetes') +
  theme(panel.background=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title=element_text(size=13, hjust=0.05, vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.05, vjust=-1),
        legend.key.height = unit(0.32, "cm"),
        legend.key.width = unit(0.32, "cm"),
        legend.position = c(.97,.30))

sum(data[data[,81] <0.05 ,27]>0)
data1[data1[,2] <0 ,2]

ggsave(filename = 'Percentage of Adults with Diabetes.jpeg',device='jpeg', dpi=500)
