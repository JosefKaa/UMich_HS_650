library(rvest)
library(gmodels)
library(ggplot2)
library(reshape2)
library(plotly)
library(MASS)
library(unbalanced)
library(GGally)
library(mi)
library(betareg)

# Q1
# Load the following two datasets, generate summary statistics for all variables, plot some of the features (e.g., histograms, 
# box plots, density plots, etc.) of some variables, and save the data locally as CSV files

# ALS case-study Training data
alsdata = read.csv('C:/Users/Xylthx/Desktop/Semester 8/HS650/HW2/ALS_TrainingData_2223.csv', header = T)
summary(alsdata)
hist(alsdata$Age_mean, main = 'Histogram of mean age', xlab = 'Mean age')
boxplot(alsdata[,3:5], main = 'Boxplot for Albumin')
plot(density(alsdata$Albumin_median), main = 'Density plot for median albumin', xlab = 'Median albumin')
write.csv(alsdata, file = 'C:/Users/Xylthx/Desktop/Semester 8/HS650/HW2/ALS_TrainingData_forHW.csv')

# SOCR Knee Pain Data
wiki_url = read_html("http://wiki.socr.umich.edu/index.php/SOCR_Data_KneePainData_041409")
html_nodes(wiki_url, "#content")
socrdata = html_table(html_nodes(wiki_url, "table")[[2]])
socrdata = as.data.frame(socrdata)

summary(socrdata)
hist(socrdata$x, main = 'Histogram of x', xlab = 'x')
boxplot(socrdata[,1:2], main = 'Boxplot for x and Y')
plot(density(socrdata$x), main = 'Density plot for x', xlab = 'x')
write.csv(socrdata, file = 'C:/Users/Xylthx/Desktop/Semester 8/HS650/HW2/SOCR_data_forHW.csv')

# Q2
# Use ALS case-study data and long-format SOCR Parkinsons Disease data(extract rows with Time=0) 
# to explore some bivariate relations (e.g. bivariate plot, correlation, table, crosstable etc.)

wiki_url = read_html("http://wiki.socr.umich.edu/index.php/SOCR_Data_PD_BiomedBigMetadata")
html_nodes(wiki_url, "#content")
pd_data = html_table(html_nodes(wiki_url, "table")[[1]])
pd_data = data.frame(pd_data)
pd_0 = subset(pd_data, pd_data$Time == 0)

plot(x = alsdata$Age_mean, y = alsdata$Albumin_median, 
     main = 'Mean age vs median albumin', xlab = 'Mean age', ylab = 'Median albumin')
cor(alsdata$Age_mean, alsdata$Albumin_median)
table(pd_0$Sex, pd_0$Dx)
CrossTable(pd_0$Sex, pd_0$Dx)

# Use 07_UMich_AnnArbor_MI_TempPrecipitation_HistData_1900_2015 data to show the relations between 
# temperature and time. [Hint: use geom_line and geom_bar]

aadata = read.csv('C:/Users/Xylthx/Desktop/Semester 8/HS650/HW2/07_UMich_AnnArbor_MI_TempPrecipitation_HistData_1900_2015.csv', 
                  header = T)
ind = seq(1, 111, 5)
aadata1 = aadata[ind,]
aanew = melt(aadata1, id.vars = 'Year')
colnames(aanew) = c('Year', 'Month', 'Temperature')
aanew$Month = as.factor(aanew$Month)
aanew$Temperature = as.numeric(aanew$Temperature)

plot1 = ggplot(aanew, aes(Year, Temperature, group = Month, color = Month)) + geom_line()
plot1

bar = ggplot(aanew, aes(x = Year, y = Temperature, fill = Month)) + geom_col() + facet_grid(. ~ Month) + 
  scale_y_continuous(breaks = seq(10, 80, 10))
bar

# Q3
# Introduce (artificially) some missing data, impute the missing values and examine the differences between the original, 
# incomplete and imputed data in statistics.

n = 1000
m = 5
data = matrix(data = rnorm(5000, 10, 1), 1000, 5)
miss = sample(1:5000, 500)
data[miss] = NA
data = as.data.frame(data)
summary(data)

mdf = missing_data.frame(data)
show(mdf)
image(mdf)

imputations = mi(data, n.iter=5, n.chains=3, verbose=TRUE)
hist(imputations)

# Q4 
# Generate a surface plot for the SOCR Knee Pain Data illustrating the 2D distribution of locations of the patient 
# reported knee pain (use plotly and kernel density estimation).

socrdata$View = as.factor(socrdata$View)
kernal_density = with(socrdata, MASS::kde2d(x, Y, n = 50))
with(kernal_density, plot_ly(x=x, y=y, z=z, type="surface"))

# Q5
# Rebalance Parkinson's Disease data(extract rows with Time=0) according to disease(SWEED OR PD) and health (HC) using 
# synthetic minority oversampling (SMOTE) to ensure approximately equal cohort sizes. (Notice: need to set 1 as the minority 
# class.)

pd_0$Dx = as.factor(pd_0$Dx)
summary(pd_0$Dx)

# Binarize Dx based on disease or not
pd_0$Dx = ifelse(pd_0$Dx == 'HC', 'Control', 'Patient')
pd_0$Dx = as.factor(pd_0$Dx)
summary(pd_0$Dx)

pd_0$Control = ifelse(pd_0$Dx == 'Control', 1, 0)
pd_0$Control = as.factor(pd_0$Control)
summary(pd_0$Control)
pd_0$Sex = as.factor(pd_0$Sex)
summary(pd_0)

# Balancing cases
input = pd_0[ , -which(names(pd_0) %in% c("Cases", "Dx", "Control", 'Time'))]
output = pd_0$Control

data.1 = ubBalance(X = input, Y = output, type="ubSMOTE", percOver=250, percUnder=150, verbose=TRUE)
table(data.1$Y)
balancedData<-cbind(data.1$X, data.1$Y)
colnames(balancedData) <- c(colnames(input), "Control")

qqplot(input[, 5], balancedData [, 5])

# Q6
# Use the California Ozone Data to generate a summary report. Make sure include: summary for every variable, 
# structure of data, proper data type convert(if needed), discuss the tendency of the ozone average concentration 
# in terms of year's average for each location, explore the differences of the ozone concentration for area, explore 
# the change of ozone concentration as seasons.

wiki_url = read_html('http://wiki.socr.umich.edu/index.php/SOCR_Data_121608_OzoneData')
html_node(wiki_url, '#content')
ozone_data = html_table(html_nodes(wiki_url, "table")[[1]])

summary(ozone_data)

ozone_data$VARIABLE = as.factor(ozone_data$VARIABLE)
ozone_data$LOCATION = as.factor(ozone_data$LOCATION)
ozone_data$COMP_SITES = as.factor(ozone_data$COMP_SITES)

# Ozone, year vs annual concentration in different locations
ozone.plot1 = ggplot(ozone_data, aes(YEAR, ANNUAL, group = LOCATION, color = LOCATION)) + geom_line()
ozone.plot1
# We can see that the annual ozone concentration generally increases from 1980 to 2010.

# Ozone annual concentraion vs latitude, longitude, and elevation, in year 2010.
ozone_data06 = subset(ozone_data, YEAR == '2006')
ggplot(ozone_data06, aes(LATITUDE, LONGITUDE, color = ANNUAL)) + geom_point()

# Ozone concentration for location 2008
ozone_data08 = subset(ozone_data, LOCATION == '2008')
ozone_data08 = ozone_data08[, 4:16]
ozone_data08 = melt(ozone_data08, id.vars = 'YEAR')
colnames(ozone_data08) = c('YEAR', 'MONTH', 'CONC')
ozone.plot2 = ggplot(ozone_data08, aes(MONTH, CONC, group = YEAR, color = YEAR)) + geom_point()
ozone.plot2
# We can see that generally the ozone concentration is low in winter.















