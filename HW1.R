library(rvest)
library(reshape2)
library(lattice)

# Q2

wiki_url <- read_html("http://wiki.socr.umich.edu/index.php/SOCR_Data_PD_BiomedBigMetadata")
html_nodes(wiki_url, "#content")

pd_data <- html_table(html_nodes(wiki_url, "table")[[1]])

df = data.frame(pd_data)
df1 = subset(df, select = c('Cases', 'Sex', 'Weight', 'Age', 'Time'))
dfw = reshape(df1, idvar = 'Cases', timevar = 'Time', direction = 'wide')

# Q3

summary(df1$Sex)
summary(df1$Age)
summary(df1$Weight)

# Q4

df_t0 = subset(df, df$Time == 0)

# First 10 obs
df_t0_1 = df_t0[1:10,]

# L_caudate_ComputeArea < 600
df_t0_2 = subset(df_t0, df_t0$L_caudate_ComputeArea < 600)

# Sort based on L_caudate_Volume in descending and ascending order
df_t0_3 = df_t0[rev(order(df_t0$L_caudate_Volume)),]
df_t0_4 = df_t0[order(df_t0$L_caudate_Volume),]

# Generate frequency and probability tables for Age and Sex
table(df_t0$Age)
prop.table(table(df_t0$Age))
table(df_t0$Sex)
prop.table(table(df_t0$Sex))

# Compute the mean Age and the correlation between Age and Weight
mean(df_t0$Age)
cor(df_t0$Age, df_t0$Weight)

# Plot Histogram and density of R_fusiform_gyrus_Volume and scatterplot L_fusiform_gyrus_Volume and R_fusiform_gyrus_Volume
hist(df_t0$R_fusiform_gyrus_Volume)
plot(density(df_t0$R_fusiform_gyrus_Volume))
plot(df_t0$L_fusiform_gyrus_Volume, df_t0$R_fusiform_gyrus_Volume)

# Q5
# Generate 1,000 standard normal variables and 1,200 student t distributed random variables with df=20 and generate a 
# quantile-quantile (Q-Q) probability plot of the 2 samples. Then, compare it with qqnorm of student t simulation.

x = rnorm(1000)
y = rt(1200, 20)

qqplot(x, y, main = 'Normal and t-dist qq plot')
qqnorm(y, main = 't-dist qq plot of the data',
       xlab = 'Theoretical Quantailes of the normal',
       ylab = 'Sample Quantiles of the data')
qqline(y)

# Q6

a = c(1,2,3,4,5,6,7,8)
mean(a)

meanfnc = function(x){
  m = 0
  for(i in 1:length(x)){
    m = m + x[i]
  }
  m = m/length(x)
  return(m)
}
meanfnc(a)

