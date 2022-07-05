setwd("~/Nakul/Advanced Statistical Methods")
df = read.csv("project.csv")
head(df)
str(df)
View(df)

df$DATE = as.Date(df$DATE)

summary(df)

library(ggplot2)
library(hrbrthemes)

ggplot(df, aes(x = DATE , y = IMPORTS))+
  geom_line(color="#69b3a2", size=1, alpha=0.9, linetype=2)+
  theme_ipsum_tw() +
  ggtitle("Imports throughout the years")

ggplot(df, aes(x = DATE , y = EXPORTS, fill = GDP))+
  geom_bar(stat = 'identity')+
  theme_ipsum_pub() +
  ggtitle("Yearly exports by GDP")

ggplot(df, aes(x = DATE , y = TOURISM_REVENUE))+
  geom_line(color="#69b3a2", size=1, alpha=0.9, linetype=2)+
  theme_ipsum_tw() +
  ylab("Tourism Revenue")+
  ggtitle("Tourism Revenue throughout the years")

ggplot(df, aes(x = DATE , y = NCPI))+
  geom_area( fill = "lightblue",color = "black" ,linetype="dashed")+
  theme_ipsum_pub() +
  ggtitle("Consumer Price Inflation throughout the years")

ggplot(df, aes(x = DATE , y = TOTAL_DEBT))+
  geom_line(color="#69b3a2", size=1, alpha=0.9, linetype=2)+
  theme_ipsum_tw() +
  ylab("Total Debt")+
  ggtitle("Total Debt throughout the years")

cor.test(df$TOTAL_DEBT,df$NCPI)

names = c("TOURISM_REVENUE","EXCHANGE_RATE","NCPI","TOTAL_DEBT","EXPORTS","IMPORTS","GDP" )

my_df = df[names]

cor(my_df)

library(plot3D)
library(heatmaply)

heatmaply_cor(x = cor(my_df), xlab = "Features",
              ylab = "Features", k_col = 7, k_row = 7)

scatter2D(df$NCPI, df$IMPORTS, col = 'blue', pch = 19, cex = 1, bty = 'g', xlab = "NCPI", ylab = "IMPORTS(US BN)")

model1 = lm(df$GDP~df$TOURISM_REVENUE+df$EXCHANGE_RATE+df$NCPI+df$TOTAL_DEBT+df$EXPORTS+df$IMPORTS)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

df2 = read.csv("project_new.csv")

df2$DATE = as.Date(df2$DATE)

model2 = lm(df2$GDP~df2$TOURISM_REVENUE+df2$EXCHANGE_RATE+df2$NCPI+df2$TOTAL_DEBT+df2$EXPORTS+df2$IMPORTS)
summary(model2)
plot(model2)

df3 = read.csv("project_exp.csv")
df3$DATE = as.Date(df3$DATE)

model3 = lm(log(df3$GDP_Nominal)~df3$TOURISM_REVENUE+df3$EXCHANGE_RATE+df3$NCPI+df3$TOTAL_DEBT+df3$EXPORTS+df3$IMPORTS)
summary(model3)
plot(model3)

anova = aov(df$GDP~df$TOURISM_REVENUE+df$EXCHANGE_RATE+df$NCPI+df$TOTAL_DEBT+df$EXPORTS+df$IMPORTS)
summary(anova)


gdpts = ts(df$GDP,start = c(2016,4), end = c(2022,1), frequency = 12)
par(mfrow=c(1,1))
plot(gdpts)

ncpits = ts(df$NCPI,start = c(2016,4), end = c(2022,1), frequency = 12)
par(mfrow=c(1,1))
plot(ncpits)

trts = ts(df$TOURISM_REVENUE,start = c(2016,4), end = c(2022,1), frequency = 12)
par(mfrow=c(1,1))
plot(trts)

debtts = ts(df$TOTAL_DEBT,start = c(2016,4), end = c(2022,1), frequency = 12)
par(mfrow=c(1,1))
plot(debtts)

t.test(df$EXCHANGE_RATE,df$NCPI)
t.test(df$GDP,df$TOURISM_REVENUE)
t.test(df$GDP,df$EXCHANGE_RATE)

df.pca = prcomp(df[,c(2:7)], center = TRUE,scale. = TRUE)
summary(df.pca)
screeplot(df.pca, type = "lines")
str(df.pca)
