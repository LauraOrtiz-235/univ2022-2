### ANÁLISIS 1 ###

df = BASE_DATOS_CHOCOLATE_LIMPIA

summary(df)

anova <- aov(df$Rating ~ df$Company_Location)
summary(anova)

sum_test = unlist(summary(anova))
names(sum_test)

Fs = ((sum_test["Sum Sq1"])/(sum_test["Df1"]))/((sum_test["Sum Sq2"])/(sum_test["Df2"])) 
Fs

fish = qf(1-0.05,df1=sum_test["Df1"],df2=sum_test["Df2"])
fish

#Como FS > a fisher son significativamente diferentes 


### ANÁLISIS 2 ###

anova <- aov(df$Rating ~ df$`Company `)
summary(anova)

sum_test = unlist(summary(anova))
names(sum_test)

sum_test["Df1"]

Fs = ((sum_test["Sum Sq1"])/(sum_test["Df1"]))/((sum_test["Sum Sq2"])/(sum_test["Df2"])) 
Fs

fish = qf(1-0.05,df1=sum_test["Df1"],df2=sum_test["Df2"])
fish


### ANÁLISIS 3 ###

anova <- aov(df$Rating ~ df$Bean_Type)
summary(anova)

sum_test = unlist(summary(anova))
names(sum_test)

sum_test["Df1"]

Fs = ((sum_test["Sum Sq1"])/(sum_test["Df1"]))/((sum_test["Sum Sq2"])/(sum_test["Df2"])) 
Fs

fish = qf(1-0.05,df1=sum_test["Df1"],df2=sum_test["Df2"])
fish


### GRÁFICAS ###

library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)


# GRÁFICA LOCATION COMPANY - RATING

mean_rating = aggregate(df$Rating, list(df$Company_Location), FUN=mean)

mean_sorted <- mean_rating[order(mean_rating$x,
                                 decreasing = TRUE),]

top10 <- mean_sorted[1:10,]

ggplot(top10, aes(x=Group.1, y=x)) +
  geom_bar(stat = 'identity')

top1010 = Company_Location('Amsterdam')

ggplot(df, aes(top1010, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

df %>%  group_by(Company_Location) %>% 
  +   summarise(mean = mean(amount), sum = sum(amount), n = n())



ggplot(df, aes(Broad_Bean_Origin, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Bean_Type, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Bean_Type, Cocoa_Percent)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Company_Location, Cocoa_Percent)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


### ANáLISIS 4 ###

choco = cbind(df$Rating, df$Cocoa_Percent)

acp = prcomp(choco, center = TRUE, scale = TRUE)
acp2 = prcomp(choco, center = TRUE)

S = cov(choco)
S

eigen_vec = eigen(S)$vectors
eigen_vec
eigen_val = eigen(S)$values

summary(acp)
summary(acp2)

acp$rotation
acp2$rotation



qqnorm(df$Cocoa_Percent, pch = 20, col = "grey50", main='QQplot para el porcentaje de cacao')
qqline(df$Cocoa_Percent, col = "steelblue", lwd = 2)

install.packages("car")

qqnorm(df$Rating, pch = 19, col = "grey50", main='QQplot para el Rating de la barra')
qqline(df$Rating, col = "steelblue", lwd = 2)





