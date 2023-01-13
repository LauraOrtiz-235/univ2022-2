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



### ANÁLISIS 4 ###

anova <- aov(df$Rating ~ df$Broad_Bean_Origin)
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
library(magrittr)
library(rcartocolor)


######COMPANY LOCATION VS RATING TOP

mean_rating = aggregate(df$Rating, list(df$Company_Location), FUN=mean)
mean_sorted <- mean_rating[order(mean_rating$x,
                                decreasing = TRUE), ]

top10_Rating <-mean_sorted[1:10,]
top10_Rating

df1 <- df[row.names(df) %in% 1:n, ]

ggplot(top10_Rating, aes(x=Group.1, y=x)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(3,4))

top_box_choc <- df %>%
  filter(Company_Location %in% top10_Rating$Group.1)
top_box_choc

ggplot(top_box_choc, aes(Company_Location, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

######COMPANY LOCATION VS RATING BOT
mean_rating = aggregate(df$Rating, list(df$Company_Location), FUN=mean)
mean_sorted <- mean_rating[order(mean_rating$x,
                                 decreasing = FALSE), ]
top10_Rating <-mean_sorted[1:10,]
top10_Rating


ggplot(top10_Rating, aes(x=Group.1, y=x)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(2,3))

top_box_choc <- df %>%
  filter(Company_Location %in% top10_Rating$Group.1)
top_box_choc

ggplot(top_box_choc, aes(Company_Location, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))




#####################################

bean_rating = BASE_DATOS_CHOCOLATE_LIMPIA %>%
  count(Bean_Type)
bean_rating
colnames(bean_rating)[1] = "Bean_Type"

mean_rating = aggregate(df$Rating, list(df$Bean_Type), FUN=mean)
colnames(mean_rating)[1] = "Bean_Type"


######BEAN TYPE VS RATING TOP

bean_pounde = merge(mean_rating,bean_rating)
bean_pounde

prom_ponde = c(bean_pounde$x * bean_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$Bean_Type))
prom_ponde

bean_pounde$prom_pon <- prom_ponde
bean_pounde

mean_sorted <- bean_pounde[order(bean_pounde$prom_pon,
                                 decreasing = TRUE), ]

top10_Rating <- mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Bean_Type, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="lightblue") + coord_cartesian(ylim=c(0,1.5))

top_box_choc <- df %>%
  filter(Bean_Type %in% top10_Rating$Bean_Type)
top_box_choc

ggplot(top_box_choc, aes(Bean_Type, Rating)) + 
  geom_boxplot(fill="lightblue") + theme(axis.text.x = element_text(angle = 90))


######BEAN TYPE VS RATING BOT

bean_pounde = merge(mean_rating,bean_rating)
bean_pounde

prom_ponde = c(bean_pounde$x * bean_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$Bean_Type))
prom_ponde

bean_pounde$prom_pon <- prom_ponde
bean_pounde

mean_sorted <- bean_pounde[order(bean_pounde$prom_pon,
                                 decreasing = FALSE), ]

bot10_Rating <-mean_sorted[1:10,]
bot10_Rating

ggplot(bot10_Rating, aes(x=Bean_Type, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="lightblue") + coord_cartesian(ylim=c(0.0001,0.003))

bot_box_choc <- df %>%
  filter(Bean_Type %in% bot10_Rating$Bean_Type)
bot_box_choc

ggplot(bot_box_choc, aes(Bean_Type, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))



#####################################

company_rating = BASE_DATOS_CHOCOLATE_LIMPIA %>%
  count(`Company `)
company_rating
colnames(company_rating)[1] = "Company"

mean_rating = aggregate(df$Rating, list(df$`Company `), FUN=mean)
mean_rating
colnames(mean_rating)[1] = "Company"


######COMPANY VS RATING TOP

comp_pounde = merge(mean_rating,company_rating)
comp_pounde

prom_ponde = c(comp_pounde$x * comp_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$`Company `))
prom_ponde

comp_pounde$prom_pon <- prom_ponde
comp_pounde

mean_sorted <- comp_pounde[order(comp_pounde$prom_pon,
                                 decreasing = TRUE), ]

top10_Rating <- mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Company, y=prom_pon)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(0.03,0.1))

top_box_choc <- df %>%
  filter(Bean_Type %in% top10_Rating$Bean_Type)
top_box_choc

ggplot(top_box_choc, aes(Bean_Type, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


######COMPANY VS RATING BOT

comp_pounde = merge(mean_rating,company_rating)
comp_pounde

prom_ponde = c(comp_pounde$x * comp_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$`Company `))
prom_ponde

comp_pounde$prom_pon <- prom_ponde
comp_pounde

mean_sorted <- comp_pounde[order(comp_pounde$prom_pon,
                                 decreasing = FALSE), ]

bot10_Rating <-mean_sorted[1:10,]
bot10_Rating

ggplot(bot10_Rating, aes(x=Company, y=prom_pon)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(0.000001,0.003))

bot_box_choc <- df %>%
  filter(`Company ` %in% bot10_Rating$`Company `)
bot_box_choc

ggplot(bot_box_choc, aes(`Company `, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))



#####################################

broad_bean_rating = BASE_DATOS_CHOCOLATE_LIMPIA %>%
  count(Broad_Bean_Origin)
bean_rating
colnames(bean_rating)[1] = "Broad_Bean_Origin"

mean_rating = aggregate(df$Rating, list(df$Broad_Bean_Origin), FUN=mean)
colnames(mean_rating)[1] = "Broad_Bean_Origin"


######BEAN TYPE VS RATING TOP

broad_bean_pounde = merge(mean_rating,broad_bean_rating)
broad_bean_pounde

prom_ponde = c(broad_bean_pounde$x * broad_bean_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$Broad_Bean_Origin))
prom_ponde

broad_bean_pounde$prom_pon <- prom_ponde
broad_bean_pounde

mean_sorted <- broad_bean_pounde[order(broad_bean_pounde$prom_pon,
                                 decreasing = TRUE), ]

top10_Rating <- mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Broad_Bean_Origin, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="lightgreen") + coord_cartesian(ylim=c(0,0.5))

top_box_choc <- df %>%
  filter(Broad_Bean_Origin %in% top10_Rating$Broad_Bean_Origin)
top_box_choc

ggplot(top_box_choc, aes(Broad_Bean_Origin, Rating)) + 
  geom_boxplot(fill="lightgreen") + theme(axis.text.x = element_text(angle = 90))


######BEAN TYPE VS RATING BOT

broad_bean_pounde = merge(mean_rating,broad_bean_rating)
broad_bean_pounde

prom_ponde = c(broad_bean_pounde$x * broad_bean_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$Broad_Bean_Origin))
prom_ponde

broad_bean_pounde$prom_pon <- prom_ponde
broad_bean_pounde

mean_sorted <- broad_bean_pounde[order(broad_bean_pounde$prom_pon,
                                       decreasing = FALSE), ]

bot10_Rating <-mean_sorted[1:10,]
bot10_Rating

ggplot(bot10_Rating, aes(x=Broad_Bean_Origin, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="lightgreen") + coord_cartesian(ylim=c(0.000001,0.003))

bot_box_choc <- df %>%
  filter(Broad_Bean_Origin %in% bot10_Rating$Broad_Bean_Origin)
bot_box_choc

ggplot(bot_box_choc, aes(Broad_Bean_Origin, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))




top_box_choc <- df %>%
  filter(`Company ` %in% top10_Rating$Company)
top_box_choc

colnames(top_box_choc)[1] = "Company"

bean_type = c(top_box_choc$Bean_Type)

see_num_comp_bean <- top_box_choc %>%  group_by(Company) %>% 
  +   summarise(a = bean_type)

company_vs_bean = data.frame("Company" = (top_box_choc)[1], "Bean_Type" = top_box_choc$Bean_Type)
company_vs_bean 

df <- data.frame(a = c("a","a","a","b","b","b"), c = c("d","d","d","d","e","e"))

df.new <- df %>%
  group_by(a) %>%
  summarise(new_strs = c(c))







###############

ggplot(df, aes(Broad_Bean_Origin, Rating)) + 
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





