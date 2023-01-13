# !diagnostics off
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

df1 <- df[row.names(df) %in% 1:n, ]

ggplot(top10_Rating, aes(x=Group.1, y=x)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(2,3))

top_box_choc <- df %>%
  filter(Company_Location %in% top10_Rating$Group.1)
top_box_choc

ggplot(top_box_choc, aes(Company_Location, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

#####################################

######BEAN TYPE VS RATING TOP
mean_rating = aggregate(df$Rating, list(df$Bean_Type), FUN=mean)
mean_sorted <- mean_rating[order(mean_rating$x,
                                 decreasing = TRUE), ]
top10_Rating <-mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Group.1, y=x)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(3,4))

top_box_choc <- df %>%
  filter(Bean_Type %in% top10_Rating$Group.1)
top_box_choc

ggplot(top_box_choc, aes(Bean_Type, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

######BEAN TYPE VS RATING BOT
mean_rating = aggregate(df$Rating, list(df$Bean_Type), FUN=mean)
mean_sorted <- mean_rating[order(mean_rating$x,
                                 decreasing = FALSE), ]
top10_Rating <-mean_sorted[1:10,]
top10_Rating

df1 <- df[row.names(df) %in% 1:n, ]

ggplot(top10_Rating, aes(x=Group.1, y=x)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(2,3))

top_box_choc <- df %>%
  filter(Bean_Type %in% top10_Rating$Group.1)
top_box_choc

ggplot(top_box_choc, aes(Bean_Type, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

###############

ggplot(df, aes(`Company `, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Broad_Bean_Origin, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Bean_Type, Rating)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Bean_Type, Cocoa_Percent)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


ggplot(df, aes(Company_Location, Cocoa_Percent)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

dev.off()

### ANáLISIS REGRESION LINEAL ###
reg_porcent_rating <- lm(Rating~Cocoa_Percent, data = df)
summary(reg_porcent_rating) 
plot(df$Cocoa_Percent, df$Rating, pch = 16)
abline(4.08060 , -0.01248)
abline(lm( df$Rating ~ df$Cocoa_Percent))

install.packages("corrplot")
library(corrplot)
res <- cor(df$Rating, df$Cocoa_Percent)
round(res, 2)
##


mean_rating_percent = aggregate(df$Rating, list(df$Cocoa_Percent), FUN=mean)
colnames(mean_rating_percent)[1] = "Cocoa_Percent"
  
bean_rating_percent = BASE_DATOS_CHOCOLATE_LIMPIA %>%
  count(Cocoa_Percent)
bean_rating_percent


bean_pounde = merge(mean_rating_percent,bean_rating_percent)
bean_pounde

prom_ponde = c(bean_pounde$x * bean_pounde$n / length(BASE_DATOS_CHOCOLATE_LIMPIA$Bean_Type))
prom_ponde

bean_pounde$prom_pon <- prom_ponde
bean_pounde

mean_sorted <- bean_pounde[order(bean_pounde$prom_pon,
                                 decreasing = TRUE), ]

top10_Rating <- mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Cocoa_Percent, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="#AF8EE1") + coord_cartesian(ylim=c(0.04,1.3))

##
######
anova <- aov(df$Rating ~ df$Bean_Type)
summary(anova)

sum_test = unlist(summary(anova))
names(sum_test)

sum_test["Df1"]

Fs = ((sum_test["Sum Sq1"])/(sum_test["Df1"]))/((sum_test["Sum Sq2"])/(sum_test["Df2"])) 
Fs

fish = qf(1-0.05,df1=sum_test["Df1"],df2=sum_test["Df2"])
fish
######
anova <- aov(df$Rating ~ df$Broad_Bean_Origin)
summary(anova)

sum_test = unlist(summary(anova))
names(sum_test)

sum_test["Df1"]

Fs = ((sum_test["Sum Sq1"])/(sum_test["Df1"]))/((sum_test["Sum Sq2"])/(sum_test["Df2"])) 
Fs

fish = qf(1-0.05,df1=sum_test["Df1"],df2=sum_test["Df2"])
fish
###########################-LOS JUZGADOS TIENEN PREFERENCIA POR COMPANIA-########################################

anova <- aov(df$Rating ~ df$`Company `)
summary(anova)

sum_test = unlist(summary(anova))
names(sum_test)

Fs = ((sum_test["Sum Sq1"])/(sum_test["Df1"]))/((sum_test["Sum Sq2"])/(sum_test["Df2"])) 
Fs

fish = qf(1-0.05,df1=sum_test["Df1"],df2=sum_test["Df2"])
fish

##
mean_rating_company = aggregate(df$Rating, list(df$`Company `), FUN=mean)
colnames(mean_rating_company)[1] = "Company"
bean_rating_company = BASE_DATOS_CHOCOLATE_LIMPIA %>%
  count(`Company `)
bean_rating_company
colnames(bean_rating_company)[1] = "Company"

bean_pounde = merge(mean_rating_company,bean_rating_company)
bean_pounde

prom_ponde = c(bean_pounde$x * bean_pounde$n / length(df$`Company `))
prom_ponde

bean_pounde$prom_pon <- prom_ponde
bean_pounde

mean_sorted <- bean_pounde[order(bean_pounde$prom_pon,
                                 decreasing = TRUE), ]

top10_Rating <- mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Company, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="#F5C37C") + coord_cartesian(ylim=c(0.020,0.1))


mean_sorted <- bean_pounde[order(bean_pounde$prom_pon,
                                 decreasing = FALSE), ]

bot10_Rating <- mean_sorted[1:10,]
bot10_Rating

ggplot(bot10_Rating, aes(x=Company, y=prom_pon)) + 
  geom_bar(stat = "identity", color="black", fill="#F5C37C") + coord_cartesian(ylim=c(0.0002,0.002))




##################33
colnames(top_box_choc)[1] = "Company"
top_box_choc <- df %>%
  filter(`Company ` %in% top10_Rating$Company)
top_box_choc

company_vs_bean = data.frame("Company" = (top_box_choc)[1], "Bean_Type" = top_box_choc$Bean_Type)
company_vs_bean 

see_company_bean = company_vs_bean %>% group_by(company_vs_bean[1]) %>%
  count(company_vs_bean[2])
see_company_bean

ggplot(data=see_company_bean, aes(x=Company., y=n, fill=Bean_Type)) + 
  geom_bar(stat="identity")




##
######
##
mean_rating_origin = aggregate(df$Rating, list(df$Broad_Bean_Origin), FUN=mean)
colnames(mean_rating_origin)[1] = "Broad_Bean_Origin"
bean_rating_origin = BASE_DATOS_CHOCOLATE_LIMPIA %>%
  count(Broad_Bean_Origin)
bean_rating_origin
colnames(bean_rating_origin)[1] = "Broad_Bean_Origin"

bean_pounde = merge(mean_rating_origin,bean_rating_origin)
bean_pounde

prom_ponde = c(bean_pounde$x * bean_pounde$n / length(df$`Company `))
prom_ponde

bean_pounde$prom_pon <- prom_ponde
bean_pounde

mean_sorted <- bean_pounde[order(bean_pounde$prom_pon,
                                 decreasing = TRUE), ]

top10_Rating <- mean_sorted[1:10,]
top10_Rating

ggplot(top10_Rating, aes(x=Company, y=prom_pon)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim=c(0.030,0.1))



##########
colnames(top_box_choc)[1] = "Company"
top_box_choc <- df %>%
  filter(`Company ` %in% top10_Rating$Company)
top_box_choc

company_vs_origin = data.frame("Company" = (top_box_choc)[1], "Broad_Bean_Origin" = top_box_choc$Broad_Bean_Origin)
company_vs_origin 

see_company_origin = company_vs_origin %>% group_by(company_vs_origin[1]) %>%
  count(company_vs_origin[2])
see_company_origin

ggplot(data=see_company_origin, aes(x=Company., y=n, fill=Broad_Bean_Origin)) + 
  geom_bar(stat="identity")


