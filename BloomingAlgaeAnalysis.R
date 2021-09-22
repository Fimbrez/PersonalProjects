algae <- read_table2("algaeBloom.txt", col_names=
                       c('season','size','speed','mxPH','mnO2','Cl','NO3','NH4',
                         'oPO4','PO4','Chla','a1','a2','a3','a4','a5','a6','a7'),
                     na="XXXXXXX")
glimpse(algae)

algae %>%
  group_by(season) %>%
  summarise(n())


chems <- select(algae,-c(a1,a2,a3,a4,a5,a6,a7,size,speed,season))
colMeans(chems, na.rm = TRUE)

chems %>%
  summarise_if(is.numeric, var, na.rm = TRUE)

chems %>%
  summarise_if(is.numeric, mad, na.rm = TRUE)

algae %>%  
  select(c('mxPH', 'mnO2', 'Cl', 'NO3', 'NH4', 'oPO4', 'Chla')) %>%
  summarize_if(is.numeric, median, na.rm = TRUE)

?ggplot
ggplot(algae, aes(x = algae$a1, y = algae$size)) + geom_boxplot() + ggtitle("A conditioned Boxplot of Algae a1") + xlab("a1")
ggplot(algae, aes(x = algae$NO3, y = algae$size)) + geom_boxplot() + ggtitle("A conditioned Boxplot of Algae NO3") + xlab("N03")
ggplot(algae, aes(x = algae$NH4, y = algae$size)) + geom_boxplot() + ggtitle("A conditioned Boxplot of Algae NH4") + xlab("NH4")

#### the median and mad seem to be more robust compared to mean and variance with the presence outliers


sapply(algae, function(x) sum(length(which(is.na(x)))))
sum(!complete.cases(algae))  
#Number of observations with missing values is 16
#8 variables with missing values

?filter
algae.del <- algae %>% filter(complete.cases(algae))
algae.del

?mutate_at
?ifelse
algae.med <-algae %>% mutate_at(vars(mxPH,mnO2,Chla,Cl,NO3,NH4,oPO4,PO4), ~ifelse(is.na(.), median(., na.rm = TRUE), .))
algae.med[c(48,62,199),]
summary(lm(PO4~oPO4, data = algae))
predict(lm())



chemicals  <- algae %>% select(mxPH, mnO2, Chla, Cl, NO3, NH4, oPO4, PO4)
cor(chemicals)
?cor




