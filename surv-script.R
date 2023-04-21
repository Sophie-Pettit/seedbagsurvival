#load libraries####
library(ggplot2)
library(tidyverse)
library(ggfortify)
library(emmeans)
rm(list=ls())
#try to use the data with just the forbs to clear out noise 
#only uninoculated b/c that's what we find in nature
#random effect for block 
#load datasets####
traits <- read.csv("~/Desktop/seedbagsurvival/20230328_Seed-Traits_cleaning.csv")
surv <- read.csv("~/Desktop/seedbagsurvival/Seed-bag-survival - Sheet1.csv")
surv <- merge(surv, traits, by = "Species", all.y = F, all.x = T)

#removing Notes and X columns
surv <- subset(surv, select = -c(Notes, X.1))
surv <- filter(surv, Species!="Stipa pulchra", X!="inoculated")


#lolium = Festuca perennis, lotus = Acmispon americanus, Taeniatherum = Elymus caput-medusae, stipa is gone 
#surv plots####
ggplot(surv, aes(x = Species, y = n.viable)) + 
  geom_boxplot()
  theme_bw()
str(surv)

ggplot(surv, aes(x = wing.loading.m, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#slight negative
  
ggplot(surv, aes(x = coat.perm, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#giltri, lepnit, plaere above 1000
#trying to make a graph without these outliers to see if there are any patterns more visible 
coat.perm.under1000 <- surv[-c(56:65,81:85),]
ggplot(coat.perm.under1000, aes(x = as.factor(coat.perm), y = n.viable)) +
  geom_jitter(aes(color = code)) + 
  geom_smooth(method="lm")
#okay, showing maybe more of a negative correlation, i'm intrigued cause thats not what's expected
#as.factor spreads out the data 

ggplot(surv, aes(x = coat.thick, y = n.viable)) +
  geom_point(aes(color = code)) +
  geom_smooth(method="lm")
#line shows some negative correlation 

ggplot(surv, aes(x = mass.mg, y = n.viable)) +
  geom_point(aes(color = code)) +
  geom_smooth(method="lm")
#smaller seeds have more survival, as expected but not as extreme as expected ig

ggplot(surv, aes(x = height.cm, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#negative correlation, makes sense with size / shape as well

ggplot(surv, aes(x = shape, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#woah that was kinda cool, i'm still not sure what "lm" does tho lol
#a bit of a negative correlation shown, expected that the viability goes down as size goes up 

ggplot(surv, aes(x = size, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#negative correlation, expected 
#so exciting that having all the data is actually giving some results now!!
size.viable = lm(size ~ n.viable, data = surv)
size.viable.anova = anova(size.viable)
#Analysis of Variance Table
#Response: size
#Df  Sum Sq Mean Sq F value    Pr(>F)
#n.viable    1  44.419  44.419  18.959 3.048e-05
#Residuals 108 253.030   2.343                  
#n.viable  ***
#  Residuals    
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#okay so i made this anova table and then realized that I don't think it's helpful haha

#fixing column headings in seeds_all.accessions
colnames(Seeds_All.Accessions) <- Seeds_All.Accessions[1,]
Seeds_All.Accessions <- Seeds_All.Accessions[-1, ] 

surv.2 <- merge(surv, Seeds_All.Accessions, by = "Species", all.y = F, all.x = T)
surv.3 <- merge(Seed.bag.survival...Sheet1, Seeds_All.Accessions, by = "Species", all.y = F, all.x = T)
surv.3 <- surv.3[-c(216,217,218,219,220),]

nat.inv.viable = lm(n.viable ~ nat.inv, data = surv.3)
nat.inv.viable.anova = anova(nat.inv.viable)
nat.inv.viable.anova
#Analysis of Variance Table
#Response: n.viable
#           Df  Sum Sq  Mean Sq  F value Pr(>F)
#nat.inv     1    706   705.70   1.0044  0.3171
#Residuals  283  198839 702.61  

#so the p.value tells us that they are not statistically different at least only in basic difference 

ggplot(surv.3, aes(x = Species, y = n.viable)) + 
  geom_boxplot(aes(color = nat.inv))

ggplot(surv.3, aes(x = Species, y = n.viable)) + 
  geom_boxplot(aes(color = group))
#try taking out the bigger box 
#also try with the forbs only 

group.viable = lm(n.viable ~ group, data = surv.3)
group.viable.anova = anova(group.viable)
group.viable.anova
#Analysis of Variance Table
#Response: n.viable
#Df Sum Sq Mean Sq F value    Pr(>F)    
#group       1  31871   31871  53.792 2.357e-12 ***
#  Residuals 283 167674     592                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#p.value means we accept it by a lot. only a few grasses tho, so it makes sense. grasses had little to no survival 
#shows that forbs survive well, but moreso shows that grasses don't survive 

ggplot(surv, aes(x = coat.thick/mass.mg, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#positive correlation

ggplot(surv, aes(x = coat.thick/size, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#positive correlation
#kinda intrigued by this, I think this might be a more accurate way to actually see how much coat.thick affects the survival? I could be off my rocker tho 
#is coat thickness just not varied enough in our data 

ggplot(surv, aes(x = shape/size, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#literally a straight line, givin me nothin
#idk why I'm doing these, i'm just messing around, seein new ways to manipulate the data 

ggplot(surv.3, aes(x = Species, y = n.viable)) + 
  geom_boxplot(aes(color = morphologicalunit))

#surv.forbs####

surv.forbs <- filter(surv, group!="grass")

ggplot(surv.forbs[surv.forbs$wing.loading.m<7,], aes(x = wing.loading.m, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#negative relationship with and without outliar 

ggplot(surv.forbs, aes(x = log(coat.perm.perc), y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm") + 
  geom_text(aes(label=code))
#do we like vicvil? we doubt what the morph is what we thought it was, and not adapted to california at the very least. it has no germ or survival, soooo maybe it wasn't meant to be. from the northeast maning it was grown in different conditions and can't be necesarily compared to the other species in this dataset 


ggplot(surv.forbs, aes(x = log(coat.thick), y = n.viable)) +
  geom_point(aes(color = code)) +
  geom_smooth(method="lm")
#asters we don't have general thickness yet, fruit coat and seed coat getting smixed up (think like sunflower seeds) 

ggplot(surv.forbs, aes(x = morph.mass.mg, y = n.viable)) +
  geom_point(aes(color = code)) +
  geom_smooth(method="lm")

ggplot(surv.forbs, aes(x = log(chem.mass.mg), y = n.viable)) +
  geom_point(aes(color = code)) +
  geom_smooth(method="lm")
#fully horizontal 

ggplot(surv.forbs, aes(x = height.cm, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#negative relationship
#more indirect than direct relationships? 
#taller seeds can move further 

ggplot(surv.forbs, aes(x = shape.m, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#positive relationship not what we expected 

ggplot(surv.forbs, aes(x = shape.c, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#positive, but less relationship

ggplot(surv.forbs, aes(x = size.mm.m, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#big positive

ggplot(surv.forbs, aes(x = size.mm.c, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#not much

ggplot(surv.forbs, aes(x = coat.thick/chem.mass.mg, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm") + 
  geom_text(aes(label=code))
#miccal, coat is sepatarted from chemical unit
#negative 

ggplot(surv.forbs, aes(x = coat.thick/morph.mass.mg, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm") + 
  geom_text(aes(label=code))
#positive
#different species focus on different traits for survival

ggplot(surv, aes(x = coat.thick/size, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")

#goals: focus on the structure of the talk, pictures are good, focus on intro

#pca####
#principle components analysis 
columns <- c("morph.mass.mglog" , "set.time.mpsec" , "height.cm" , "shape" , "size.mm", "prop.C"  ,  "prop.Nlog"  ,   "wing.loadinglog" ,  "coat.perm.perclog" , "E.S"  , "coat.thick.per.sizelog"  )
#coluns we want: morph.mass.mg, 
# "morph.mass.mg" , "set.time.mpsec" , "height.cm" , "shape" , "size.mm", "area.mm2"  "prop.C"  ,  "prop.N"  , "cn" ,   "wing.loading" ,  "coat.perm.perc" , "E.S" , "both.thick"  ,  "mucilage"   
traits.forbs <- filter(traits, Species%in%surv.forbs$Species)
pca.forbs <-prcomp(traits.forbs[,colnames(traits.forbs)%in%columns],scale=TRUE)
summary(pca.forbs)
biplot(pca.forbs)
traits.forbs.novicia <- filter(traits.forbs, Species!="Vicia villosa")
pca.forbs.novicia <-prcomp(traits.forbs.novicia[,colnames(traits.forbs.novicia)%in%columns],scale=TRUE)
summary(pca.forbs.novicia)
biplot(pca.forbs.novicia)

#how to extract the pca variables to use in graphs and analysis 

str(pca.forbs.novicia)
pca.forbs.novicia$x
traits.forbs.novicia <- cbind(traits.forbs.novicia,pca.forbs.novicia$x[,1:4]) 
#traits.forbs.novicia <- traits.forbs.novicia[,-c(31:34)]
traits.forbs.novicia <- merge(traits.forbs.novicia, surv, by = "Species", all.y = F, all.x = T)
#merge(surv, traits, by = "Species", all.y = F, all.x = T)

#graph to heart's desire 
#no more traits than species, maybe get rid of some more traits or look at them 

#transforming data####

hist((traits.forbs.novicia$wing.loading))
#right skewed 
hist(log(traits.forbs.novicia$wing.loading))
traits.forbs.novicia$wing.loadinglog <- log(traits.forbs.novicia$wing.loading)

hist((traits.forbs$cn))
#right skewed
hist(log(traits.forbs$cn))
#now a little left skewed, but way more normal 
traits.forbs.novicia$cnlog <- log(traits.forbs.novicia$cn)

hist(traits.forbs$prop.C)
#left skewed
hist(log(traits.forbs$prop.C))
#okay the log super didn't help, not sure why
#i have discovered why, let's see if this works 
hist(asin(sqrt(traits.forbs$prop.C)))
#once again worse, Idk how to fix this 
#keep as is, no transformation 

hist((traits.forbs$prop.N))
hist(log(traits.forbs$prop.N))
#normal with log
traits.forbs.novicia$prop.Nlog <- log(traits.forbs.novicia$prop.N)

hist((traits.forbs$coat.perm.perc))
hist(log(traits.forbs$coat.perm.perc))
traits.forbs.novicia$coat.perm.perclog <- log(traits.forbs.novicia$coat.perm.perc)
#not super normal with the log either 
#go with the log 


hist(log(traits.forbs$both.thick/traits.forbs$size.mm))
traits.forbs.novicia$coat.thick.per.sizelog <- log(traits.forbs.novicia$both.thick/traits.forbs.novicia$size.mm)
#normal with log 

hist(log(traits.forbs$both.thick/traits.forbs$chem.mass.mg))
traits.forbs.novicia$coat.thick.per.mass <- log(traits.forbs.novicia$both.thick/traits.forbs.novicia$chem.mass.mg)
#normal with log

hist((traits.forbs$morph.mass.mg))
hist(log(traits.forbs$morph.mass.mg))
traits.forbs.novicia$morph.mass.mglog <- log(traits.forbs.novicia$morph.mass.mg)
#use log

hist(log(traits.forbs$chem.mass.mg))
hist(sqrt(traits.forbs$chem.mass.mg))
traits.forbs.novicia$chem.mass.mglog <- log(traits.forbs.novicia$chem.mass.mg)
#use log

hist((traits.forbs$size.mm))
hist(log(traits.forbs$size.mm))
traits.forbs$size.mmlog <- log(traits.forbs$size.mm)
#still not really normal 
#do not use log

hist(traits.forbs$set.time.mpsec)

hist((traits.forbs$E.S))
hist(log(traits.forbs$E.S))
traits.forbs$E.Slog <- log(traits.forbs$E.S)
#way not normal with log
hist(asin(sqrt(traits.forbs$E.S)))
#keep as is


#graphs with pca####

ggplot(traits.forbs.novicia, aes(x = PC1, y = n.viable)) +
  geom_point(aes(color = code)) +
  facet_wrap(~nat.inv)

ggplot(traits.forbs.novicia, aes(x = PC2, y = n.viable)) +
  geom_point(aes(color = code)) +
  facet_wrap(~nat.inv)

ggplot(traits.forbs.novicia, aes(x = PC3, y = n.viable)) +
  geom_point(aes(color = code))

ggplot(traits.forbs.novicia, aes(x = PC1, y = n.viable)) +
  geom_boxplot(aes(color = code)) +
  facet_wrap(~nat.inv) 


#presentation figures#### 
surv$fun.group <- paste(surv$nat.inv,surv$group,sep=" ")
ggplot(surv, aes(x = fun.group, y = n.viable)) + 
  geom_boxplot() +
  theme_bw() 

fun.group.m = lm(n.viable ~ fun.group, data = surv)
summary(fun.group.m)
pairs(emmeans(fun.group.m, ~ fun.group), adjust = "BH")
#coat.perm.viable = lm(size ~ n.viable, data = traits.forbs.novicia)
#goal: Make models for other graphs. if continuous, might not need emmeans. look at linear models in google drive. 

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}
surv.forb.sum <- traits.forbs.novicia %>%
  group_by(Species) %>%
  summarize(mean.viable = mean(n.viable), se.viable = calcSE(n.viable))

traits.forbs <- merge(traits.forbs, surv.forb.sum, by = "Species", all.y=T, all.x = F)  

traits.forbs$fun.group <- paste(traits.forbs$nat.inv,traits.forbs$group,sep=" ")
ggplot(traits.forbs, aes(x=shape, y=mean.viable, group = fun.group,col=fun.group )) +
  geom_point()+
  geom_errorbar(aes(ymin=mean.viable-se.viable, ymax = mean.viable+se.viable)) +
  geom_smooth(method = "lm") 

traits.forbs <- merge(traits.forbs, surv, by = "Species", all.y = T, all.x = F)

shape.m = lm(sqrt(n.viable) ~ shape, data = traits.forbs)
summary(shape.m)
#pairs(emmeans(fun.group.m, ~ fun.group), adjust = "BH")

ggplot(traits.forbs, aes(x=size.mm, y=mean.viable, group = fun.group,col=fun.group )) +
  geom_point()+
  geom_errorbar(aes(ymin=mean.viable-se.viable, ymax = mean.viable+se.viable)) +
  geom_smooth(method = "lm") 
#not much, unlike expected 

size.m = lm(sqrt(n.viable) ~ size.mm, data = traits.forbs)
summary(size.m)
#still not significant with the sqrt 

ggplot(traits.forbs, aes(x=log(both.thick/size.mm), y=mean.viable)) +
  geom_point()+
  geom_errorbar(aes(ymin=mean.viable-se.viable, ymax = mean.viable+se.viable)) +
  geom_smooth(method = "lm") 
#slight but significant, driven by natives, not enough invasive to confirm that 

traits.forbs$both.thick.size.mm.log <- log(traits.forbs$both.thick/traits.forbs$size.mm)
#traits.forbs.novicia$coat.perm.perclog <- log(traits.forbs.novicia$coat.perm.perc)

thick.size.m = lm(sqrt(n.viable) ~ both.thick.size.mm.log, data = traits.forbs)
summary(thick.size.m)
#sig with sqrt(n.viable) 

ggplot(traits.forbs, aes(x=log(coat.perm.perc), y=mean.viable,group = fun.group,col=fun.group)) +
  geom_point()+
  geom_errorbar(aes(ymin=mean.viable-se.viable, ymax = mean.viable+se.viable)) +
  geom_smooth(method = "lm") 
#driving it, once again not deeply studied trait 

coat.perm.perc.m = lm(sqrt(n.viable) ~ coat.perm.perc, data = traits.forbs)
summary(coat.perm.perc.m)

coat.perm.viable = lm(size.mm ~ n.viable, data = traits.forbs.novicia)


