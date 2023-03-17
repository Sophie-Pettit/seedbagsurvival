library(ggplot2)
library(tidyverse)

#try to use the data with just the forbs to clear out noise 
#only uninoculated b/c that's what we find in nature
#random effect for block 

traits <- read.csv("~/Desktop/Pettit_seed-survival/Seed-Traits_cleaning.csv")

surv <- read.csv("~/Desktop/Pettit_seed-survival/Seed-bag-survival - Sheet1.csv")

surv <- merge(surv, traits, by = "Species", all.y = F, all.x = T)

#removing Notes and X columns
surv <- subset(surv, select = -c(Notes, X.1))
surv <- surv[-c(86,87,88,89,90),]

#lolium = Festuca perennis, lotus = Acmispon americanus, Taeniatherum = Elymus caput-medusae, stipa is gone 

ggplot(surv, aes(x = Species, y = n.viable)) + 
  geom_boxplot()
  theme_bw()

ggplot(surv, aes(x = wing.loading, y = n.viable)) +
  geom_point(aes(color = code)) + 
  geom_smooth(method="lm")
#not sure what wing.loading is, gotta ask marina on friday 
  
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
#idk why I'm doing these, i'm just messing around, seein new ways to manupulate the data 

ggplot(surv.3, aes(x = Species, y = n.viable)) + 
  geom_boxplot(aes(color = morphologicalunit))







