#PCA Skills
#https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
  
library(tidyr)
library(tidyverse)
library(devtools)
library(ggbiplot)
library(caret)

#Set up data
onet.s <- full.avg.crosswalk.skill

onet.s$element<- paste(substr(onet.s$element.id, 1, 5), onet.s$element.name)

onet.s.f <- onet.s %>%
  select(noc_title, element, scale.name, V1) %>%
  spread(scale.name, V1) %>%
  mutate(score = Importance * Level) %>% # Multiplied importance by level
  select(noc_title, element, score) %>%
  spread(element, score)

# onet.s.f <- onet.s %>%
#   filter(scale.name == "Level") %>%
#   select(noc_title, element, V1) %>%
#   spread(element, V1) #%>%
#   #select(-noc_title)

#Create PCA
onet.s.pca <- prcomp(onet.s.f[,-1], center=TRUE, scale.=TRUE)

#Analyze PCA
print(onet.s.pca)
plot(onet.s.pca, type = "l")
summary(onet.s.pca)


#Biplot
## Change group to include our tech and digital occupations
g <- ggbiplot(onet.s.pca, obs.scale = 1, var.scale = 1, 
              #groups = element, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#Caret PCA
trans = preProcess(onet.s.f[,-1], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, onet.s.f[,-1])

head(PC, 3)
trans$rotation

write.csv(trans$rotation, "pcaloads.csv")
