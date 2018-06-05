############# PCA Skills
#https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
  
library(tidyr)
library(tidyverse)
library(devtools)
library(ggbiplot)
library(caret)
library(Rtsne)
library(BFTheme)
library(skimr)

#Set up data
onet.s <- full.avg.crosswalk.skill

onet.s$element<- paste(substr(onet.s$element.id, 1, 5), onet.s$element.name)

onet.s.f <- onet.s %>%
  select(noc_title, element, scale.name, V1) %>%
  filter(!is.na(noc_title)) %>%
  spread(scale.name, V1) %>%
  mutate(score = Importance * Level) %>% # Multiplied importance by level
  select(noc_title, element, score) %>%
  spread(element, score) %>%
  remove_rownames %>% 
  column_to_rownames(var="noc_title")


#Create PCA
onet.s.pca <- prcomp(onet.s.f, center=TRUE, scale.=TRUE, rank. = 5)
#onet.s.pca <- prcomp(onet.s.f[,-1], center=TRUE, scale.=TRUE, rank. = 5)

#Analyze PCA
onet.s.pca$rotation
write.csv(onet.s.pca$rotation, "pcaloads.csv")
plot(onet.s.pca, type = "l")
summary(onet.s.pca)

onet.pca <- predict(onet.s.pca, newdata = onet.s.f)

#Biplot
## Change group to include our tech and digital occupations
ggbiplot(onet.s.pca, obs.scale = 1, var.scale = 1,
         #groups = element,
         ellipse = TRUE, 
         circle = TRUE) +
  brookfield.base.theme()

# #Caret PCA
# trans = preProcess(onet.s.f, 
#                    method=c("BoxCox", "center", 
#                             "scale", "pca"))
# PC = predict(trans, onet.s.f)
# 
# head(PC, 3)
# trans$rotation
# 
# onet.pca <- predict(trans, newdata = onet.s.f)
# 
# write.csv(trans$rotation, "pcaloads.csv")

############# t-SNE and cluster
#https://www.r-bloggers.com/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/

tsne_model_1 = Rtsne(as.matrix(onet.s.f[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
d_tsne_1 = as.data.frame(tsne_model_1$Y) 

## plotting the results without clustering
ggplot(d_tsne_1, aes(x=V1, y=V2)) +  
  geom_point() +
  brookfield.base.theme()
