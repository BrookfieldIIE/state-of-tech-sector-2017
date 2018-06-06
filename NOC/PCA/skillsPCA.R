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
setwd("~/GitHub/state-of-tech-sector-2017/NOC/PCA")

#Set up data
full.avg.crosswalk.skill <- readRDS("full.avg.crosswalk.skill.Rds")
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

onet.pca <- as.data.frame(predict(onet.s.pca, newdata = onet.s.f))

#Biplot
## Change group to include our tech and digital occupations
ggbiplot(onet.s.pca, obs.scale = 1, var.scale = 1,
         #groups = element,
         ellipse = TRUE, 
         circle = TRUE) +
  brookfield.base.theme()


#Regress on COPS and NOC earnings
#Census earnings
earnings <- read_csv("14100356.csv")
earnings <- earnings %>%
  filter(Statistics == "Average offered hourly wage", is.na(TERMINATED), !is.na(VALUE)) %>%
  select(-DGUID, -Statistics, -UOM, -UOM_ID, -SCALAR_FACTOR, -SCALAR_ID, -VECTOR, -COORDINATE, -SYMBOL, -DECIMALS, -STATUS, -TERMINATED, NOC = 'National Occupational Classification')

onet.pca.r <- onet.pca %>%
  rownames_to_column(var="NOC") %>%
  mutate(NOC.code = substr(NOC, 1,4), NOC = substr(NOC, 6, nchar(NOC))) %>%
  left_join(earnings) %>% 
  drop_na() %>%
  select(-NOC.code)

onet.pca.model <- lm(VALUE ~ (PC1 + PC2 + PC3 + PC4 + PC5 + NOC)^2 , onet.pca.r)
summary(onet.pca.model)

#COPS
cops <- read_csv("employment_growth_croissance_emploi_2017_2026.csv")
cops <- cops %>%
  filter(row_number() >= 17) %>%
  filter(Change != "#DIV/0!") %>%
  mutate(NOC.code = substr(Code, 2,5)) %>%
  select(Occupation_Name, NOC.code, Change)

onet.pca <- onet.pca %>%
  rownames_to_column(var="NOC") %>%
  mutate(NOC.code = substr(NOC, 1,4)) %>%
  left_join(cops) %>% 
  drop_na() %>%
  select(-NOC)

onet.pca.model <- lm(Change ~ (PC1 + PC2 + PC3 + PC4 + PC5) , onet.pca)
summary(onet.pca.model)

############# t-SNE and cluster
#https://www.r-bloggers.com/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/

tsne_model_1 = Rtsne(as.matrix(onet.s.f[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
d_tsne_1 = as.data.frame(tsne_model_1$Y) 

## plotting the results without clustering
ggplot(d_tsne_1, aes(x=V1, y=V2)) +  
  geom_point() +
  brookfield.base.theme()
