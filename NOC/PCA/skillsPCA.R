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
library(doParallel)
library(lindia)
library(broom)
setwd("NOC/PCA")

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

onet.pca.model <- lm(VALUE ~ (PC1 + PC2 + PC3 + PC4 + PC5)^2 + GEO + REF_DATE, onet.pca.r) #Residuals explode at high values, but other diagnostics look good
onet.pca.model <- lm(VALUE ~ (PC1 + PC2 + PC3 + PC4 + PC5)^2 + I(PC1^.5) + I(PC2^.5) + I(PC3^.5) + I(PC4^.5) + I(PC5^.5) + GEO + REF_DATE, onet.pca.r) #Reins in some of the residuals for high fitted values
onet.pca.model <- lm(VALUE ~ (PC1 + PC2 + PC3 + PC4 + PC5)^2 + I(PC1^.5) + I(PC3^.5) + I(PC4^.5) + GEO + REF_DATE, onet.pca.r) #Remove some insignificant variables, but reintroduces some heteroskedasticity
summary(onet.pca.model)
glance(onet.pca.model)
tidy(onet.pca.model)

#Diagnostics: https://www.r-bloggers.com/regression-analysis-essentials-for-machine-learning/
par(mfrow = c(2,2))
plot(onet.pca.model)

gg_reshist(onet.pca.model) #looks about normal
gg_resfitted(onet.pca.model) #looks about flat, but high residuals at high-end. May need to log the PCAs
gg_qqplot(onet.pca.model) #same issue as above
gg_boxcox(onet.pca.model)
gg_scalelocation(onet.pca.model)
gg_resleverage(onet.pca.model)
gg_cooksd(onet.pca.model)
gg_diagnose(onet.pca.model)

#Caret https://topepo.github.io/caret/recursive-feature-elimination.html
set.seed(10)
cl <- makeCluster(detectCores()-4)
registerDoParallel(cl)

control <- trainControl(method="cv", number=3) #faster cv for interim models
split.m <- createDataPartition(onet.pca.r$VALUE, p = 0.7, list=FALSE)
train.m <- onet.pca.r[split.m,]
test.m <- onet.pca.r[-split.m,]

date()
train.rpart <- train(VALUE ~ PC1 + PC2 + PC3 + PC4 + PC5 + GEO + REF_DATE, data=train.m, method="rf", trControl=control, tuneLength=5)
print(train.rpart)
plot(train.rpart)
date()

date()
train.lm <- train(VALUE ~ (PC1 + PC2 + PC3 + PC4 + PC5)^2 + GEO + REF_DATE, data=train.m, method="lm", trControl=control)
print(train.lm)
plot(train.lm)
date()


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
d_tsne_1 = cbind(d_tsne_1, rownames(onet.s.f))

## plotting the results without clustering
ggplot(d_tsne_1, aes(x=V1, y=V2, label = rownames(onet.s.f))) +  
  geom_point() +
  brookfield.base.theme() +
  geom_text(check_overlap = TRUE, size=4, angle = 45)

## Factor analysis
#https://data.library.virginia.edu/getting-started-with-factor-analysis/

library(psych)
fa <- factanal(covmat = cov(onet.s.f), factors = 10, n.obs = 109, rotation = "varimax")
fa

fa.pred <- fa$loadings
