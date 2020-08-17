##Set working directory
setwd("C:/Users/user/Desktop/HCVData")
library(tidyverse)
library(plotly)
library(gridExtra)
library(caret)
library(caretEnsemble)
library(doParallel)
library(ROSE)
library(DMwR)
## Read the data into R
hcv <- read.csv("hcvdat0.csv", stringsAsFactors = FALSE)

## Check the structure of the data
glimpse(hcv)

##Check the dimensions of the data
dim(hcv) ##It shows that its a data with 615 rows and 14 columns
head(hcv, 5)
tail(hcv, 5)

summary(hcv)
##First create a function to calculate the percentage of missing values per column
percent_missing <- function(x){
  sum(is.na(x))/length(x) * 100
}
##apply the function to every column of the data
apply(hcv,2, percent_missing)
## The percentage of missing values per column is a tiny fraction. we can afford to drop them

hcv <- na.omit(hcv) ##A total of 26 rows dropped

##Find the unique numbers per column
lapply(hcv, function(x) length(unique(x)))

##Then we tidy the "Category" and 'Age' variable 
hcv$Category[hcv$Category == "0=Blood Donor"] <- "BD"
hcv$Category[hcv$Category == "0s=suspect Blood Donor"] <- "SBD"
hcv$Category[hcv$Category == "1=Hepatitis"] <- "HP"
hcv$Category[hcv$Category == "2=Fibrosis"] <- 'FIB'
hcv$Category[hcv$Category == "3=Cirrhosis"] <- "CIR"

hcv$Sex[hcv$Sex == 'f'] <- "F"
hcv$Sex[hcv$Sex == 'm'] <- "M"
##EXPLORATORY DATA ANALYSIS

## Examine the categorical variables
table(hcv$Category)
table(hcv$Sex)

##Make pie charts of categorical variables

##first create a frequency table
tab <- data.frame(table(hcv$Category))
colnames(tab) <- c('Cat', "Count")
tab$Prop <- round(tab$Count/sum(tab$Count), 3)

##Then plot the pie chart using the Plotly package
## First create a vector to carry the colours
colors <- c('rgb(211, 94, 69)', 'rgb(128, 133, 133)', 'rgb(144, 103, 167)',
            'rgb(171, 104, 87)', 'rgb(114, 147, 203)')

py <- plot_ly(tab, labels = ~Cat, values = ~Count, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent', insidetextinfo = list(color = '#FFFFFF'),
              hoverinfo = ~Count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE)

py <- py %>% layout(title = "Category Sizes",
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py
##Overwhelming majority of the Category are Blood Donors(86.7%), while Suspected Blood Donors are the list
##group(1.14%)

##A customised bar plot using ggplot2
bar <- ggplot(tab, aes(Cat, Count, fill = Cat)) + geom_bar(stat = "identity") +
  ggtitle(paste("Category Sizes")) + coord_flip() + 
  geom_label(aes(label = paste(Prop * 100, "%"), y = -50, fill = Cat), show.legend = FALSE,
             size = 5, label.padding = unit(0.1, "lines")) +
  expand_limits(y = -50) + scale_fill_brewer(palette = "Set2", direction = -1) + 
  labs(y = "Frequency", x = "Category")

bar + theme_classic()


##Sex
tab1 <-data.frame(table(hcv$Sex))
colnames(tab1) <- c('Sex', "Count")
tab1$Prop <- round(tab1$Count/sum(tab1$Count), 3)

##Plot the pie chart using Plotly
col1 <- c('rgb(202, 167, 96)', 'rgb(190, 208, 36)')

py1 <- plot_ly(tab1, labels = ~Sex, values = ~Count, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              indextextfont = list(color = '#FFFFFF'), hoverinfo = ~Count,
              marker = list(colors = col1, line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE)

py1 <- py1 %>% layout(title = "Sex",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

py1
##The pie plot shows that 61.3% of respondents are Male, while 38.7% are Female

###Check the distribution of the various quantitative variables using ggplot2
dens <- ggplot(hcv, aes(Age))  + geom_density(aes(fill = Sex), alpha = 0.3) +
  labs(x = "Age", y = "", title = "KDE of Age by Sex")

##Alternatively we can plot the log transformed variable
dens1 <- ggplot(hcv, aes(log(Age)))  + geom_density(aes(fill = Sex), alpha = 0.3) +
  labs(x = "Log of Age", y = "", title = "KDE of Log of Age by Sex")

## We can plot a Histogram to for the tab1 table 
hist <- ggplot(hcv, aes(Age))  + geom_histogram(aes(fill = Sex), bins = 30) +
  labs(x = "Log of Age", y = "", title = "Distribution of Age by Sex")

hist1 <- ggplot(hcv, aes(log(Age)))  + geom_histogram(aes(fill = Sex), bins = 30) +
  labs(x = "Log of Age", y = "", title = "Distribution of Log of Age by Sex")

grid.arrange(dens, hist, dens1, hist1)

## To find out the minimum, maximum, mean, median and standard deviation of Age by Sex
hcv %>% group_by(Sex) %>% summarise(MIN = min(Age), MAX = max(Age), MEAN = mean(Age),
                                    MED = median(Age), STD = sd(Age))

## If we want to know the previous age parameters but also as a function of Category and Gender
hcv %>% group_by(Category, Sex) %>% summarise(MIN = min(Age), MAX = max(Age), MEAN = mean(Age),
                                              MED = median(Age), STD = sd(Age))
## We can visualize the above using a boxplot
colors = c('blue', 'magenta')

fig1 <- plot_ly(hcv, x = ~Category, y = ~Age, color = ~Sex, colors = colors, type = 'box')
fig1 <- fig1 %>% layout(boxmode = 'group', title = 'Distribution of Age by Blood Category and Gender')

## Or using ggplot2 
hcv %>% ggplot(aes(Category, Age)) + geom_boxplot(aes(fill = Sex)) + 
  ggtitle('Distribution of Age Vs Blood Category by Gender') 
##From the above we see that the minimum, maximum, mean and median age of the Female group is,
## 32, 70, 47.6, and 48 respectively compared to 23, 77, 47.3, and 47, respectively for the men. 
## Also there is a slightly more variability in Age distribution of the male group than the female

## This variability can be seen with the aid of a boxplot
gbox <- hcv %>% ggplot(aes(Age, Sex)) + geom_boxplot(aes(fill = Sex), outlier.color = 'green',
                                                outlier.size = 2) + coord_flip() + labs(x = 'Age', y = 'Sex')


### MULTIVARIATE ANALYSIS
##First make a copy of the data frame
hcv1 <- data.table::copy(hcv)

## We might want to get an idea of the correlation matrix between numeric variables
hcvc <- hcv1[, c(3, 5: 14)]
hcvcorr <- cor(hcvc)

## A correlation plot
corrplot::corrplot(hcvcorr, type = 'upper', order = 'hclust', col = c('black', 'white'),
                   bg = 'lightblue')

## Age, Sex and ALB
cols = c('blue', 'magenta')
#cols = setNames(cols, c("F", "M"))
plt <- plot_ly(hcv1, x = ~Age, y = ~ALB, color = ~Sex, colors = cols,  mode = 'markers', size = 1)

plt <- plt %>% layout(title = "Albumin Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))

## Also we can add an additional variable(Category) to fully under the relation ship

colors1 = c('blue', 'magenta')

hcv1 %>% ggplot(aes(Age, ALB)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Albumin') +
  ggtitle('Albumin Levels by Age, Gender and Blood Category')

## Age, Sex and ALP
colors1 = c('blue', 'magenta')

plt1 <- plot_ly(hcv1, x = ~Age, y = ~ALP, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt1 <- plt1 %>% layout(title = "Alkaline Phosphatase Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))

hcv1 %>% ggplot(aes(Age, ALP)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Alkaline Phosphatase') +
  ggtitle('Alkaline Phosphatase Levels by Age, Gender and Blood Category')
## The ALP levels for those in BD Blood Category across gender then drops for other categories with a corresponding increase with Age  
##even though there seems to a spike in those with Cirrhosis

## Age, Sex and ALT

plt2 <- plot_ly(hcv1, x = ~Age, y = ~ALT, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt2 <- plt2 %>% layout(title = "Alanine Transaminase Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt2

## Faceted by Blood Category
hcv1 %>% ggplot(aes(Age, ALT)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Alanine Transaminase') +
  ggtitle('Alanine Transaminase Levels by Age, Gender and Blood Category')
## ALT levels follow same pattern of dropping off with Age. However, the drop is more pronounced in those with
## Cirrhosis while there is a mild increase in those who are considered Suspect Blood Donors

## Age, Sex and AST
plt3 <- plot_ly(hcv1, x = ~Age, y = ~AST, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt3 <- plt3 %>% layout(title = "Aspertate Aminotransferase Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt3
## Facet the plot by Blood Category
hcv1 %>% ggplot(aes(Age, AST)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Aspertate Aminotransferase') +
  ggtitle('Aspertate Aminotransferase Levels by Age, Gender and Blood Category')
## It can be seen that AST levels are fairly stable with advancing age across genders among the Blood Donor group
## However other Blood categories see a sharp increase with AST levels with advanced age

## Age, Sex and BIL
plt4 <- plot_ly(hcv1, x = ~Age, y = ~BIL, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt4 <- plt4 %>% layout(title = "Bilirubin Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt4

## Faceted by Blood Category
hcv1 %>% ggplot(aes(Age, BIL)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Bilirubin') +
  ggtitle('Bilirubin Levels by Age, Gender and Blood Category')
## In general there are higher BIL levels in Males than in Females even across blood categories. However we notice fairly
## stable BIL levels in the BD and SBD category, and much higher BIL levels in the CIR and HP blood categories across
## genders. This is consistent with studies that show higher BIL levels in Cirrhosis patients, and in some cases 
## Hepatitis patients too

## Age, Sex and CHE

plt5 <- plot_ly(hcv1, x = ~Age, y = ~CHE, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt5 <- plt5 %>% layout(title = "Cholinesterase Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt5

## Faceted by Blood Category
hcv1 %>% ggplot(aes(Age, CHE)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Cholinesterase') +
  ggtitle('Cholinesterase Levels by Age, Gender and Blood Category')
## This also follows same pattern of higher levels of CHE in men than in women across blood categories but we 
## see a substantial drop in levels in those with Cirrhosis

## Age, Sex and CHOL

plt6 <- plot_ly(hcv1, x = ~Age, y = ~CHOL, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt6 <- plt6 %>% layout(title = "Cholesterol Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt6

## Faceted  by Blood Category
hcv1 %>% ggplot(aes(Age, CHOL)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Cholesterol') +
  ggtitle('Cholesterol Levels by Age, Gender and Blood Category')


plt7 <- plot_ly(hcv1, x = ~Age, y = ~CREA, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt7 <- plt7 %>% layout(title = "Creatinine Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt7

## Faceted by Blood Category

hcv1 %>% ggplot(aes(Age, CREA)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'Creatinine') +
  ggtitle('Creatinine Levels by Age, Gender and Blood Category')

## There are generally higher CREA levels in men than in women across different categories, and spikes in levels is
## noticed in those with Cirrhosis

## Age, Sex and GGT

plt8 <- plot_ly(hcv1, x = ~Age, y = ~GGT, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt8 <- plt8 %>% layout(title = "GGT(Gamma-Glutanyl Transferase) Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt8

## Faceted by Blood Category

hcv1 %>% ggplot(aes(Age, GGT)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'GGT') +
  ggtitle('GGT(Gamma-Glutanyl Transferase) Levels by Age, Gender and Blood Category')
## We see that GGT levels are higher on average in Males than Females across categories. However we notice in some
## cases higher levels of GGT in CIR, FIB and HP patients

## Age, Sex and PROT

plt9 <- plot_ly(hcv1, x = ~Age, y = ~PROT, color = ~Sex, colors = colors1,  mode = 'markers', size = 1)

plt9 <- plt9 %>% layout(title = "PROT Levels by Age and Gender", yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))
plt9

## Faceted by Blood Category

hcv1 %>% ggplot(aes(Age, PROT)) + geom_point(aes(color = Sex), size = 2.5, alpha = 0.5) + 
  facet_wrap( ~ Category) + scale_color_manual(values = colors1) + labs(x = 'Age', y = 'PROT') +
  ggtitle('PROT Levels by Age, Gender and Blood Category')


## PREDICTION USING MACHINE LEARNING ALGORITHMS
## Reacll that using the table() on the target(Category) variable, we were able to determine that its a 
## multiclass imbalanced data and as such would be treated with a different approach, this involves using
## different sampling methods


## First feature engineering and Preprocessing
## Then drop the Index and Category columns

hcv1$BD <- as.factor(ifelse((hcv1$Category == 'BD' | hcv1$Category == 'SBD'), 'No', 'Yes'))
hcv1$Sex[hcv1$Sex == 'M'] <- 1
hcv1$Sex[hcv1$Sex == 'F'] <- 0
hcv1 <- hcv1[, -1:-2]

head(hcv1, 3) ## To confirm that the column was dropped indeed

set.seed(42)
idx <- createDataPartition(hcv1$BD, p = 0.8, list = FALSE)
train_data <- hcv1[idx,]
test_data <- hcv1[-idx,]

## Under Sampling
set.seed(42)
ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, verboseIter = FALSE,
                     sampling = 'down')

down_model <- train(BD ~ ., data = train_data, method = 'rf',
                    preProcess = c("scale", "center"), trControl = ctrl)

## Over Sampling
ctrl$sampling = 'up'
set.seed(42)

up_model <- train(BD ~ ., data = train_data, method = 'rf',
                    preProcess = c("scale", "center"), trControl = ctrl)


## ROSE
ctrl$sampling = 'rose'
set.seed(42)
rose_model <- train(BD ~ ., data = train_data, method = 'rf',
                  preProcess = c("scale", "center"), trControl = ctrl)

## SMOTE

ctrl$sampling = 'smote'
set.seed(42)
smote_model <- train(BD ~ ., data = train_data, method = 'rf',
                    preProcess = c("scale", "center"), trControl = ctrl)


## Make predictions using the various models
## Down Sampling model
pred_down <- data.frame(actual = test_data$BD, 
                        predict(down_model, newdata = test_data, type = 'prob'))
pred_down$predict <-ifelse(pred_down$Yes > 0.5, 'Yes', 'No')

downCM <- confusionMatrix(factor(pred_down$predict), test_data$BD)


## Up Sampling model
pred_up <- data.frame(actual = test_data$BD,
                      predict(up_model, newdata = test_data, type = 'prob'))
pred_up$predict <-ifelse(pred_up$Yes > 0.5, 'Yes', 'No')
UPCM <- confusionMatrix(factor(pred_up$predict), test_data$BD)


## ROSE Sampling
pred_rose <- data.frame(actual = test_data$BD,
                      predict(rose_model, newdata = test_data, type = 'prob'))
pred_rose$predict <-ifelse(pred_rose$Yes > 0.5, 'Yes', 'No')
roseCM <- confusionMatrix(factor(pred_rose$predict), test_data$BD)

## SMOTE Sampling
pred_smote <- data.frame(actual = test_data$BD,
                      predict(smote_model, newdata = test_data, type = 'prob'))
pred_smote$predict <-ifelse(pred_smote$Yes > 0.5, 'Yes', 'No')
smoteCM <- confusionMatrix(factor(pred_smote$predict), test_data$BD)

## Compare the performance of the different models

models <- list(DOWN = down_model, UP = up_model,
               ROSE = rose_model, SMOTE = smote_model)
resampling <- resamples(models)
bwplot(resampling)

## The accuracy, sensitivity, specificity and Kappa scores of the respective models are as follows:
## downCM: 0.9658, 0.9717, 0.9091, 0.8144
## UPCM: 0.9829, 1.00, 0.8182, 0.8908
## roseCM: 0.9658, 0.9811, 0.8182, 0.7993
## smoteCM: 1.00, 1.00, 1.00, 1.00

## On the basis of accuracy smote_model could be the best but there is a possibility of overfitting
## so instead we choose the model gotten from up sampling(up_model). But on the basis of Kappa scores
## the rose_model is the best model as it has the lowest score.

## Similary we can simultaneously train 2 different models using the caretList() function, and the caretEnsemble
## and doParallel packages, using cross validation, Logistic and Naive Bayes algorithms


registerDoParallel(3)
getDoParWorkers()
## First we use the downsampling method
set.seed(42)
ctrli <- trainControl(method = 'cv', number = 10, classProbs = TRUE,
                      savePredictions = 'final', index = createResample(train_data$BD, 3),
                      sampling = 'down', allowParallel = TRUE)

## Bulid the simultaneous models
modelList <- caretList(BD ~ ., data = train_data, methodList = c('glm', 'nb'), metric = 'kappa',
                       tuneList = NULL, continue_on_fail = FALSE, preProcess = c('center', 'scale'),
                       trControl = ctrli)
## Look at the models
modelList$glm
modelList$nb

## Test performance on test data

##Logistic
confusionMatrix(predict(modelList$glm, test_data, type = 'raw'), test_data$BD)

##Naive Bayes
confusionMatrix(predict(modelList$nb, test_data, type = 'raw'), test_data$BD)

## UpSampling
ctrli$sampling <- 'up'
set.seed(42)
modelListUP <- caretList(BD ~ ., data = train_data, methodList = c('glm', 'nb'), metric = 'kappa',
                       tuneList = NULL, continue_on_fail = FALSE, preProcess = c('center', 'scale'),
                       trControl = ctrli)

## Look at the models
modelListUP$glm
modelListUP$nb

#### Test performance on test data

##Logistic
confusionMatrix(predict(modelListUP$glm, test_data, type = 'raw'), test_data$BD)

##Naive Bayes
confusionMatrix(predict(modelListUP$nb, test_data, type = 'raw'), test_data$BD)

## ROSE Sampling
ctrli$sampling <- 'rose'
set.seed(42)
modelListROSE <- caretList(BD ~ ., data = train_data, methodList = c('glm', 'nb'), metric = 'kappa',
                         tuneList = NULL, continue_on_fail = FALSE, preProcess = c('center', 'scale'),
                         trControl = ctrli)

## Look at the models
modelListROSE$glm
modelListROSE$nb

#### Test performance on test data

##Logistic
confusionMatrix(predict(modelListROSE$glm, test_data, type = 'raw'), test_data$BD)

##Naive Bayes
confusionMatrix(predict(modelListROSE$nb, test_data, type = 'raw'), test_data$BD)


## SMOTE Sampling
ctrli$sampling <- 'smote'
set.seed(42)
modelListSMOTE <- caretList(BD ~ ., data = train_data, methodList = c('glm', 'nb'), metric = 'kappa',
                           tuneList = NULL, continue_on_fail = FALSE, preProcess = c('center', 'scale'),
                           trControl = ctrli)

## Look at the models
modelListSMOTE$glm
modelListSMOTE$nb

#### Test performance on test data

##Logistic
confusionMatrix(predict(modelListSMOTE$glm, test_data, type = 'raw'), test_data$BD)

##Naive Bayes
confusionMatrix(predict(modelListSMOTE$nb, test_data, type = 'raw'), test_data$BD)

## Each of the 8 models in 4groups produce different results with different degree of accuracy and 
## kappa scores and while there could be overfitting in the models, they are quite as accurate as
## could be considering that this is a rather smaller dataset