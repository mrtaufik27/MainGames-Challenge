# MAINGAMES CHALLENGE
library(writexl)
library(vtable)
library(ggplot2); library(ggcorrplot); library(corrgram) ;library(lares) # correlation
library(rpart); library(rpart.plot) # decision tree
library(caTools) # data splitting
library(MASS)
library(caret) # machine learning algorithm
library(leaps) # leaps for LM
library(gbm) # boosting
library(caret) ; library(ellipse)
library(tidyverse) ;library(tidytext)


setwd("C:/Users/USER/Google Drive/04. Lain-lain/maingame/DS_test")

df <- read.csv("Maingames_DS_dataset.csv")
str(df)

df$Country <- as.factor(df$Country)
df$Gender <- as.factor(df$Gender)
df$Game <- as.factor(df$Game)
df$Total.Follower <- as.numeric(df$Total.Follower)
df$MBTI_grouped <- as.factor(df$MBTI_grouped)


levels(df$Gender) <- c(NA, NA, "Female", "Male")
levels(df$Game)[c(1:2)] <- NA

str(df)
summary(df)

# Look at the data

tab <- sumtable(df,
         summ=c('notNA(x)',
                'mean(x)',
                'median(x)',
                'propNA(x)'),out="return")
tab
View(tab)
View(df)

str(df)
df <- df[,-1]
#write_xlsx(tab, "descriptive.xlsx")

names(df)

ggplot(df[is.na(df$Gender)==F,], aes(x =Country, y = PaidStarPerWatchedHour)) +
  geom_boxplot(aes(fill=Gender), show.legend = T)+
  labs(title = "",
       subtitle = "Country vs Gender",
       caption = "Source: MainGames",
       x = "",
       y = "Stars") +
  guides(scale="none") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "right", 
        legend.title = element_blank())+
  geom_text(aes(label= ifelse(PaidStarPerWatchedHour > 1,round(PaidStarPerWatchedHour,2),"")), vjust=1, size=4)

df[df$PaidStarPerWatchedHour > 1 & is.na(df$Gender)==F,]

# exclude extreme value 

ggplot(na.omit(df[df$PaidStarPerWatchedHour < 1,]), aes(x =Country, y = PaidStarPerWatchedHour), na.rm=T) +
  geom_boxplot(aes(fill=Gender), show.legend = T)+
  labs(title = "",
       subtitle = "Country vs Gender",
       caption = "Source: MainGames",
       x = "",
       y = "Stars") +
  guides(scale="none") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "right", 
        legend.title = element_blank())+
  geom_text(aes(label= ifelse(PaidStarPerWatchedHour > 1,round(PaidStarPerWatchedHour,2),"")), vjust=1, size=4)+
  facet_wrap(~MBTI_grouped, ncol=2)

# pyramid
levels(df$Gender)

library(devtools)
#
df1 <- aggregate(formula = PaidStarPerWatchedHour ~ Gender + MBTI_grouped, data = df[df$PaidStarPerWatchedHour<1,], FUN = mean)
df1$PaidStarPerWatchedHour <- ifelse(df1$Gender == "Male", -1*df1$PaidStarPerWatchedHour, df1$PaidStarPerWatchedHour)

ggplot(df1, aes(x = MBTI_grouped, y=PaidStarPerWatchedHour, fill = Gender)) + 
  geom_bar(data=subset(df1,Gender == "Female"), stat = "identity") + 
  geom_bar(data=subset(df1, Gender == "Male"), stat = "identity") + 
#  scale_y_continuous(labels = paste0(as.character(c(seq(2, 0, -1), seq(1, 2, 1))), "m"))+ 
  scale_fill_brewer(palette = "Set1") + 
  scale_x_reordered() +
  #  theme_bw()+
  scale_y_continuous(
                     breaks = seq(-0.05,0.05,0.01),
                     labels = abs(seq(-0.05,0.05,0.01)))+
  coord_flip()  
  
#
df1 <- aggregate(formula = PaidStarPerWatchedHour ~ Gender + Game, data = df[df$PaidStarPerWatchedHour<1,], FUN = mean)
df1$PaidStarPerWatchedHour <- ifelse(df1$Gender == "Male", -1*df1$PaidStarPerWatchedHour, df1$PaidStarPerWatchedHour)

ggplot(df1, aes(x = Game, y=PaidStarPerWatchedHour, fill = Gender)) + 
  geom_bar(data=subset(df1,Gender == "Female"), stat = "identity") + 
  geom_bar(data=subset(df1, Gender == "Male"), stat = "identity") + 
  #  scale_y_continuous(labels = paste0(as.character(c(seq(2, 0, -1), seq(1, 2, 1))), "m"))+ 
  scale_fill_brewer(palette = "Set1") + 
  scale_x_reordered() +
  #  theme_bw()+
  scale_y_continuous(
    breaks = seq(-0.16,0.12,0.02),
    labels = abs(seq(-0.16,0.12,0.02)))+
  coord_flip()  


# begin ggplot
ggplot(mapping = aes(x = MBTI_grouped, fill = Gender)) +
  
  # female histogram
  geom_histogram(data = linelist %>% filter(gender == "f"),
                 breaks = seq(0,85,5),
                 colour = "white") +
  
  # male histogram (values converted to negative)
  geom_histogram(data = linelist %>% filter(gender == "m"),
                 breaks = seq(0,85,5),
                 mapping = aes(y = ..count..*(-1)),
                 colour = "white") +
  
  # flip the X and Y axes
  coord_flip() +
  
  # adjust counts-axis scale
  scale_y_continuous(limits = c(-600, 900),
                     breaks = seq(-600,900,100),
                     labels = abs(seq(-600, 900, 100)))


# identify correlation among variables

#model.matrix(~0+., data=df) %>% 
#  cor(use="pairwise.complete.obs") %>% 
#  ggcorrplot(show.diag = F, type="lower", lab=F, lab_size=2)


names(df)
corrgram(df[,c(1:20)], lower.panel=panel.shade, upper.panel=panel.cor, order=T)

# top 10 correlation among dataset
corr_cross(df, max_pvalue = 0.05, top = 10)

# dependent against all
corr_var(df, PaidStarPerWatchedHour, top = 20) 

# MACHINE LEARNING APPROACH
set.seed(123)

# SPLIT DATASET
names(df)
sampleSplit <- sample.split(Y=df$PaidStarPerWatchedHour, SplitRatio=0.7)
trainSet <- subset(x=df, sampleSplit==TRUE)
testSet <- subset(x=df, sampleSplit==FALSE)


###########
# LINEAR MODEL
lm.mod <- lm(PaidStarPerWatchedHour ~ ., data=trainSet)
plot(lm.mod)
summary(lm.mod)

step.model <- stepAIC(lm.mod, direction = "both", trace = FALSE)
summary(step.model)

# best fitted linear model

#best.lm.mod <- regsubsets(PaidStarPerWatchedHour~.,
#             data =trainSet, really.big = T,
#             nbest = 1, nvmax = NULL, force.in = NULL, force.out = NULL,
#             method = "exhaustive")
#summary.best.lm.mod <- summary(best.lm.mod)
#as.data.frame(summary_best_subset$outmat)

#lm.full.model <- regsubsets(PaidStarPerWatchedHour~., data = trainSet, nvmax = 5,
#                     method = "seqrep")
#summary(models)

# too many predictors, take forever computation, not a correct way to analyze
# computer can calculate 100,000 models per second then it will only take 400+ years to complete (assuming that your computer does not start swapping memory to keep all the results, that really slows things down
2^100/100000/60/60/24/365.24 
############ LINEAR REGRESSION DOES NOT SEEM TO BE A GOOD MODEL FOR THIS DATASET

######### DECISION TREE
# rpart
mod <- rpart(PaidStarPerWatchedHour~.,trainSet)
printcp(mod)
plotcp(mod)
plot(mod)
plot(mod, uniform=T)
text(mod,cex=1,digits=3,font=2)
summary(mod)

rpart.plot(mod)

mod
mod$variable.importance

######

######## random forest
library(randomForest)
set.seed(123)

row.has.na <- apply(trainSet, 1, function(x){any(is.na(x))})
trainSet_new <- trainSet[!row.has.na, ]
str(trainSet_new)

rf.model <- randomForest(PaidStarPerWatchedHour~.,trainSet_new,importance=TRUE) 
rf.model # Can not handle categorical predictors with more than 53 categories.

# exclude game 
rf.model <- randomForest(PaidStarPerWatchedHour~.,trainSet_new[,-3],importance=TRUE) 
print(rf.model)

importance(rf.model)
varImpPlot(rf.model,pch=21,bg=4, main = "Random Forest")
mtext(line=1,adj=1,paste("Sample size: ",nrow(trainSet_new),sep=""))

row.has.na <- apply(testSet, 1, function(x){any(is.na(x))})
testSet_new <- testSet[!row.has.na, ]

pred.rf <- predict(rf.model,newdata=testSet_new[,-3])
#windows()
plot(testSet_new$PaidStarPerWatchedHour,pred.rf,ylab="",
     xlab="Random Forest",cex.lab=1.2,cex.axis=1.2, las=1)
mtext(side=3,line=0.15,adj=-0.1,"Fitted Values")
mtext(side=3,line=0.15,adj=1,paste(nrow(testSet_new),"observations"))
mtext(side=3,line=1.5,adj=0.5,"Random Forest",font=2)
lm.model <- lm(pred.rf~testSet_new$PaidStarPerWatchedHour)
rsq <- summary(lm.model)$adj.r.sq
abline(lm.model$coef,lwd=2,col=5)
lg <- paste("r-sq:",round(100*rsq,2),"%")
legend("bottomright",bty="n",leg=lg,cex=1.2)
mse <- mean((pred.rf-testSet_new$PaidStarPerWatchedHour)^2) 
lg1 <- paste("mse:",round(mse,5))
legend("bottomright",y.intersp=2.5,bty="n",leg=lg1,cex=1.2)

###########
# BOOSTING
set.seed(1)
boost.model = gbm(PaidStarPerWatchedHour~.,data=trainSet_new,distribution="gaussian",n.trees=10000,interaction.depth=1, cv.folds=5)
windows()
par(mar=c(5,20,5,2))
summary(boost.model, cBars=10, las=2)
print(boost.model)


attributes(a)
a[c(1:15),]

sqrt(min(boost.model$cv.error))
gbm.perf(boost.model, method = "cv")

#windows()
#par(mfrow=c(2,2))

pred.boost=predict(boost.model,newdata=testSet_new,n.trees=5000)
mean((pred.boost-testSet_new$PaidStarPerWatchedHour)^2,na.rm=T)

#windows()
plot(testSet_new$PaidStarPerWatchedHour,pred.boost,ylab="",
     xlab="Boosting",cex.lab=1.2,cex.axis=1.2, las=1)
mtext(side=3,line=0.15,adj=-0.1,"Fitted Values")
mtext(side=3,line=0.15,adj=1,paste(nrow(testSet_new),"observations"))
mtext(side=3,line=1.5,adj=0.5,"Boosting",font=2)
lm.model <- lm(pred.boost~testSet_new$PaidStarPerWatchedHour)
rsq <- summary(lm.model)$adj.r.sq
abline(lm.model$coef,lwd=2,col=5)
lg <- paste("r-sq:",round(100*rsq,2),"%")
legend("bottomright",bty="n",leg=lg,cex=1.2)
mse <- mean((pred.boost-testSet_new$PaidStarPerWatchedHour)^2) 
lg1 <- paste("mse:",round(mse,5))
legend("bottomright",y.intersp=2.5,bty="n",leg=lg1,cex=1.2)

###########
########### Neural Network
library(neuralnet)
str(trainSet)
nn <- neuralnet(PaidStarPerWatchedHour~.,data=trainSet[,-3],hidden=10, threshold=0.01)



###############
############### COMPARISON

# kNN
set.seed(123)
fit.knn <- train(PaidStarPerWatchedHour~.,data=trainSet_new, method="knn")
# c) advanced algorithms
# SVM
set.seed(123)
fit.svm <- train(PaidStarPerWatchedHour~.,data=trainSet_new, method="svmRadial")
# Random Forest
set.seed(123)
#fit.rf <- train(PaidStarPerWatchedHour~.,data=trainSet_new, method="rf") # takes forever


# summarize accuracy of models
results <- resamples(list( knn=fit.knn, svm=fit.svm))
summary(results)
dotplot(results)
