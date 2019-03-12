setwd("/Users/Christine/Downloads/")
# Preprocessing
og_survey <- read.csv("stack-overflow-2018-developer-survey/survey_results_public.csv")
og_survey <- data.table(og_survey)
og_survey <- subset(og_survey, !is.na(ConvertedSalary))
og_survey$ConvertedSalary <- as.numeric(as.character(og_survey$ConvertedSalary))
og_survey <- subset(og_survey, !is.na(ConvertedSalary))
og_survey <- subset(og_survey, (Country=="United States")|(Country=="China")|(Country=="Japan")|(Country=="Germany")|(Country=="United Kingdom")|(Country=="France")|(Country=="India")|(Country=="Brazil")|(Country=="Italy")|(Country=="Canada"))
og_survey <- subset(og_survey, (ConvertedSalary>=(quantile(ConvertedSalary,probs=0.25)-IQR(ConvertedSalary)))&(ConvertedSalary<=(quantile(ConvertedSalary,probs=0.75)+IQR(ConvertedSalary))))
og_survey <- subset(og_survey, Employment=="Employed full-time")
og_survey$salarycategory <- ifelse(og_survey$ConvertedSalary>=97910, "High", ifelse(og_survey$ConvertedSalary>=69543, "Higher than average", ifelse(og_survey$ConvertedSalary>=39040, "Lower than average", "Low")))
temp <- og_survey
temp <- temp[,-c(1,5,10,13:114,127:129)]

temp_1 <- na.omit(temp)

rerun_factor <- function(x) {
  if (is.factor(x)) return(factor(x))
  return(x)
}
temp_1 <- as.data.frame(lapply(temp_1, rerun_factor))
temp_1$Employment <- NULL
temp_1$RaceEthnicity <- NULL

#### RQ 1: Which attribute would affect one's salary the most? <- Use tree
library(tree)
library(caret)
set.seed(1234)
ind <- sample(2, nrow(temp_1), replace=TRUE, prob=c(0.8, 0.2))
trainData <- temp_1[ind==1,]
testData <- temp_1[ind==2,]

modelfit= tree(salarycategory~ ., data=trainData)


library(RColorBrewer)
pal <- brewer.pal(9,"Set1")

print(modelfit)
plot(modelfit, col=pal)
text(modelfit, col=pal)
dev.off()

treeprediction <- predict(modelfit, newdata=testData,type="class")
confusionMatrix(treeprediction, testData$salarycategory)




#### RQ 2: How much salary will one get according to their characteristics?

rq2 <- og_survey[,c("Country","YearsCodingProf","ConvertedSalary")]
lm_country <- lm(formula = ConvertedSalary ~ Country, data = rq2)
lm_yearscoding <- lm(formula = ConvertedSalary ~ YearsCodingProf, data = rq2)
summary(lm_country)$coef

summary(lm_country)
library(ggplot2)
ggplot(rq2, aes(x = Country, y = ConvertedSalary, colour=Country, group=1)) + geom_point() + labs(x="Country") + geom_smooth(method = "lm", col = "black")
ggplot(rq2, aes(x = YearsCodingProf, y = ConvertedSalary, colour=YearsCodingProf, group=1)) + geom_point() + labs(x="YearsCodingProf") + stat_smooth(method = "lm",fullrange=TRUE, col = "black")

ggplot(rq2, aes(x=Country, y=ConvertedSalary, group=1)) + geom_point() + geom_smooth(method=lm)



summary(lm(formula = ConvertedSalary ~ factor(Country), data = rq2))
summary(lm(formula = ConvertedSalary ~ factor(YearsCodingProf), data = rq2))


rq2 <- fastDummies::dummy_cols(rq2)
rq2$YearsCodingProf_NA <- NULL


