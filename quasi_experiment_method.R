PSData <- read_excel("Desktop/Harrisburg University/ANLY 510-51-A/PSData.xlsx")

missing <- data.frame(is.na(PSData))
pmissing <- apply(missing, 2, mean)
names(missing)[pmissing > 0]<-paste(names(PSData)[pmissing >0], "NA", sep="")
PSData<-cbind(PSData,missing[,pmissing>0])
print(round(pmissing,3))


datacleaned<- PSData %>%
  select(rating, package, college , unemployed , age , similarsoftwareused , multiplecomputers , income , employerreimbursement , married) %>%
  na.omit()
psmodel <- formula("package ~ college + unemployed + age + similarsoftwareused + multiplecomputers + income + employerreimbursement + married")

logit <- glm(psmodel, family = "binomial", data = datacleaned)
datacleaned$logitscored <- fitted(logit)

library(ggplot2)
datacleaned %>%
  ggplot(aes(x = logitscored)) +
  geom_histogram(color = "white") +
  facet_wrap(~package) +
  xlab("Probability of purchasing gold package: PS")
boxplot(logitscored~package, data = datacleaned)



#Matching

datacleaned$tps <- log(datacleaned$logitscored/(1- datacleaned$logitscored))

#MATCHING
#greedy matching
install.packages("MatchIt")
library("MatchIt")
greedy <- matchit(psmodel, distance = datacleaned$tps, m.order = "largest", data = datacleaned, method = "nearest", replace = TRUE, caliper = .25)

summary(greedy)

#genetic matching
install.packages("rgenoud")
library("rgenoud")
genetic <- matchit(psmodel, distance = datacleaned$tps, data = datacleaned, method = "genetic", pop.size = 1000, fit.func="pvals", estimand = "ATT", replace = TRUE, ties = TRUE)

#analysis
readydata <- match.data(greedy)
install.packages("survey")
library("survey")
analysis <- svydesign(ids = ~1, weights = ~weights, data = readydata)
modelgreedy <- svyglm(rating~package, analysis, family = gaussian())
summary(modelgreedy)

#WEIGHTING
datacleaned$ATTw <- with(datacleaned, ifelse(package == 1, 1, logitscored /(1- logitscored)))
with(datacleaned, by(ATTw,package, summary))

model <- glm (rating ~ package, weights = ATTw, data = datacleaned) 
summary(model)
#STRATIFICATION
summary(datacleaned)
datacleaned$stratum <- cut(x = datacleaned$logitscored, breaks = quantile(datacleaned$logitscored), prob = seq(0, 1, 1/5), include.lowest = TRUE)

levels(datacleaned$stratum) <- 1:length(levels(datacleaned$stratum))
##############
stratification <- matchit(psmodel, data = datacleaned, method = "subclass", sub.by = "treat", subclass = 5)
data.stratum <- match.data(stratification)
bcheck <- summary(data.stratum, standardize = TRUE)

stdmeandifferences <- data.frame(stratification$q.table[,3,])
summary(stdmeandifferences)

summary(abs(bcheck$sum.subclass$"Std. Mean Diff."))

table(abs(bcheck$sum.subclass$"Std. Mean Diff.") > 0.1)


