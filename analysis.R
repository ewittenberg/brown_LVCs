## user input before running anew
library(car)
setwd("~/GitHub/brown_LVCs/raw data")
# read in the target data, with duplicates removed
target <- read.csv("merged.csv")
nrow(target)
head(target)
myvars <- c("Name", "Verb", "Age", "Convergence")
data <- target[myvars]
head(data)
pronounsdata <- droplevels(subset(target, (Convergence %in% c("pronoun"))))
nrow(pronounsdata)
#use only unambiguous events and objects
levels(data$Convergence)
data <- droplevels(subset(data, !(Convergence %in% c("","!!!","0", "pronoun"))))
data$Convergence <- recode(data$Convergence, "'object '='object'")
nrow(data)

#recode verbs
levels(data$Verb)
data$Verb <- recode(data$Verb, "'do|does|doing|did|didnt|done|doesnt'='do'");
data$Verb <- recode(data$Verb, "'get|gets|got|gotten|getting'='get'");
data$Verb <- recode(data$Verb, "'give|gives|given|gived|giving|gave'='give'");
data$Verb <- recode(data$Verb, "'have|haves|having|had|havent'='have'");
data$Verb <- recode(data$Verb, "'make|makes|making|maked'='make'")

str(data)
summary(data)
type.table <- xtabs(~ Convergence, data)
prop.table(type.table)

type.by.verb <- xtabs(~ Convergence + Verb, data)
type.by.verb

prop.table(type.by.verb, margin=2)
barplot(prop.table(type.by.verb, margin=2) * 100, xlab="Verb",ylab="Type of Syntactic Object (%)", legend.text=c("event","object"))
