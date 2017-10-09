## user input before running anew
library(car)
library('scales')
setwd("~/GitHub/brown_LVCs/raw data")
# read in the target data, with duplicates removed
target <- read.csv("merged.csv")
nrow(target)
head(target)
myvars <- c("Name", "Verb", "Age", "Convergence")
data <- target[myvars]
head(data)
pronounsdata <- droplevels(subset(target, (Convergence %in% c("pronoun", "0", ""))))
nrow(pronounsdata)
otherdata <- droplevels(subset(target, (!Convergence %in% c("object","event", "object "))))
nrow(otherdata)
nulldata <- droplevels(subset(target, (Convergence %in% c(""))))
nrow(nulldata)

#use only unambiguous events and objects
levels(data$Convergence)
data <- droplevels(subset(data, !(Convergence %in% c("","!!!","0", "pronoun"))))
data$Convergence <- recode(data$Convergence, "'object '='object'")
nrow(data)

nrow(data) + nrow(otherdata)

#recode verbs
levels(data$Verb)
data$Verb <- recode(data$Verb, "'do|does|doing|did|didnt|done|doesnt'='do'");
data$Verb <- recode(data$Verb, "'get|gets|got|gotten|getting'='get'");
data$Verb <- recode(data$Verb, "'give|gives|given|gived|giving|gave'='give'");
data$Verb <- recode(data$Verb, "'have|haves|having|had|havent'='have'");
data$Verb <- recode(data$Verb, "'make|makes|making|maked'='make'")
levels(data$Convergence)
data$Convergence <- recode(data$Convergence, "'event'='light'")
data$Convergence <- recode(data$Convergence, "'object'='non-light'")

str(data)
summary(data)

####simple pie chart of light vs non-light ####
png(filename="~/GitHub/brown_LVCs/simplepie.png")

type.table <- xtabs(~ Convergence, data)
mytable <- prop.table(type.table)
lbls <- paste(names(mytable), "\n", sprintf("%.1f%%",round(prop.table(type.table)*100, 3)),  sep="")
pie(prop.table(type.table), labels = lbls, col = c('green3', 'blue'), cex=2.5)
dev.off()

####per verb pie chart of light vs non-light ####
type.by.verb <- xtabs(~ Convergence + Verb, data)
type.by.verb
png(filename="~/GitHub/brown_LVCs/bar_per_verb.png")

prop.table(type.by.verb, margin=2)
barplot(prop.table(type.by.verb, margin=2) * 100, cex = 1.5, 
        cex.axis=1.5,cex.lab=1.5, xlab="Verb",col = c('green3', 'blue'), 
        ylab="Type of Syntactic Object (%)", legend.text=c("light","non-light"))
dev.off()

####factor in age####
head(data$Year)
data$Year <- sapply(strsplit(as.character(data$Age),'P'), "[", 2);
data$Year <- sapply(strsplit(as.character(data$Year),'M'), "[", 1);
data$Year <- as.factor(gsub("Y", ".", data$Year))
str(data)

age.table <- xtabs(~ Convergence + Year, data)
age.table
png(filename="~/GitHub/brown_LVCs/bar_per_age.png")

barplot(prop.table(age.table, margin=2) * 100, xlab="Age",
        col = c('green2', 'blue'), 
        cex = 1.5, cex.axis=1.5,cex.lab=1.5,
        ylab="Type of Syntactic Object (%)", legend.text=c("light","non-light"))

dev.off()
age.table <- xtabs(~ Convergence + Year, data=subset(data, Verb=="do"))
age.table
barplot(prop.table(age.table, margin=2) * 100, xlab="Age",ylab="Type of Syntactic Object (%)", legend.text=c("event","object"))
