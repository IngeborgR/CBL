#Loading packages and setting paths
library(ggplot2)
library(lme4)
library(grid)
library(gridExtra)
library(stringr)
library(jtools)
library(lattice)

#Plot layout settings
basic.theme <- theme(
  panel.background = element_rect(
    fill = "transparent",colour = NA),
  panel.grid.major = element_line(colour = "grey95"),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(
    fill = "transparent",colour = NA),
  legend.background = element_rect(
    fill="transparent"),
  legend.text = element_text(size=24),
  legend.title = element_text(size=30),
  legend.key.height = unit(2, "lines"),
  legend.key = element_rect(colour = NA, fill = NA),
  axis.text.x = element_text(size=30, angle=45, hjust=1),
  axis.title.x = element_text(size=30),
  axis.text.y = element_text(size=28),
  axis.title.y = element_text(size=32),
  strip.text = element_text(size=30),
  panel.spacing = unit(2, "lines"))

#Set colours
groupcolours <- c("deepskyblue2","aquamarine3","aquamarine4","deepskyblue","deepskyblue3","lightskyblue")

#set working directory and output directory
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus/"

#open and merge csvfiles
filenames <- list.files(pattern="productiontask*.csv")
data.list <- lapply(filenames, sep = ";",na.strings=c("NaN","Nan"), read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$utterance <- as.character(data$utterance)
data$numwords <- (str_count(data$utterance,"Word")-1)

#For analysis:
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is skipped, Y=0 if not skipped.
newdata <- subset(data,select=c(1,3,4,5,6,12))
newdata$Y <- ifelse(newdata$skipped == "True", 1,0)

## Model without controlling for number of words in an utterance 

# model <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
# #NOTE: the glmer package automatically turns (age|child) into (1+age|child)
# sink("ageanalysis_skipped.txt")
# summary(model)
# sink()
# summary(model)
# ranef(model)

#### HERE CORRECT FOR NUMBER OF WORDS PER UTTERANCE ####
model_numwords <- glmer(Y ~ age + (age|child) + numwords, family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child)
sink("corrected_unknown_local.txt")
summary(model_numwords)
sink()
summary(model_numwords)
ranef(model_numwords)

#Plot figure 2
#Compute percentage of skipped utterances per child per age
aggdata <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = sum)
aggdata2 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(aggdata)[3] <- "absolute_score"
colnames(aggdata2)[3] <- "total_num_utterances"
total <- merge(aggdata, aggdata2, by = c("child","age"))
total$percentages <- (total$absolute_score / total$total_num_utterances)*100

#Make plot
plot.local.unknown <- ggplot(total, 
                aes(x=age, y = percentages, group = child, linetype = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) +
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("% Utterances containing \nnew words\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))


#Save plot
png(paste(plot.path,
          "corrected_unknown_local.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.unknown + theme_apa()
dev.off()
plot.local.unknown + theme_apa()

##For cumulatively sampled corpus

setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus/"

#open and merge csvfiles
filenames <- list.files(pattern="productiontask*.csv")
data.list <- lapply(filenames, sep = ";",na.strings=c("NaN","Nan"), read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$utterance <- as.character(data$utterance)
data$numwords <- (str_count(data$utterance,"Word")-1)

#For analysis:
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is skipped, Y=0 if not skipped.
newdata <- subset(data,select=c(1,3,4,5,6,12))
newdata$Y <- ifelse(newdata$skipped == "True", 1,0)

### Model that does not take utterance length into account
# model <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
# #NOTE: the glmer package automatically turns (age|child) into (1+age|child)
# sink("ageanalysis_skipped.txt")
# summary(model)
# sink()
# summary(model)
# ranef(model)

#### HERE CORRECT FOR NUMBER OF WORDS PER UTTERANCE ####
model_numwords <- glmer(Y ~ age + (age|child) + numwords, family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child)
sink("corrected_unknown_cumulative.txt")
summary(model_numwords)
sink()
summary(model_numwords)
ranef(model_numwords)


#Plot figure 2
#Compute percentage of skipped utterances per child per age
aggdata <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = sum)
aggdata2 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(aggdata)[3] <- "absolute_score"
colnames(aggdata2)[3] <- "total_num_utterances"
total <- merge(aggdata, aggdata2, by = c("child","age"))
total$percentages <- (total$absolute_score / total$total_num_utterances)*100

#Make plot
plot.acc.unknown <- ggplot(total, 
                aes(x=age, y = percentages, group = child, linetype = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) +
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("% Utterances containing \nnew words\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))


#Save plot
png(paste(plot.path,
          "corrected_unknown_cumulative.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.unknown + theme_apa()
dev.off()
plot.acc.unknown + theme_apa()