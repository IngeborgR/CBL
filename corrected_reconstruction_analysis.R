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

#load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask.csv")
data.list <- lapply(filenames, sep = ";",na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)

#Analysis
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
#Y=0 if not. Exclude all skipped utterance.
newdata <- subset(data,select=c(1,3,4,5,6,10,11))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

#Model with Y as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age <- lmer(controlledscore ~ age + (age|child), data = newdata, control=lmerControl(optimizer = "bobyqa"),optCtrl=list(maxfun=100000))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept 
#is included still
sink("corrected_reconstruction_local.txt")
summary(model_age)
sink()
summary(model_age)
ranef(model_age)


#Make plot for poster
plotdata <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

#Make plot 
plot.local.reconstruction <- ggplot(plotdata, 
                aes(x=age, y = averagescore, group = child, linetype = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(-5,2))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "correct_recon_local.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.reconstruction+theme_apa()
dev.off()
plot.local.reconstruction+theme_apa()


# #Plot figure 1: 'original score' against baseline probability
# #Note to self: If I want to display the two graphs side-by-side: ensure the Y-axes of both graphs are the same
# #Compute percentage of correctly reconstructed utterances per child per age
# aggdata <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = sum)
# aggdata2 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
# aggdata3 <- aggregate(newdata$chance, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = sum)
# colnames(aggdata)[3] <- "score"
# colnames(aggdata2)[3] <- "total_num_utterances"
# colnames(aggdata3)[3] <- "score"
# total <- merge(aggdata, aggdata2, by = c("child","age"))
# total$percentages <- (total$score / total$total_num_utterances)*100
# chance <- merge(aggdata2, aggdata3, by = c("child","age"))
# chance$percentages <- (chance$score / chance$total_num_utterances)*100
# total <- subset(total, total$percentages != 'NA')
# chance <- subset(chance, chance$percentages != 'NA')
# 
# #Make plot 
# plot1 <- ggplot(data = total, 
#                 aes(x=age, y = percentages, fill = child)) +#group = child, color = child)) + 
#   geom_bar(position = "dodge", stat = "identity",size = 1) + 
#   #geom_bar(aes(y = chancepercentages)), stat, size = 1.5) + 
#   #basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #scale_colour_manual(name = "Child:", values = groupcolours) + 
#   coord_cartesian(ylim=(c(0,100))) + 
#   xlab("\nAge (years)") + 
#   ylab("% Corrrectly reconstructed \nutterances\n") + 
#   #ggtitle("Correctly reconstructed utterances\ndecrease with age (p < 0.001)") + 
#   #basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "correctedageanalysis.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot1
# dev.off()
# plot1
# 
# plot2 <- ggplot(data = chance, 
#                 aes(x=age, y = percentages, fill = child)) +#group = child, color = child)) + 
#   geom_bar(position = "dodge", stat = "identity") + 
# #geom_bar(aes(y = chancepercentages)), stat, size = 1.5) + 
# #basic.theme + theme(axis.text.x = element_text(size=22)) + 
# #scale_colour_manual(name = "Child:", values = groupcolours) + 
# coord_cartesian(ylim=(c(0,100))) + 
# xlab("\nAge (years)") + 
# ylab("% Corrrectly reconstructed \nutterances\n") + 
# #ggtitle("Correctly reconstructed utterances\ndecrease with age (p < 0.001)") + 
# #basic.theme + theme(axis.text.x = element_text(size=22)) + 
# #theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "correctedageanalysischance.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot2
# dev.off()
# plot2

###CUMULATIVE DATA
#For cumulatively sampled corpus
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus/"


#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask.csv")
data.list <- lapply(filenames, sep = ";",na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)

#Analysis
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
#Y=0 if not. Exclude all skipped utterance.
newdata <- subset(data,select=c(1,3,4,5,6,10,11))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

#Model with Y as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_cumulative <- lmer(controlledscore ~ age + (age|child), data = newdata)#, control=lmerControl(optimizer = "bobyqa"),optCtrl=list(maxfun=100000))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept 
#is included still
sink("corrected_reconstruction_cumulative.txt")
summary(model_age_cumulative)
sink()
summary(model_age_cumulative)
ranef(model_age_cumulative)

#Make plot for poster
plotdata <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

#Make plot 
plot.acc.reconstruction <- ggplot(plotdata, 
                    aes(x=age, y = averagescore, group = child, linetype = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(-5,2))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "correct_recon_cumulative.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.reconstruction + theme_apa()
dev.off()
plot.acc.reconstruction + theme_apa()

# ##ADD in graph here
# plot1 <- ggplot(data = total, 
#                 aes(x=age, y = percentages, fill = child)) +#group = child, color = child)) + 
#   geom_bar(position = "dodge", stat = "identity",size = 1) + 
# #geom_bar(aes(y = chancepercentages)), stat, size = 1.5) + 
# #basic.theme + theme(axis.text.x = element_text(size=22)) + 
# #scale_colour_manual(name = "Child:", values = groupcolours) + 
# coord_cartesian(ylim=(c(0,100))) + 
# xlab("\nAge (years)") + 
# ylab("% Corrrectly reconstructed \nutterances\n") + 
# #ggtitle("Correctly reconstructed utterances\ndecrease with age (p < 0.001)") + 
# #basic.theme + theme(axis.text.x = element_text(size=22)) + 
# #theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "correctedageanalysiscumulative.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot1
# dev.off()
# 
# plot2 <- ggplot(data = chance, 
#                 aes(x=age, y = percentages, fill = child)) +#group = child, color = child)) + 
#   geom_bar(position = "dodge", stat = "identity") + 
# #geom_bar(aes(y = chancepercentages)), stat, size = 1.5) + 
# #basic.theme + theme(axis.text.x = element_text(size=22)) + 
# #scale_colour_manual(name = "Child:", values = groupcolours) + 
# coord_cartesian(ylim=(c(0,100))) + 
# xlab("\nAge (years)") + 
# ylab("% Corrrectly reconstructed \nutterances\n") + 
# #ggtitle("Correctly reconstructed utterances\ndecrease with age (p < 0.001)") + 
# #basic.theme + theme(axis.text.x = element_text(size=22)) + 
# #theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "correctedageanalysischancecumulative.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot2
# dev.off()