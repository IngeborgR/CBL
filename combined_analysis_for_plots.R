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
  #ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrecon.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.reconstruction+theme_apa()
dev.off()
plot.local.reconstruction+theme_apa()


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
  #ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotaccrecon.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.reconstruction + theme_apa()
dev.off()
plot.acc.reconstruction + theme_apa()


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
  #ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))


#Save plot
png(paste(plot.path,
          "plotlocalunknown.png", sep=""),
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
  #ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))


#Save plot
png(paste(plot.path,
          "plotaccunknown.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.unknown + theme_apa()
dev.off()
plot.acc.unknown + theme_apa()


### MERGE PLOTS

# COMBINE 1: plot.local.reconstruction &  plot.local.unknown  and 2: plot.acc.reconstruction & plot.acc.local

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

plot.local.reconstruction <- plot.local.reconstruction + theme_apa(x.font.size = 18, y.font.size = 18,
                                                                   legend.font.size = 18) + theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
                                                                   )
plot.local.unknown <- plot.local.unknown + theme_apa(x.font.size = 18, y.font.size = 18,
                                                     legend.font.size = 18) + theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
                                                     )
plot.local.combined <- grid_arrange_shared_legend(plot.local.reconstruction, plot.local.unknown, ncol = 2, nrow = 1, position = "right")

png(paste(plot.path,
          "plotlocalcombined.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.combined <- grid_arrange_shared_legend(plot.local.reconstruction, plot.local.unknown, ncol = 2, nrow = 1, position = "right")
dev.off()
plot.local.combined

plot.acc.reconstruction <- plot.acc.reconstruction + theme_apa(x.font.size = 18, y.font.size = 18,
                                                               legend.font.size = 18) + theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
                                                               )
plot.acc.unknown <- plot.acc.unknown + theme_apa(x.font.size = 18, y.font.size = 18,
                                                 legend.font.size = 18) + theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
                                                 )
plot.acc.combined <- grid_arrange_shared_legend(plot.acc.reconstruction, plot.acc.unknown, ncol = 2, nrow = 1, position = "right")

png(paste(plot.path,
          "plotacccombined.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.combined <- grid_arrange_shared_legend(plot.acc.reconstruction, plot.acc.unknown, ncol = 2, nrow = 1, position = "right")
dev.off()
plot.acc.combined

