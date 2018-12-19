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
  axis.text.x = element_text(size=40, angle=45, hjust=1),
  axis.title.x = element_text(size=40),
  axis.text.y = element_text(size=40),
  axis.title.y = element_text(size=40),
  strip.text = element_text(size=40),
  panel.spacing = unit(2, "lines"))

#Set colours
groupcolours <- c("deepskyblue2","aquamarine3","aquamarine4","deepskyblue","deepskyblue3","lightskyblue")

#load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper//supplementary_materials/localsampledcorpus/"

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
sink("corrected_reconstruction_analysis.txt")
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
plot.local.suppl <- ggplot(plotdata, 
                aes(x=age, y = averagescore, group = child, linetype = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(-5,2))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
#png(paste(plot.path,
#          "posterplot.png", sep=""),
#    width=900,height=500,units="px",
#    bg = "transparent")
#plot.local.suppl <-plot.local.suppl + theme_apa()
#dev.off()
#plot.local.suppl

###CUMULATIVE DATA
#For cumulatively sampled corpus
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/accumulativesampledcorpus/"

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
sink("corrected_reconstruction_analysis.txt")
summary(model_age_cumulative)
sink()
summary(model_age_cumulative)
ranef(model_age_cumulative)


#Following-up on non-convergence of the model: it's not a serious warning, see below:

relgrad <- with(model_age_cumulative@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

#Make plot for poster
plotdata <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

#Make plot 
plot.acc.suppl <- ggplot(plotdata, 
                    aes(x=age, y = averagescore, group = child, linetype = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(-5,2))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #theme(legend.position = "none") +
  theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "posterplot_cumulative.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
#plot.acc.suppl <- plot.acc.suppl + theme_apa()
dev.off()
plot.acc.suppl 

### COMBINE FIGURES

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

plot.local.suppl <- plot.local.suppl + theme_apa(x.font.size = 18, y.font.size = 18,
                                                 legend.font.size = 18)
plot.acc.suppl <- plot.acc.suppl + theme_apa()

plot.combined <- grid_arrange_shared_legend(plot.local.suppl, plot.acc.suppl, ncol = 2, nrow = 1, position = "right")

png(paste(plot.path,
          "supplplotcombined.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.combined <- grid_arrange_shared_legend(plot.local.suppl, plot.acc.suppl, ncol = 2, nrow = 1, position = "right")
dev.off()
plot.combined +theme(axis.text=element_text(size=12),
                        axis.title=element_text(size=14,face="bold"))
