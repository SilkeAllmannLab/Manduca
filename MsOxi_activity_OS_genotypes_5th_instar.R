## One-way Anova and Post-hoc to compare MsOxi activity of OS from 5th instar M.sexta WT/HT/KO larvae 

## Open packages
library("ggplot2")
library("readxl")
library("mvnormtest")
library("ggpubr")
library("ggsignif")
library("multcompView")
library("plyr")
library("dplyr")
library("RColorBrewer")
library("reshape2")

## Load data
Juliette_data_II <- read_excel("surfdrive/Lab/R/Manduca/MsOxi_activity_OS_genotypes_5th_instar/Data/Juliette_data_II.xlsx")
View(Juliette_data_II)

## Give data set a shorter name
data <- Juliette_data_II

## View structure of dataset
str(data)

## Make variable with Genotype_CP as categorical variable
Genotype <- as.factor(data$Genotype_CP)

## Control
class(Genotype)
## If OK, it will say "factor"

## Add new variable to dataset and check variables
data$Genotype <- Genotype

## CHANGE ORDER FACTOR

# Specify levels in factor Genotype
Genotype <- factor(Genotype, levels=c("WT", "HT", "KO", "Control"))
levels(data$Genotype)

# Note that the levels are not in a meaningful order
# Change the order to something useful
data$Genotype <- factor(data$Genotype, levels = data$Genotype[c("WT","HT","KO","Control")])
levels(data$Genotype)
Genotype

## Add new variable to dataset and check variables
data$Genotype <- Genotype

## LINEAR REGRESSION ANALYSIS
mod1<-lm(data$Percentage_E2AL~data$Genotype)
summary(mod1)

## Can we assume normality?
plot(mod1)
qplot(x = Genotype, y = Percentage_E2AL, data = data) +
  geom_smooth(method = mod1)

# Anova and Tukey HSD to identify significant differences in variance and where (post hoc doesn't work yet, too little df)
Anova<-aov(Percentage_E2AL~ Genotype, data=data)
summary(Anova)
Interaction<-TukeyHSD(Anova)
print(Interaction)

## Group letters for graph
tukeylabel <- multcompLetters(extract_p(Interaction$Genotype), threshold = 0.05)
print (tukeylabel)

# Turn into data.frame for ggplot
tukeyLabelDF <- as.data.frame(x=tukeylabel$Letters)
colnames(tukeyLabelDF) <- c('Marks')                   ##set column names
tukeyLabelDF <- transform(tukeyLabelDF, Genotype=row.names(tukeyLabelDF)) #add Genotype labes for ordering

# Get means for plotting
library(plyr)
library(reshape2)

# Melt data frame; get means in a new data frame. SPECIFY VARIABLES!
melted <- melt(data, id.vars=c("Genotype"), measure.vars = "Percentage_E2AL")

means <- ddply(melted, c("Genotype"), summarise,
               Mean=mean(value))

## Add SD to the matrix
means.sd <- ddply(melted, c("Genotype"), summarise,
                  Mean=mean(value), SD=sd(value))

## Labels for post-hoc test matching order in data frame
tukeyLabelDF <- tukeyLabelDF[match(means.sd$Genotype, tukeyLabelDF$Genotype),]

## Add values for error bars to the matrix
means.sd <- transform(means.sd, lower=Mean-SD, upper=Mean+SD, tukey=tukeyLabelDF$Marks)
print (means.sd)


# Draw boxplot 
b <-ggplot(data, aes(x = Genotype, y = Percentage_E2AL, fill= Genotype)) +
  geom_boxplot(outlier.shape=NA, alpha=0.5, width=0.5) +
  scale_y_continuous(name = "% E2AL",
                     breaks = seq(0,92,10),
                     limits = c(0,92)) +
  scale_x_discrete(name = "Genotype") +
  ggtitle("MsOxi activity of OS M.sexta caterpillars (5th instar)") +
  theme_bw() +
  geom_jitter(size=0.8, color= "black", width=0.15)+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE) +
  geom_text(data=means.sd, aes(x=Genotype, y=88, label=tukey))
b + theme(legend.position ="none")
