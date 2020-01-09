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
Juliette_data_V <- read_excel("surfdrive/Lab/R/Manduca/MsOxi_activity_OS_developmental_stages/Data/Juliette_data_V.xlsx")
View(Juliette_data_V)

## Give data set a shorter name
data <- Juliette_data_V

## View structure of dataset
str(data)

## Make variable with Genotype_CP as categorical variable
Developmental_stage <- as.factor(data$Developmental_stage)

## Control
class(Developmental_stage)
## If OK, it will say "factor"

## Add new variable to dataset and check variables
data$Developmental_stage <- Developmental_stage

## CHANGE ORDER FACTOR

# Specify levels in factor Genotype
Developmental_stage <- factor(Developmental_stage, levels=c("Ctrl", "1st_instar", "2nd_instar", "3rd_instar", "4th_instar", "5th_instar"))
levels(data$Developmental_stage)

# Note that the levels are not in a meaningful order
# Change the order to something useful
data$Developmental_stage <- factor(data$Developmental_stage, levels = data$Developmental_stage[c("Ctrl","1st_instar","2nd_instar", "3rd_instar", "4th_instar", "5th_instar")])
levels(data$Developmental_stage)
Developmental_stage

## Add new variable to dataset and check variables
data$Developmental_stage <- Developmental_stage

## LINEAR REGRESSION ANALYSIS
mod1<-lm(data$Percentage_E2AL~data$Developmental_stage)
summary(mod1)

## Can we assume normality?
plot(mod1)
qplot(x = Developmental_stage, y = Percentage_E2AL, data = data) +
  geom_smooth(method = mod1)

# Parametric: assume normality
# Boxplots with p-value based on t-test (parametric: assume normality)
b <-ggplot(data, aes(x = Developmental_stage, y = Percentage_E2AL, color=Developmental_stage)) +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(name = "% E2AL",
                     breaks = seq(0,50,5),
                     limits = c(0,50)) +
  scale_x_discrete(name = "Developmental stage") +
  ggtitle("MsOxi activity of OS during larval development of M. sexta") +
  theme_bw() +
  geom_jitter(size=0.8, color= "black")+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE)
b

# Anova and Tukey HSD to identify significant differences in variance and where (post hoc doesn't work yet, too little df)
Anova<-aov(Percentage_E2AL~ Developmental_stage, data=data)
summary(Anova)
Interaction<-TukeyHSD(Anova)
print(Interaction)

## Group letters for graph
tukeylabel <- multcompLetters(extract_p(Interaction$Developmental_stage), threshold = 0.05)
print (tukeylabel)

# Turn into data.frame for ggplot
tukeyLabelDF <- as.data.frame(x=tukeylabel$Letters)
colnames(tukeyLabelDF) <- c('Marks')                   ##set column names
tukeyLabelDF <- transform(tukeyLabelDF, Developmental_stage=row.names(tukeyLabelDF)) #add Developmental stage labes for ordering

# Get means for plotting
library(plyr)
library(reshape2)

# Melt data frame; get means in a new data frame. SPECIFY VARIABLES!
melted <- melt(data, id.vars=c("Developmental_stage"))

means <- ddply(melted, c("Developmental_stage"), summarise,
               Mean=mean(value))

## Add SD to the matrix
means.sd <- ddply(melted, c("Developmental_stage"), summarise,
                  Mean=mean(value), SD=sd(value))

## Labels for post-hoc test matching order in data frame
tukeyLabelDF <- tukeyLabelDF[match(means.sd$Developmental_stage, tukeyLabelDF$Developmental_stage),]

## Add values for error bars to the matrix
means.sd <- transform(means.sd, lower=Mean-SD, upper=Mean+SD, tukey=tukeyLabelDF$Marks)
print (means.sd)


# Draw boxplot 
b <-ggplot(data, aes(x = Developmental_stage, y = Percentage_E2AL, fill= Developmental_stage)) +
  geom_boxplot(outlier.shape=NA, alpha=0.5, width=0.3) +
  scale_y_continuous(name = "% E2AL",
                     breaks = seq(0,40,5),
                     limits = c(0,40)) +
  scale_x_discrete(name = "Developmental stage") +
  ggtitle("MsOxi activity of OS during larval development of M. sexta") +
  theme_bw() +
  geom_jitter(size=0.8, color= "black", width=0.15)+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE) +
  geom_text(data=means.sd, aes(x=Developmental_stage, y=37.5, label=tukey))
b+theme(legend.position = "none")

