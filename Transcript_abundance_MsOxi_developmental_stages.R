## One-way Anova and Post-hoc to compare transcript abundance of MsOxi during larval development of M.sexta  

## Open packages
library(readxl)
library(ggplot2)
library(ggpubr)
library(datasets)
library(plyr)
library(dplyr)
library(multcompView)
library(reshape2)

# View data
Juliette_data_IV <- read_excel("surfdrive/Lab/R/Manduca/Transcript_abundance_MsOxi_developmental_stages/Data/Juliette_data_IV.xlsx", 
                                na = "NA")
View(Juliette_data_IV)

# Give shorter name to dataset
data <- Juliette_data_IV

# View structure of data
str(data)

# Make variable developmental stage as categorical variable
Developmental_stage <- as.factor(data$Developmental_stage)

## Control
class(Developmental_stage)
## If OK, "factor" is now shown

# Specify levels in factor Developmental_stage
Developmental_stage <- factor(Developmental_stage, levels=c("Eggs", "Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", "Day8", "Day9", "Day10", "Day11", "Day12", "Day13", "Day14", "Day15"))
levels(data$Developmental_stage)

## Add new variable to dataset and check variables
data$Developmental_stage <- Developmental_stage
Developmental_stage

# Give variable a shorter name
MsOxi <- data$Ratio_MsOxi_relative_to_eggs

# Can we assume normality?
mod1<-lm(MsOxi ~ Developmental_stage, data=data)
summary(mod1)

plot(mod1)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod1)
qplot(x = Developmental_stage, y = MsOxi, data = data) +
  geom_smooth(method = mod1)

# Factorial Anova
# Anova and Tukey HSD to identify significant differences in variance and where
Anova<-aov(MsOxi~Developmental_stage, data=data)
summary(Anova)
Interaction<-TukeyHSD(Anova)
print(Interaction)

# Tukey test result on top of boxplot
## Group letters for graph
tukeylabel <- multcompLetters(extract_p(Interaction$Developmental_stage), threshold = 0.05)
print (tukeylabel)

# Turn into data.frame for ggplot
tukeyLabelDF <- as.data.frame(x=tukeylabel$Letters)
colnames(tukeyLabelDF) <- c('Marks')                   ##set column names
tukeyLabelDF <- transform(tukeyLabelDF, Developmental_stage=row.names(tukeyLabelDF)) #add Genotype labels for ordering

# Get means for plotting
library(plyr)
library(reshape2)

# Melt data frame; get means in a new data frame. SPECIFY VARIABLES!
melted <- melt(data, id.vars=c("Developmental_stage"))

means <- ddply(melted, c("Developmental_stage"), summarise,
               Mean=mean(value, na.rm = TRUE))

## Add SD to the matrix
means.sd <- ddply(melted, c("Developmental_stage"), summarise,
                  Mean=mean(value, na.rm = TRUE), SD=sd(value, na.rm = TRUE))

##order data.frame for plotting
##labels for post-hoc test matching order in data frame
tukeyLabelDF <- tukeyLabelDF[match(means.sd$Developmental_stage, tukeyLabelDF$Developmental_stage),]

##add values for error bars to the matrix
means.sd <- transform(means.sd, lower=mean-SD, upper=mean+SD, tukey=tukeyLabelDF$'Marks')
print (means.sd)

tukey=tukeyLabelDF$Marks

##order data.frame for plotting
means$Developmental_stage <- factor(means$Developmental_stage, levels=c("Eggs", "Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", "Day8", "Day9", "Day10", "Day11", "Day12", "Day13", "Day14", "Day15"))


#Draw boxplot 
b <-ggplot(data, aes(x = Developmental_stage, y = MsOxi, fill= Developmental_stage)) +
  geom_boxplot(outlier.shape=NA, alpha=0.5, width=0.5) +
  scale_y_continuous(name = "Ratio MsUbi/ MsOxi\nrelative to eggs",
                     breaks = seq(0,1050,100),
                     limits = c(0,1050)) +
  scale_x_discrete(name = "Developmental stage") +
  ggtitle("Transcript abundance MsOxi during larval developmental of M.sexta") +
  theme_bw() +
  geom_jitter(size=0.8, color= "black", width=0.15)+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE) +
  geom_text(data=means.sd, aes(x=Developmental_stage, y=1035, label=tukey))
b+theme(legend.position = "none")

