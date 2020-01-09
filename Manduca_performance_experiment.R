## One-way Anova to compare MsOxi activity of OS from 5th instar M.sexta WT/HT/KO larvae 

## Open packages
library(readxl)
library(ggplot2)
library(ggpubr)
library(multcompView)
library(plyr)
library(reshape2)
library(dplyr)

## Load data
Juliette_data_I <- read_excel("surfdrive/Lab/R/Manduca/Manduca_performance_experiment/Data/Juliette_data_I.xlsx", 
                              na = "NA")
View(Juliette_data_I)

## Give dataset a shorter name
data <- Juliette_data_I

## View structure of dataset
str(data)

## Make variable with Genotype_CP as categorical variable
Genotype_CP <- as.factor(data$Genotype_CP)

## Control
class(Genotype_CP)
## If OK, it will say "factor"

# Specify levels in factor Genotype_CP
Genotype_CP <- factor(Genotype_CP, levels=c("WT", "KO"))
levels(data$Genotype_CP)

# Note that the levels are not in a meaningful order
# Change the order to something useful
data$Genotype_CP <- factor(data$Genotype_CP, levels = data$Genotype_CP[c("WT","KO")])
levels(data$Genotype_CP)
Genotype_CP

data$Genotype_CP <- Genotype_CP
Genotype_CP

## Make variable with Genotype_Plant as categorical variable
Genotype_Plant <- as.factor(data$Genotype_Plant)

## Control
class(Genotype_Plant)
## If OK, it will say "factor"

# Specify levels in factor Genotype_Plant
Genotype_Plant <- factor(Genotype_Plant, levels=c("WT", "irLox2", "irLox2/3", "irLox3", "irPMT"))
levels(data$Genotype_Plant)

# Note that the levels are not in a meaningful order
# Change the order to something useful
data$Genotype_Plant <- factor(data$Genotype_Plant, levels = data$Genotype_Plant[c("WT","irLox2", "irLox2/3", "irLox3", "irPMT")])
levels(data$Genotype_Plant)
Genotype_Plant

data$Genotype_Plant <- Genotype_Plant
Genotype_Plant

## Make variable with Days as categorical variable
Day <- as.factor(data$Day)

## Control
class(Day)
## If OK, it will say "factor"

# Specify levels in factor Day
Day <- factor(Day, levels=c("7", "9", "11"))
levels(data$Day)

data$Day <- Day
Day

# Can we assume normality?
mod1<-lm(Weight ~ Genotype_CP + Genotype_Plant + Day, data=data)
summary(mod1)

plot(mod1)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod1)

qplot(x = Genotype_Plant, y = Weight, facets = ~Genotype_CP, data = data) +
  geom_smooth(method = "lm")

# Anova and Tukey HSD to identify significant differences in variance and where (post hoc doesn't work yet, too little df)
Anova<-aov(Weight~ Genotype_CP*Genotype_Plant*Day, data=data)
summary(Anova)
Interaction<-TukeyHSD(Anova)
print(Interaction)

# Tukey test result on top of boxplot???
## Group letters for graph
extract_p(Interaction)
tukeylabel<-multcompLetters(Interaction$`Genotype_CP:Genotype_Plant:Day`[,4])
print (tukeylabel)

#turn into data.frame for ggplot
tukeyLabelDF <- as.data.frame(x=tukeylabel$Letters)
colnames(tukeyLabelDF) <- c('marks') ##set column names
tukeyLabelDF <- transform(tukeyLabelDF, Genotype_CP=row.names(tukeyLabelDF)) #add genotype labes for ordering

# Get means for plotting
library(plyr)
library(reshape2)

#melt data frame; get means in a new data frame
melted <- melt(data, id.vars=c("Genotype_CP","Genotype_Plant", "Day"), measure.vars= "Weight")

means <- ddply(melted, c("Genotype_CP","Genotype_Plant", "Day"), summarise,
               mean=mean(value), na.rm=TRUE)

##add SD to the matrix
means.sd <- ddply(melted, c("Genotype_CP","Genotype_Plant", "Day"), summarise,
                  mean=mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE) )

#add Tukey combinations
means.sd <- transform(means.sd, combination=paste(means.sd$Genotype_CP,means.sd$Genotype_Plant,means.sd$Day, sep=":"))
##labels for post-hoc test matching order in data frame
tukeyLabelDF <- tukeyLabelDF[match(means.sd$combination, tukeyLabelDF$Genotype),]

##add values for error bars to the matrix
means.sd <- transform(means.sd, lower=mean-SD, upper=mean+SD, tukey=tukeyLabelDF$marks)
print (means.sd)
tukey=tukeyLabelDF$marks

## draw boxplot
b <-ggplot(data, aes(x = Genotype_Plant, y = Weight, color=Genotype_CP)) +
  facet_wrap(~Day, labeller = label_both) +
  geom_boxplot(outlier.shape=NA, alpha=0.5) +
  scale_y_continuous(name = "Weight larvae (grams)",
                     breaks = seq(0,0.55,0.1),
                     limits = c(0,0.55)) +
  scale_x_discrete(name = "Genotype Plant") +
  ggtitle("Weight KO and WT larvae feeding on N. attenuata", subtitle= "Molting larvae not included") +
  theme_bw() +                                            # White background
  scale_color_discrete(name = "Genotype CP") +
  geom_point((aes(color=Genotype_CP)), position=position_jitterdodge(), alpha=0.6, size=0.8)+
  geom_text(data =means.sd, 
            aes(x=Genotype_Plant, y=0.55,label=tukey, group=Genotype_CP),
            size = 4, position=position_jitterdodge())
b+theme(legend.position = "bottom")


