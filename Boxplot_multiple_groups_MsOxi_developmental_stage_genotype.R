## One-way Anova and Post-hoc to compare transcript abundance of MsOxi of WT/HT/KO larvae from M.sexta  
## HT cross of KO female x WT male

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
Juliette_data_III <- read_excel("surfdrive/Lab/R/Manduca/Transcript_abundance_MsOxi_genotypes/Data/Juliette_data_III.xlsx", 
                                na = "NA")
View(Juliette_data_III)

# Give shorter name to dataset
data <- Juliette_data_III

# View structure of data
str(data)

# Make variable Gentoype as categorical variable
Genotype <- as.factor(data$Genotype_CP)

## Control
class(Genotype)
## If OK, "factor" is now shown

# Specify levels in factor Genotype
Genotype <- factor(Genotype, levels=c("WT", "HT", "KO"))
levels(data$Genotype)

# Note that the levels are not in a meaningful order
# Change the order to something useful
data$Genotype <- factor(data$Genotype, levels = c("WT","HT", "KO"))
levels(data$Genotype)

## Add new variable to dataset and check variables
data$Genotype <- Genotype
Genotype

# Give variable a shorter name
MsOxi <- data$MsOxi_relative_to_WT_neonates

# Add labelled factor variable called Developmental_stage
# Make variable Genotype as categorical variable
Developmental_stage <- as.factor(data$Developmental_stage)

## Control
class(Developmental_stage)
## If OK, "factor" is now shown

# Specify levels in factor Developmental_stage
Developmental_stage <- factor(Developmental_stage, levels=c("Neonates", "1st_instar", "4th_instar"))
levels(data$Developmental_stage)

# Note that the levels are not in a meaningful order
# Change the order to something useful
data$Developmental_stage <- factor(data$Developmental_stage, levels = c("Neonates","1st_instar", "4th_instar"))
levels(data$Developmental_stage)

## Add new variable to dataset and check variables
data$Developmental_stage <- Developmental_stage
Developmental_stage

# Can we assume normality?
mod1<-lm(MsOxi ~ Genotype + Developmental_stage, data=data)
summary(mod1)

plot(mod1)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod1)
qplot(x = Genotype, y = MsOxi, facets = ~Developmental_stage, data = data) +
  geom_smooth(method = mod1)

# Factorial Anova
# Anova and Tukey HSD to identify significant differences in variance and where (post hoc doesn't work yet, too little df)
Anova<-aov(MsOxi~ Genotype*Developmental_stage, data=data)
summary(Anova)
Interaction<-TukeyHSD(Anova)
print(Interaction)


# Tukey test result on top of boxplot
## Group letters for graph
extract_p(Interaction)
tukeylabel<-multcompLetters(Interaction$`Genotype:Developmental_stage`[,4])
print (tukeylabel)

#turn into data.frame for ggplot
tukeyLabelDF <- as.data.frame(x=tukeylabel$Letters)
colnames(tukeyLabelDF) <- c('marks') ##set column names
tukeyLabelDF <- transform(tukeyLabelDF, Genotype=row.names(tukeyLabelDF)) #add genotype labes for ordering

# Get means for plotting
library(plyr)
library(reshape2)

#melt data frame; get means in a new data frame
melted <- melt(data, id.vars=c("Genotype","Developmental_stage"), measure.vars= "MsOxi_relative_to_WT_neonates")

means <- ddply(melted, c("Genotype","Developmental_stage"), summarise,
               mean=mean(value), na.rm=TRUE)

##add SD to the matrix
means.sd <- ddply(melted, c("Genotype","Developmental_stage"), summarise,
                  mean=mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE) )

#add Tukey combinations
means.sd <- transform(means.sd, combination=paste(means.sd$Genotype,means.sd$Developmental_stage, sep=":"))
##labels for post-hoc test matching order in data frame
tukeyLabelDF <- tukeyLabelDF[match(means.sd$combination, tukeyLabelDF$Genotype),]

##add values for error bars to the matrix
means.sd <- transform(means.sd, lower=mean-SD, upper=mean+SD, tukey=tukeyLabelDF$marks)
print (means.sd)

#draw boxplot
b <-ggplot(data, aes(x = Genotype, y = MsOxi, fill= Genotype)) +
  facet_wrap(~Developmental_stage) +
  geom_boxplot(outlier.shape=NA, alpha=0.5, width=0.5) +
  scale_y_continuous(name = "Ratio MsUbi/ MsOxi\nrelative to WT neonates",
                     breaks = seq(0,4,0.5),
                     limits = c(0,4)) +
  scale_x_discrete(name = "Genotype") +
  ggtitle("Transcript abundance MsOxi of WT, HT and KO larvae during development") +
  theme_bw() +
  geom_jitter(size=0.8, color= "black", width=0.15)+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE)+
  geom_text(data =means.sd, 
            aes(x=Genotype, y=3.8,label=tukey), 
            size = 4,position=position_dodge(.5))
b+theme(legend.position = "none")



