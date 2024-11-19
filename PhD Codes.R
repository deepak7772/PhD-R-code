#Upload library#
library(nlme)
library(lme4)
library(lmerTest)
library(LMERConvenienceFunctions)
library(plyr)
library(nnet)
library(nloptr)
library(lsmeans)
library(car)
library(energy)
library(buildmer)
library(tidyverse)
library(ordinal)
library(Rmisc)
library(scales)
library(readxl)
library(dplyr)


#Let's subset the data ###

Mainsd <- subset(df_2, language == "S")

Mainsd$reflevel <- paste0(Mainsd$referend, Mainsd$level)
Mainhd$item2<- NULL

#Let's add the items for each particpant#

Mainhd$item2 <- rep(1:48, length.out = nrow(Mainhd))
Mainsd$item2 <- rep(1:48, length.out = nrow(Mainsd))

#Rename the items#
colnames(Mainhd)[colnames(Mainhd) == "item2"] ="item"
colnames(Mainsd)[colnames(Mainsd) == "item2"] ="item"
colnames(df_2)[colnames(df_2) == "English Profieicny"] ="English Proficiency"
colnames(Mainsd)[colnames(Mainsd) == "item2"] ="item"


##Let's first convert our 0 to NA for Spanish##
Mainsd$response[Sd$response == 0] <- NA

Mainsd <- subset(Mainsd, response != 0)

##Let's delete all NA###
Mainsd <- Mainsd[!is.na(Mainsd$response), ]

# Separate data into languages#

Hd <- working_file[working_file$language ==  "H", ]
Sd <-working_file[working_file$language ==  "S", ]


##Let's first convert our 0 to NA for Hindi##
Hd$response[Hd$response == 0] <- NA

##Let's delete all NA###
Mainhd <- Hd[!is.na(Hd$response), ]

Mainhd$response <- as.ordered(Mainhd$response)
#Let's merge our three columns together#
Mainhd$merg_col<-paste0(Mainhd$condition, Mainhd$referend, Mainhd$level)

##Let's first convert our 0 to NA for Spanish##
Sd$response[Sd$response == 0] <- NA

##Let's delete all NA###
Mainsd <- Sd[!is.na(Sd$response), ]
Mainsd$response <- as.ordered(Mainsd$response) 

#Let's merge our three columns together#

Mainsd$merg_col<-paste0(Mainsd$condition, Mainsd$referend, Mainsd$level)

#Let's remove the first letter from the columns#
Mainsd$merg_col <- substr(Mainsd$merg_col, 2, nchar(Mainsd$merg_col))

#Rename the colums Metalinguistic Results#

colnames(df_2)[colnames(df_2) == "Institution"] ="Inst"
colnames(df_2)[colnames(df_2) == "Year of your study"] ="YOS"
colnames(df_2)[colnames(df_2) == "Spanish Lextale"] ="Slextale"
colnames(df_2)[colnames(df_2) == "English Lextale"] ="Elextale"
colnames(df_2)[colnames(df_2) == "Metalinguistic Results"] ="Meta"
colnames(Mainhd)[colnames(Mainhd) == "Year of your study"] ="YOS"
colnames(Mainsd)[colnames(Mainsd) == "Year of your study"] ="YOS"
colnames(Mainsd)[colnames(Mainsd) == "Institution"] ="Inst"
colnames(Mainhd)[colnames(Mainhd) == "Institution"] ="Inst"
colnames(Mainhd)[colnames(Mainhd) == "Spanish Lextale"] ="Slextale"
colnames(Mainhd)[colnames(Mainhd) == "English Lextale"] ="Elextale"
colnames(Mainsd)[colnames(Mainsd) == "Spanish Lextale"] ="Slextale"
colnames(Mainsd)[colnames(Mainsd) == "English Lextale"] ="EP"
colnames(Mainhd)[colnames(Mainhd) == "Metalinguistic Results"] ="Meta"
colnames(Mainsd)[colnames(Mainsd) == "Metalinguistic Results"] ="Meta"
colnames(Sd2)[colnames(Mainsd) == "English Lextale"] ="Elextale"
colnames(Mainsd)[colnames(Mainsd) == "Institution"] ="Inst"
colnames(Mainsd)[colnames(Mainsd) == "Elextale"] ="English Proficiency"

colnames(Hd2)[colnames(Hd2) == "referend"] ="ref"

Hd2$reflevel<-paste0(Hd2$ref,Hd2$level)
#Lets seprate 2nd and 3rd year data by languages#
#Let's seperate Hindi 2nd year data#

Hd2<-filter(Mainhd,YOS=="2nd Year", language=="H")

#Let's seperate Hindi 3rd year data#

Hd3<-filter(Mainhd,YOS=="3rd Year", language=="H")

#Create a new datase##
df_2<-arrangedData

df_2$response[df_2$response == 0] <- NA

df_2 <- df_2[!is.na(df_2$response), ]
##Let's delete all NA###
Mainhd <- Hd[!is.na(Hd$response), ]

#Now let's separate the data for Spanish#

Sd2<-filter(Mainsd,YOS=="2nd Year", language=="S")

Sd3<-filter(Mainsd,YOS=="3rd Year", language=="S")


#Change to factor/numer as required#

df_2$response<-factor(df_2$response)
df_2$Meta<-as.numeric (df_2$`Meta)
df_2df_2$`English Lextale`<-as.numeric (df$`English Lextale`)
df_2$Institution<-as.factor (df_2$Institution)
df_2$`Year of your study`<-as.factor (df_2$`Year of your study`)
df_2$Gender<-as.factor (df_2$Gender)
df_2$condition<-as.factor (df_2$condition)
df_2$referend<-as.factor (df_2$referend)
df_2$level<-as.factor (df_2$level)
df_2$Id<-as.factor (df_2$Id)



#######################  HINDI LANGUAGE DATA ANALYSIS  ####################

#Let's club referend, condition, level and response coulmn to visualize the results#


# Ensure the dplyr and ggplot2 packages are installed and loaded
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Convert the Response column to numeric if it's not already
Mainhd$Response <- as.numeric(as.character(Mainhd$Response))

# Calculate the mean and standard deviation for each condition and level
summary_stats <- Mainhd %>%
  group_by(Condition, Level) %>%
  summarise(
    AverageRating = mean(Response, na.rm = TRUE),
    StandardDeviation = sd(Response, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)

# Create the bar graph
ggplot(summary_stats, aes(x = Condition, y = AverageRating, fill = Level)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = AverageRating - StandardDeviation, ymax = AverageRating + StandardDeviation),
                position = position_dodge(0.6), width = 0.2) +
  geom_text(aes(label = round(AverageRating, 2)), 
            position = position_dodge(0.6), 
            vjust = -0.5) +
  labs(title = "Average Acceptability Ratings by Condition and Level",
       x = "Condition",
       y = "Average Rating (± SD)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

summarise(summary_stats)


# Plot showing Mean Ratings by Proficiency and Condition
MeanRatingProfGraph <- ggplot(Mainsd, aes(x=ENglish Proficiency, y=mean_ratings, color=Condition)) +
  geom_point(shape=19, size=2, position=position_jitter(h=0, w=0.1)) + 
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Mean Ratings by Proficiency \nand Condition") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.2)) +
  xlab("Proficiency Score") + 
  ylab("Mean Rating") +
  scale_y_continuous(breaks=seq(1, 4, 0.5), limits=c(0.8, 4.2), expand=c(0, 0), oob=rescale_none) +
  scale_color_manual(values=c("blue","deeppink2","mediumseagreen","deepskyblue", "pink", "seagreen2"), 
                     name="Condition", 
                     labels=c("Condition 1", "Condition 2", "Condition 3", "Condition 4", "Condition 5", "Condition 6")) +
  theme(plot.title = element_text(vjust = 5, size=20), 
        axis.title.x = element_text(vjust = -2, size=18), 
        axis.title.y = element_text(vjust = 5, size=18),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"),
        legend.position = c(0.16, 0.12)) +
  guides(col = guide_legend(ncol=2, byrow=FALSE))

print(MeanRatingProfGraph)


##########Mean Rating visualization with Mean and SD#################

library(ggplot2)
library(RColorBrewer)  # For the scale_fill_brewer function

max_rating <- max(summary_stats$AverageRating + summary_stats$StandardDeviation, na.rm = TRUE)
min_rating <- min(summary_stats$AverageRating - summary_stats$StandardDeviation, na.rm = TRUE)

# Create an enhanced bar graph with adjusted y-axis limits
ggplot(summary_stats, aes(x = Condition, y = AverageRating, fill = Level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = AverageRating - StandardDeviation, ymax = AverageRating + StandardDeviation),
                position = position_dodge(width = 0.7), width = 0.25) +
  geom_text(aes(label = sprintf("%.2f", AverageRating)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black", fontface = "bold", 
            check_overlap = TRUE) +  # Prevent text overlap
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Average Acceptability Ratings by Condition and Level",
       x = "Condition",
       y = "Mean and SD",
       fill = "Level") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Increased title size
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.2, color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(min_rating - 0.5, max_rating + 0.5))  # Adjust y-axis limits

###################################################################################################
#Let's analysze the data for Hindi 2nd year first#

#################################################################################################


# Create a new variable for the facet labels
H2ndyear$condition_label <- factor(paste(H2ndyear$referend, H2ndyear$condition))

# Define a colorful palette
colors <- brewer.pal(4, "Set1")

# Plot using ggplot2 with facet_wrap
ggplot(H2ndyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  facet_wrap(~condition, scales = "free_x", ncol = 2) +
  labs(title = "2nd Year (Hindi)",
       x = "ACCEPTABILITY JUDGEMENT TASK", y = "%", fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("black", "lightgreen", "blue", "red"))

scale_fill_brewer(palette = "Set4")


library(dplyr)

# Assuming 'your_data' is your data frame and 'old_condition_column' is the column with the old condition names
H2ndyear <- H2ndyear %>%
  mutate(condition = recode(condition, 
                            "C4" = "Dubitative",
                            "C3" = "Factual",
                            "C1" = "Specific Referent",
                            "C2" = "Non-specific Referent"))

########################################################################################################
#Let's analyse the data for Hindi 3rd year#
#######################################################################################################

#Plotting 3rd Year Hindi results#
# Create a new variable for the facet labels
H3rdyear$condition_label <- factor(paste(H3rdyear$referend, H3rdyear$condition))

# Define a colorful palette
colors <- brewer.pal(4, "Set1")

ggplot(H3rdyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~condition, scales = "free_x", ncol = 2) +
  labs(title = "3rd Year (Hindi)",
       x = "ACCEPTABILITY JUDGEMENT TASK", y = "%", fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "brown", "red", "yellow"))
scale_fill_brewer(palette = "Set1")


# Assuming 'your_data' is your data frame and 'old_condition_column' is the column with the old condition names
H3rdyear <- H3rdyear %>%
  mutate(condition = recode(condition, 
                            "C4" = "Dubitative",
                            "C3" = "Factual",
                            "C1" = "Specific Referent",
                            "C2" = "Non-specific Referent"))    
#############################################################################################

##Let's combine our Spanish data and check the overall results#

#############################################################################################

####After calculating the mean rating, visualize data with all the conditions####

library(ggplot2)
library(RColorBrewer)  # For the scale_fill_brewer function


# Calculate maximum and minimum ratings for y-axis limits
max_rating <- max(summary_stats$AverageRating + summary_stats$StandardDeviation, na.rm = TRUE)
min_rating <- min(summary_stats$AverageRating - summary_stats$StandardDeviation, na.rm = TRUE)

# Create an enhanced bar graph with adjusted y-axis limits and professional colors
ggplot(summary_stats2, aes(x = Condition, y = AverageRating, fill = level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = AverageRating - StandardDeviation, ymax = AverageRating + StandardDeviation),
                position = position_dodge(width = 0.7), width = 0.25) +
  geom_text(aes(label = sprintf("%.2f", AverageRating)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black", fontface = "bold", 
            check_overlap = TRUE) +  # Prevent text overlap
  scale_fill_manual(values = c("#6c6ebf", "#a9a9a9")) +  # Softer colors
  labs(title = "Mean Ratings by Condition and Level",
       x = "Condition",
       y = "Mean and SD",
       fill = "Level") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Increased title size
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(min_rating - 0.5, max_rating + 0.5))  # Adjust y-axis limits




library(dplyr)

Ssummary <- Mainsd %>%
  dplyr::group_by(referend, condition, level, response) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>% 
  
  # order the levels of response manually so that the order is not alphabetical
  dplyr::mutate(response = factor(response, 
                                  levels = 1:4,
                                  labels = c("Totally unacceptable", 
                                             "Unacceptable", 
                                             "Acceptable", 
                                             "Totally Acceptable")))



##Let's visualize this###

#########################################################################  

ggplot(Ssummary, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~referend*condition) +
  labs(title = "Spanish AJT Responses",
       x = "Level", y = "%", fill = "Responses") +
  theme_minimal() + scale_fill_brewer(palette = "Set1")


########################################################
#Let's analyse the data for Spanish 2nd year first#

library(dplyr)

S2ndyear <- Sd2 %>%
  dplyr::group_by(referend, condition, level, response) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>% 
  
  # order the levels of response manually so that the order is not alphabetical
  dplyr::mutate(response = factor(response, 
                                  levels = 1:4,
                                  labels = c("Totally Unacceptable", 
                                             "Unacceptable", 
                                             "Acceptable", 
                                             "Totally Acceptable")))

##Let's visualize this###

ggplot(S2ndyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~referend*condition) +
  labs(title = "2nd Year Spanish AJT",
       x = "Level", y = "%", fill = "Response") +
  theme_minimal() + scale_fill_brewer(palette = "Set1")

######################################################################
#Let's analyse the data for Spanish 3rd year#

library(dplyr)

S3rdyear <- Sd3 %>%
  dplyr::group_by(referend, condition, level, response) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>% 
  
  
  
  # order the levels of response manually so that the order is not alphabetical
  dplyr::mutate(response = factor(response, 
                                  levels = 1:4,
                                  labels = c("Totally Unacceptable", 
                                             "Unacceptable", 
                                             "Acceptable", 
                                             "Totally Acceptable")))

##Let's visualize this###

plot3<-ggplot(S3rdyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~referend*condition) +
  labs(title = "3rd Year Spanish AJT",
       x = "Level", y = "%", fill = "Response") +
  theme_minimal() + scale_fill_brewer(palette = "Set1")


###################################################################################################
#Let's plot them together#

library(ggplot2)
library(cowplot)
combined_plot <- plot_grid(plot3, plot2, ncol = 2)

print(combined_plot)

#Let's compare the Spanish reults from 2nd and 3rd year through boxplot#


boxplot(Sd2$response,Sd3$response, main= "Acceptability Judgement Task Responses",
        xlab="Comparasion",names=c("Spanish 2nd Year","Spanish 3rd Year"),vertical= TRUE,col="gold")


#Let's check the distribution of metalinguistic# 

shapiro.test(Hd$Meta) #It's normally distributed#

shapiro.test(Mainhd$Elextale)

summary(Mainsd$`Spanish Proficiency`)

summary(Mainsd$`Metalinguistic Knowledge`)

sd(Mainsd$`Metalinguistic Knowledge`)
hist(Mainhd$Elextale)
hist(Mainhd$Meta)

#Calculate Mean & Standard deviation for Hindi#

sd(Hd$Meta)
1.582051#
mean(Hd$Meta)
2.915081#


################################Spanish AJT Analysis########################


# Ensure that the Response variable is numeric
Mainsd$Response <- as.numeric(as.character(Mainsd$Response))

# Now calculate the summary statistics
summary_stats2 <- Mainsd %>%
  group_by(Condition, level) %>%
  summarise(
    AverageRating = mean(Response, na.rm = TRUE),
    StandardDeviation = sd(Response, na.rm = TRUE)
  )

#Let's club referend, condition, level and response coulmn to visualize the results#

library(dplyr)

#Let's remove NA first#

Sd <- na.omit(Sd)

Ssummary <- Sd %>%
  dplyr::group_by(referend, condition, level, response) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  
  # order the levels of response manually so that the order is not alphabetical
  dplyr::mutate(response = factor(response, 
                                  levels = 1:4,
                                  labels = c("Totally Unacceptable", 
                                             "Unacceptable", 
                                             "Acceptable", 
                                             "Totally Acceptable")))

##Let's visualize this beauty###

library(ggplot2)


ggplot(Ssummary, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~referend*condition) +
  labs(title = "Spanish Overall AJT Responses",
       x = "Level", y = "%", fill = "Response") +
  theme_minimal()


#Let's check through histogram if proficiency in Eng & Span distribution is normal or not$

boxplot(Hd$Elextale,Sd$response, main= "Acceptability Judgement Task",
        xlab="Comparasion",names=c("English Proficiency","Spanish Proficiency"),vertical= TRUE,col="gold")
library(ggplot2) 


#Let's check Eng Prof data#

hist(Sd$Elextale, main="English Lextale Score",xlab = "Score", ylab ="Count") # This looks normally distributed#

#Let's check the mean#

mean
16.93333

summary(Sd$Elextale)

#Let's check Spa Prof Data#

hist(Sd$Slextale, main="Spanish Lextale Score",xlab = "Score", ylab ="Count") #this looks skewed$-

#Let's calculate our median as the data is skewed#

median(Sd$Slextale)
30

#Let's check the overall summary#
summary(Sd$Slextale)

#Let's calculate their mean and standard deviation# '

mean(Sd$Slextale)
6.111111

sd(Sd$Slextale)
11.57184

mean(Sd$Elextale)
6.111111

sd(Sd$Elextale)
10.3214


#Let's run a corelation matrix -positive corelation between EnglLex & Meta#

CoSE<-df_2[7:9]
cor(CoSE)
library(corrplot)
corrplot(cor(CoSE),)


# Assuming df_2 is your dataframe
CoSE <- df_2[7:9]

# Calculate the correlation matrix
cor_matrix <- cor(CoSE)

# Set the layout with a larger size
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

# Plot the correlation matrix using corrplot
library(corrplot)
corrplot(cor_matrix, method = "number", type = "upper", diag = FALSE, tl.cex = 0.8)

Mainsd$merge <- NULL

Mainsd$reflevel <- paste0(Mainsd$referend, Mainsd$level)

# Assuming 'df_2' is your dataframe and 'CoSE' contains columns 7 to 9
# Assuming 'df_2' is your dataframe and 'CoSE' contains columns 7 to 9)


##Let's plot to check the corelation betwen Spanish and English Proficiencyy#

plot(arrangedData$`English Lextale`,arrangedData$`Spanish Lextale`)

#Let's change variables into factors#

Hd$response<-factor(Hd$response)
Hd$Meta<-as.numeric (Hd$`Meta)
Hd$ELextale<-as.numeric (Hd$ELextale)
Hd$Inst<-as.factor (Hd$Inst)
Hd$Institution<-as.factor (Hd$Institution)
Hd$Gender<-as.factor (Hd$Gender)
Hd$condition<-as.factor (Hd$condition)
Hd$referend<-as.factor (Hd$referend)
Hd$level<-as.factor (Hd$level)
Hd$Id<-as.factor (Hd$Id)
Mainhd$Inst<-as.factor (Mainhd$Inst)
Mainhd$Meta<-as.factor (Mainhd$Meta)
Mainhd$Elextale<-as.factor (Mainhd$Elextale)
Mainhd$response<-as.factor (Mainhd$response)


#Let's save our Hindi dataset as a seprate file#
write.csv(Mainhd, file = "C:/Users/21252854/OneDrive - Maynooth University/Documents/Practice/Mainhd.csv", row.names = FALSE)


library(ggplot2)
ggplot(Mainhd, aes(x = Elextale, y = Meta, fill = response)) +
  geom_tile() +
  theme_minimal()

##Let's use a scatter plot to visualize the interaction between Eng prof & Meta##
ggplot(Mainhd, aes(x = Meta, y = Elextale)) +
  geom_point() +
  labs(x = "Meta", y = "Elextale", title = "Meta and Eng Prof. Relationship")


##Let's add a line to the scatter plot to see if the relationship is strong##
ggplot(Mainhd, aes(x = Meta, y = Elextale)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add a blue loess line
  labs(x = "Meta", y = "Elextale", title = "Meta and Eng Prof. Relationship")


# Make sure 'level' is a factor
Mainhd$level <- factor(Mainhd$level, levels = c("IND", "SUB"), ordered = FALSE)

# Relevel the 'level' column
Mainhd$level <- relevel(Mainhd$level, ref = "IND")

colnames(Mainhd)[colnames(Mainhd) == "merg_col"] ="NC"
colnames(Mainsd)[colnames(Mainsd) == "merg_col"] ="NC"


#Let's run some corelation test##

#Let's run a corelation test between categorica variables#

chisq.test(table(Mainhd$Inst, Mainhd$YOS)) # There is a corelation between Inst and YOS#

chisq.test(table(Mainhd$Elextale, Mainhd$YOS, method = "spearman"))# Elextale and YOS are corelated#

#Let's run the Shapito .test to check if the data distribution is normal or not normal#

shapiro.test(Mainsd$Slextale) # Not normally distributied#


#Let's check the correlation between meta and Slextale through covariabce test#
#Covariance tells us the direction of the relationship--positive# ([1] 1.903671)

cor(Mainsd$Slextale, Mainsd$Elextale)


cor.test(Mainsd$Slextale, Mainsd$Meta) #positive corelation (0.1464483)

Mainsd$Slextale <- as.numeric(Mainsd$Slextale)
Mainsd$Meta <- as.numeric(Mainsd$Meta)
#Let's check the same for Elextale and Slextale#It's positive(7.713034) and meand when Elextale increases Slex increases as well#

cor(Mainsd$Slextale, Mainsd$'English Proficiency')
cor.test(Mainsd$Slextale, Mainsd$'English Proficiency') #positive corelation.However, the correlation is relatively low  (0.08966605 )


# Compute the correlation coefficient
# Compute the correlation coefficient using backticks for column names with spaces
correlation <- cor(Mainsd$`English Proficiency`, Mainsd$Response, use = "complete.obs")
print(correlation)

# Perform correlation test to see if this is statistically relevent or not###
correlation_test <- cor.test(Mainsd$`English Proficiency`, Mainsd$Response)

# Print the results
print(correlation_test)
# Load necessary libraries
library(ggplot2)

# Scatter plot with regression line
ggplot(Mainsd, aes(x = `English Proficiency`, y = Response)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red") + # Regression line
  labs(title = "Correlation between English Proficiency and AJT Response",
       x = "English Proficiency",
       y = "AJT Response") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


# Calculate correlation coefficient
correlation <- cor(Mainsd$`English Proficiency`, Mainsd$Response, use = "complete.obs")

# Scatter plot with correlation coefficient
ggplot(Mainsd, aes(x = `English Proficiency`, y = Response)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red") + # Regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("r = %.2f", correlation), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 5) +
  labs(title = "Correlation between English Proficiency and AJT Response",
       x = "English Proficiency",
       y = "AJT Response") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Print the correlation coefficient
print(correlation)


#####Calculate the des stats for EP###

summary(Mainsd$`English Proficiency`)

#Let's check the average rating#

ggplot(Mainsd, aes(x = Condition, y = Response, fill = referend)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.7) +
  labs(title = "Average Ratings by Participants for Each Condition",
       x = "Condition",
       y = "Average Rating",
       fill = "Level") +
  theme_minimal()


##Let's creat a descriptive statistics for HIndi#

# Install and load necessary packages if not already installed
install.packages(c("dplyr", "knitr"))
library(tidyr)
library(dplyr)
library(knitr)
Hd2summary <- Hd2 %>%
  group_by(condition,
           level, response) %>%
  summarise(Count = n()) %>%
  group_by(condition, level) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  select(condition, level, response, Percentage)

# Reshape the data to have Response as columns
Hwide_table <- spread(Hd2summary, key = response, value = Percentage)
kable(Hwide_table, caption = "Descriptive Statistics for Hindi AJT Responses")


##Let's visualize this descripitve data###
library(ggplot2)

# Your ggplot code here
plot2 <- ggplot(H2ndyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_grid(~condition, scales = "free_x", switch = "x") +
  labs(title = "2nd Year (AJT in Hindi)",
       x = "",  # Set x to empty string
       y = "%", fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "black", "green", "purple"))

# Combine facet labels, legend, and title customizations
plot +
  theme(strip.text.x = element_text(size = 12, color = "blue", face = "italic"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",  # Adjust the position as needed
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5))


cor(Mainsd$Meta,Mainsd$Elextale
)
library(rafa)
install.packages("swirl")
library(swirl)
cor.test(Mainsd$Meta,Mainsd$Elextale)

cor.test

summary(Mainsd$Meta)


##Let's create a mean rating for the second year Hindi and plot it##



# Create a more appealing grouped bar plot with legend at the bottom and customized y-axis label

mean_ratings_modified <- mean_ratings %>%
  mutate(condition = ifelse(condition == "C1", "Specific Referent", 
                            ifelse(condition == "C2", "Non-specific Referent", 
                                   ifelse(condition == "C3", "Factual", 
                                          ifelse(condition == "C4", "Dubitative", condition)))))

# Create a more appealing grouped bar plot with legend at the bottom and customized y-axis label
ggplot(mean_ratings_modified, aes(x = condition, y = mean_rating, fill = level, color = level)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_text(aes(label = sprintf("%.2f", mean_rating)),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5, color = "black", family = "Arial") +
  labs(title = "Mean Ratings by 2nd Year Students",
       x = "",
       y = "Response Rating") +
  scale_fill_manual(values = c("black", "yellow"), name = "Level") +
  scale_color_manual(values = c("purple", "red"), name = "Level") +  # Adjusted color values
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial", color = "black", size = 12),
        plot.title = element_text(family = "Arial", size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(color = "blue", size = 12, face = "bold"),  # Change x-axis label color
        axis.text.y = element_text(color = "black"))  # Change y-axis label color



##Let's create a mean rating for the third year Hindi and plot it##

Condition             Level          1         2         3         4
Specific_Referent     IND    11.111111  17.54386  39.76608  31.57895
Specific_Referent     SUB    21.414538  26.71906  29.86248  22.00393
Non_specific_Referent IND    20.432220  28.29077  31.23772  20.03929
Non_specific_Referent SUB     8.823529  17.64706  41.37255  32.15686
Factual               IND     6.407767  14.75728  44.27184  34.56311
Factual               SUB    24.505929  29.05138  27.66798  18.77470
Dubitative            IND    21.936759  26.87747  31.62055  19.56522
Dubitative            SUB     8.023483  18.78669  43.05284  30.13699



##Let's visualize this in an effiencient manner#

library(ggplot2)

# Your ggplot code here
plot <- ggplot(H3rdyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_grid(~condition, scales = "free_x", switch = "x") +
  labs(title = "3rd Year (AJT in Hindi)",
       x = "",  # Set x to empty string
       y = "%", fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "black", "blue", "yellow"))

# Combine facet labels, legend, and title customizations
plot +
  theme(strip.text.x = element_text(size = 12, color = "blue", face = "italic"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",  # Adjust the position as needed
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5))



#Let first check the class of response as it needs to be numeric to calculate mean#
class(Hd3$response)

#Let's convert it as numeric#
Hd3$response <- as.numeric(as.character(Hd3$response))

#Now let's calculate mean rating and plot it#


library(dplyr)

# Using dplyr package
mean_rating <- Hd3 %>%
  group_by(condition,level) %>%
  summarise(mean_rating = mean(response))  %>%  
  
  
  mutate(condition = ifelse(condition == "C1", "Specific Referent", 
                            ifelse(condition == "C2", "Non-specific Referent", 
                                   ifelse(condition == "C3", "Factual", 
                                          ifelse(condition == "C4", "Dubitative", condition)))))

# Create a more appealing grouped bar plot with legend at the bottom and customized y-axis label
ggplot(mean_rating, aes(x = condition, y = mean_rating, fill = level, color = level)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_text(aes(label = sprintf("%.2f", mean_rating)),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5, color = "black", family = "Arial") +
  labs(title = "Mean Ratings by 3rd Year Students",
       x = "",
       y = "Response Rating") +
  scale_fill_manual(values = c("green", "blue"), name = "Level") +
  scale_color_manual(values = c("purple", "red"), name = "Level") +  # Adjusted color values
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial", color = "black", size = 12),
        plot.title = element_text(family = "Arial", size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(color = "blue", size = 12, face = "bold"),  # Change x-axis label color
        axis.text.y = element_text(color = "black"))  # Change y-axis label color



##Let's also calculate the descriptive statistics for Spanish#

##Descriptive statistics for Spanish data second year#
library(dplyr)
library(knitr)
percentage <- Sd2 %>%
  group_by(condition, level, response) %>%
  summarise(Count = n()) %>%
  group_by(condition, level) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  select(condition, level, response, Percentage)

# Reshape the data to have Response as columns
table3 <- spread(percentage, key = response, value = Percentage)
kable(table3, caption = "Descriptive Statistics for Spanish AJT Responses")

Table: Descriptive Statistics for Spanish AJT Responses

|condition |level |        1|        2|        3|        4|
  |:---------|:-----|--------:|--------:|--------:|--------:|
  |C1        |IND   | 15.94203| 16.90821| 38.64734| 28.50242|
  |C1        |SUB   | 16.82692| 21.63462| 40.86538| 20.67308|
  |C2        |IND   | 14.56311| 17.96117| 45.63107| 21.84466|
  |C2        |SUB   | 17.22488| 24.40191| 44.97608| 13.39713|
  |C3        |IND   | 15.65657| 19.19192| 45.45455| 19.69697|
  |C3        |SUB   | 16.33663| 20.79208| 40.09901| 22.77228|
  |C4        |IND   | 15.76355| 27.09360| 39.90148| 17.24138|
  |C4        |SUB   | 17.08543| 26.13065| 37.18593| 19.59799|
  
  
  
  
  
  # Boxplot for English Proficiency
  # Boxplot for English Proficiency with scale up to 40
  boxplot(Mainsd$`English Proficiency`,
          main = "Boxplot of English Proficiency Scores",
          ylab = "Scores",
          col = "lightblue",
          border = "darkblue",
          horizontal = FALSE,
          ylim = c(0, 40))  # Set y-axis limits from 0 to 40



#Let's modify our condition name first so it looks good#

library(dplyr)

S2ndyear <- S2ndyear %>%
  mutate(condition = case_when(
    condition == "C1" ~ "Specific Referent",
    condition == "C2" ~ "Non-specific Referent",
    condition == "C3" ~ "Factual",
    condition == "C4" ~ "Dubitative",
    TRUE ~ condition  # Keep the original value if none of the conditions are met
  ))

rm(plot3)
##Let's visualize it#
ggplot(S2ndyear, aes(x = level, y = Percent, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_grid(~condition, scales = "free_x", switch = "x") +
  labs(title = "2nd Year (AJT in Spanish)",
       x = "",  # Set x to empty string
       y = "%", fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "black", "red", "yellow")) +
  
  # Combine facet labels, legend, and title customizations
  
  theme(strip.text.x = element_text(size = 12, color = "blue", face = "italic"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",  # Adjust the position as needed
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5))

#Let's calculate the mean rating for Spanish 2nd Year#

library(dplyr)

# Using dplyr package
library(dplyr)

# Assuming your original data frame is named 'your_original_data'
S2mean_rating <- Sd2 %>%
  group_by(Condition, level) %>%
  summarise(response = mean(Response, na.rm = TRUE)) %>%
  ungroup()



# Assuming your data frame is properly defined
ggplot(S2mean_rating, aes(x = Condition, y = response, fill = level, color = level)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_text(aes(label = sprintf("%.2f", response)),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5, color = "black", family = "Arial") +
  labs(title = "Mean Ratings by 2nd Year Students",
       x = "",
       y = "Response Rating") +
  scale_fill_manual(values = c("magenta", "black"), name = "Level") +
  scale_color_manual(values = c("purple", "red"), name = "Level") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial", color = "black", size = 12),
        plot.title = element_text(family = "Arial", size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(color = "blue", size = 12, face = "bold"),
        axis.text.y = element_text(color = "black"))


####################3##Descriptive statistics for Spanish data third year####################################

library(dplyr)
library(knitr)
percentage3rd <- Sd3 %>%
  group_by(condition, level, response) %>%
  summarise(Count = n()) %>%
  group_by(condition, level) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  select(condition, level, response, Percentage)

# Reshape the data to have Response as columns
table4 <- spread(percentage3rd, key = response, value = Percentage)
kable(table4, caption = "Descriptive Statistics for Spanish AJT Responses")

Table: Descriptive Statistics for Spanish AJT Responses

|condition |level |        1|        2|        3|        4|
  |:---------|:-----|--------:|--------:|--------:|--------:|
  |C1        |IND   | 15.94203| 16.90821| 38.64734| 28.50242|
  |C1        |SUB   | 16.82692| 21.63462| 40.86538| 20.67308|
  |C2        |IND   | 14.56311| 17.96117| 45.63107| 21.84466|
  |C2        |SUB   | 17.22488| 24.40191| 44.97608| 13.39713|
  |C3        |IND   | 15.65657| 19.19192| 45.45455| 19.69697|
  |C3        |SUB   | 16.33663| 20.79208| 40.09901| 22.77228|
  |C4        |IND   | 15.76355| 27.09360| 39.90148| 17.24138|
  |C4        |SUB   | 17.08543| 26.13065| 37.18593| 19.59799|
  
  
  #Let's modify Percentage3rd and change the condition name to visualize better#
  
  percentage3rd <- percentage3rd %>%
  mutate(condition = case_when(
    condition == "C1" ~ "Specific Referent",
    condition == "C2" ~ "Non-specific Referent",
    condition == "C3" ~ "Factual",
    condition == "C4" ~ "Dubitative",
    TRUE ~ condition  # Keep the original value if none of the conditions are met
  ))

#Let's also change our response variable#

library(dplyr)

# Assuming your Likert scale responses are in a column named "likert_response" in your dataframe
percentage3rd <- percentage3rd %>%
  mutate(response = case_when(
    response == 1 ~ "Totally unacceptable",
    response == 2 ~ "Unacceptable",
    response == 3 ~ "Acceptable",
    response == 4 ~ "Totally acceptable",
    TRUE ~ as.character(response)  # Keep the original value if none of the conditions are met
  ))

##Let's visualize it#
ggplot(percentage3rd, aes(x = level, y = Percentage, fill = factor(response))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_grid(~condition, scales = "free_x", switch = "x") +
  labs(title = "3rd Year (AJT in Spanish)",
       x = "",  # Set x to an empty string
       y = "%", fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("maroon", "black", "blue", "orange")) +
  
  # Combine facet labels, legend, and title customizations
  
  theme(strip.text.x = element_text(size = 12, color = "blue", face = "italic"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",  # Adjust the position as needed
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5))


##Let's calculate the mean rating for 3rd year#

#Let's change response as numeric first#
Sd3$response<-as.numeric(Sd3$response)

S3mean_rating <- Sd3 %>%
  group_by(condition, level) %>%
  summarise(response = mean(response, na.rm = TRUE)) %>% ungroup()


##  Let's modify condition name##

S3mean_rating <- S3mean_rating %>%
  mutate(condition = case_when(
    condition == "C1" ~ "Specific Referent",
    condition == "C2" ~ "Non-specific Referent",
    condition == "C3" ~ "Factual",
    condition == "C4" ~ "Dubitative",
    TRUE ~ condition  # Keep the original value if none of the conditions are met
  ))

##Let's visualize it##

ggplot(S3mean_rating, aes(x = condition, y = response, fill = level, color = level)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_text(aes(label = sprintf("%.2f", response)),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5, color = "black", family = "Arial") +
  labs(title = "Mean Ratings by 3rd Year Students",
       x = "",
       y = "Response Rating") +
  scale_fill_manual(values = c("red", "green"), name = "Level") +
  scale_color_manual(values = c("purple", "red"), name = "Level") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial", color = "black", size = 12),
        plot.title = element_text(family = "Arial", size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(color = "blue", size = 12, face = "bold"),
        axis.text.y = element_text(color = "black"))


##Let's change some names#
lnames(Mainhd)[colnames(Mainhd) == "Slextale"] ="Slextale"
colnames(Mainhd)[colnames(Mainhd) == "English Proficiency"] ="Elextale"
colnames(Mainhd)[colnames(Mainhd) == "Metalinguistic Knowledge"] ="Meta"

colnames(Mainsd)[colnames(Mainsd) == "Spanish Lextale"] ="SP"
colnames(Mainsd)[colnames(Mainsd) == "English Proficiency"] ="Elextale"
colnames(Mainsd)[colnames(Mainsd) == "Metalinguistic Knowledge"] ="Meta"

colnames(Mainsd)[colnames(Mainsd) == "level"] ="Level"
colnames(Mainhd)[colnames(Mainhd) == "Metalinguistic Results"] ="Meta"
colnames(Mainhd)[colnames(Mainhd) == "Meta"] ="Metalinguistic Knowledge"
colnames(Mainhd)[colnames(Mainhd) == "Elextale"] ="English Proficiency"
colnames(Mainhd)[colnames(Mainhd) == "Slextale"] ="Spanish Proficiency"
colnames(Mainhd)[colnames(Mainhd) == "response"] ="Response"
colnames(Mainhd)[colnames(Mainhd) == "condition"] ="Condition"
colnames(Mainhd)[colnames(Mainhd) == "level"] ="Level"


####################################Hindi MODEL################################################
# Assuming your data frame is named df
Mainhd$model <- paste(Mainhd$referend, Mainhd$level, sep = "_")

# Using dplyr for easier handling
library(dplyr)

# Create the item column, numbering 1 to 48 for each participant
Mainhd <- Mainhd %>%
  group_by(Id) %>%
  mutate(item = 1:n())

# Check the result
head(df)

Mainhd$response <- as.factor(Mainhd$response)
levels(Mainhd$YOS)

Mainhd$YOS <- factor(Mainhd$YOS, levels = c("2nd Year", "3rd Year"))


#####Let's start building our model with Meta#

modnull232<-clmm(response ~ Meta*model + (1|Id) +(1|item), data=Mainhd, link="probit")

summary(modnull232)  #Meta is not significant. Also, there is no interaction#

##Let's check without Meta#

modnull233<-clmm(response ~ model + (1|Id) + (1|item), data=Mainhd, link="probit") #NULL MODEL##

summary(modnull233)  #This model is more significant without Meta# This makes sense as Hindi is the L1 and it was acquired naturally so it can be assumed that the participants may not have the Meta knowledge, but still have the correct intitution to differciate between Sub and Ind in Hindi$ 

anova(modnull232,modnull233)

#Let's include Yos and Meta# YOS and Inst are corelated hence only one of thgm could be included while running the model#

modnull234<-clmm(response ~ YOS*Meta*model + (1|Id) +(1|item), data=Mainhd, link="probit")

summary(modnull234)  #YOS and Meta together not relevant and no interaction again#

anova(modnull234,modnull233)-#Model 234 is a better model with YOS#
  
  Mainhd$YOS <- relevel(Mainhd$YOS, ref = "3rd Year")



Mainhd$YOS <- relevel(Mainhd$YOS, ref = "2nd Year")  # Reverting back to original reference



################Let's do it with Elextale like before we did it with Meta#

modnull235<-clmm(response ~ Elextale*reflevel + (1|Id), data=Mainhd, link="probit")

summary(modnull235)  ## Eng Prof is not significant on its own, but combined with YOS it becomes significant.(Check for example)

#Let's compare the model with null model#

anova(modnull235,modnull233)

#Let's add Meta to this model and see if this is relevant#

modnull240<-clmm(Response ~ Elextale*reflevel*Meta*YOS + (1|Id), data=Mainhd, link="probit")

summary(modnull240) # This model fits best the data#

#Let's compare this model with model234#

anova(modnull240,modnull234)
## This model shows that there is no effect of English prof on the participants' intutions about modd choice in Hindi. In addition, Eng prof has not resulted in Hindi attrition

#Cuando es negativo quiere decir que es rating es mas bajo. Los particpantes rated subjunvtive significanlty lower with the fact condition as the P-valus is very low#
#referendFact:levelSUB -1.366969   0.099179 -13.783  < 2e-16 ***#

# Si no es significatva la interacion no se puede hablar de la diferencia ni mas ni menos. Si es significative eso quiere decir que el rating es significativamente es mas alto o mas bajo##
summary(nullmm)


#Let's run pairwise comparision##



emmeans_hindi <- emmeans(modnull240, pairwise ~ reflevel, type = "response")

summary(emmeans_hindi)

emmeans_hindi2 <- emmeans(nene2, pairwise ~ reflevel, type = "response")
summary(emmeans_hindi2)




##Run pairwise comparision to see if the proficience in English has an effect on AJT task###
EQ1<-clmm(response ~ Elextale*reflevel + (1|Id)+(1|item), data=Q1_data, link="probit")
summary(EQ1)

EQ2<-clmm(response ~ Elextale*reflevel + (1|Id)+(1|item), data=Q2_data, link="probit")
summary(EQ2)

EQ3<-clmm(response ~ Elextale*reflevel + (1|Id)+(1|item), data=Q3_data, link="logit")
summary(EQ3)

EQ4<-clmm(response ~ Elextale*reflevel +  (1|Id)+(1|item), data=Q4_data, link="logit")

summary(EQ4)

#Pairwise comparision#

EQ1P <- emmeans(EQ1, pairwise ~ reflevel, type = "response")

summary(EQ1P)

EQ2P <- emmeans(EQ2, pairwise ~ reflevel, type = "response")

summary(EQ2P)

EQ3P <- emmeans(EQ3, pairwise ~ reflevel, type = "response")
summary(EQ3P)

EQ4P <- emmeans(EQ4, pairwise ~ reflevel, type = "response")
summary(EQ4P)

####################################Spanish MODEL################################################


cor(Mainsd$Meta,Mainsd$Slextale)

#Let's run our first model with Meta#

modnull237<-clmm(response ~ Meta*reflevel + (1|Id) +(1|item), data=Mainsd, link="probit")

summary(modnull237)     # Meta is not significant# Question:If meta is not significant, but it shows signficant association while interacting with condition how we interpret that:

#Let's run a null model to compare it#

ModNULL<-clmm(Response ~ reflevel+ (1|Id) +(1|item), data=Mainsd, link="probit")
summary(ModNULL)

anova(modnull237,modnull238)## Model with Meta (modnull237) is better#

##Let's build more models with Spa.prof proficiency#

modnull239<-clmm(response ~ Slextale*reflevel + (1|Id)+(1|item), data=Mainsd, link="logit")

summary(modnull239)   ##Spanish Prof. is significant#



anova(modnull237,modnull239)- # Model with Spa (modnull239) fits better#
  
  modnull241<-clmm(Response ~ Slextale*reflevel*Meta + (1|Id)+(1|item), data=Mainsd, link="probit")

summary(modnull241) # No intearction betwen Spanish and Meta Also, they have no interaction with other conditions#

anova(modnull237,modnull241)

#Let's check it with Elextale#

modnull240 <- clmm(response ~ reflevel*Elextale*Meta*YOS + (1|Id), data=Mainsd, link="probit")

summary(modnull240)         -#English is not significant#
  
  
  modnull000<-clmm(response ~ reflevel*Elextale*Slextale*Meta*YOS + (1|Id)+(item) data=Mainsd, link="probit")

summary(modnull000)# Spa & Eng together are not significant,but there is a 3-way interaction#


anova(modnull000,modnull239) #Model with Spanish prof fits the data best#

rm(checkM)

checkM<-clmm(Response ~ reflevel*Slextale*Meta*YOS + (1|Id) + (1|item), data=SQ1, link="probit")

summary(checkM)
##Let's run pairwise comparision for Spanish##

library(emmeans)

emmeans_Spanish <- emmeans(modnull239, pairwise ~ reflevel, type = "response")

summary(emmeans_Spanish)
