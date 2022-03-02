# Install and call required packages
install.packages(c("tidyverse", "ggplot2", "scales", "psych", "patchwork"))
library(ggplot2)
library(tidyverse)
library(scales)
library(psych)
library(patchwork)


########################################################################
########################## Game 4 output Agents only #########################
########################################################################

# Analysis of Game 4
mydata <- subset(read.csv(file = 'EXPERIMENT-Game-4-output.csv'), ID != 0)
#View(mydata)
total_num_rounds_played <- subset(mydata, players_turn == 2 & decision_consistency == 2 & coalition_suggestion == 2)
unique(total_num_rounds_played$round_num)


# Comparing G4 CDF of CP with in_cp
# Plot CDFs
CP <- mydata %>%
  group_by(ID) %>%
  filter(CP == 0) %>%
  summarize(round = min(round_num))
CP$type <- "Coalition partition" 
qqnorm(CP$round)
qqline(CP$round, lty=2)

in_cp <- mydata %>%
  group_by(ID) %>%
  filter(in_cp == 0) %>%
  summarize(round = min(round_num))
in_cp$type <- "Core coalition" 
qqnorm(in_cp$round) 

df <- rbind(CP, in_cp)

ggplot(df, aes(x = round, color = type)) + 
  stat_ecdf() + 
  xlab("Round numbers") +
  ylab("Cumulative probability")+
  #scale_x_continuous(limits = c(1, 200)) +
  scale_x_continuous(trans = 'log2', limits = c(1, max(df$round))) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("#AE4031", "#4631AE")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.position = c(0.8, 0.15),
        legend.title = element_blank())

# Statistical analysis for Game 4
shortdata <- subset(mydata, players_turn == 2 & decision_consistency == 2 & coalition_suggestion == 2,
                  select = c(ID, round_num))
orderedShortdata <- shortdata[order(shortdata$round_num),]
df <- table(orderedShortdata$round_num) 
game4 <- describe(shortdata) #with Psych library
game4
#write.table(game4, file = "descStG4.csv", sep = ",", quote = FALSE, row.names = F) # with dplyer tidyr, broom


# Histogram of round number
# Original data the first 100 observations only
originial_data <- ggplot(shortdata, aes(x = round_num)) + 
  geom_histogram(color="lightblue", fill="#293352")+
  scale_x_continuous(limits = c(0, 100)) +
  xlab("Number of rounds") +
  ylab("Count")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA))

# Data applied log function
log_transformed_data <- ggplot(shortdata, aes(x = round_num)) + 
  geom_histogram(color="lightblue", fill="#293352")+
  scale_x_continuous(trans = 'log2') + 
  xlab("Number of rounds") +
  ylab("Count")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA))

# Print both the figures next to each other
originial_data + log_transformed_data

########################################################################
########################## Game 4 output Humans ########################
########################################################################

game4humans <- read.csv(file = 'EXPERIMENT-Game-4-output-human.csv')
total_num_rounds_played <- subset(game4humans, players_turn == 2 & decision_consistency == 2 & coalition_suggestion == 2)
nrow(total_num_rounds_played)

##################### Rounds when agents reach the CP #####################
##################### CP = in_cp: coalition partition is formed ###########

CP__reached_G4h <- subset(game4humans, in_cp == 0 & CP == 0,
                          select = c(ID, round_num, CP, in_cp)) %>%
  group_by(ID) %>%
  filter(row_number()==1)
View(CP__reached_G4h)

game4hum <- describe(CP__reached_G4h) #with Psych library
game4hum

# Did not form a coalition #####################
newdataH1 <- subset(game4humans, in_cp == -1 & CP == -1,
                    select = c(ID, round_num, CP, in_cp))

# barplot comparing the round numbers between agents and human
bar_data <- read.csv(file = 'G4_cum_percent.csv')
write.table(bar_data, file = "chi_sqGame4.csv", sep = ",", quote = FALSE, row.names = F) 
View(bar_data)
bar_data$ï..agents
df <- gather(bar_data, key = "category", value = "value", c("ï..agents", "humans"))
df$category[df$category=="ï..agents"] <- "Cummulative % agents"
df$category[df$category=="humans"] <- "Cummulative % humans"
df$id = c(1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
View(df)

ggplot(df, aes(x = id, y = value, fill = category)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.5) +
  ylab("Cumulative percentages") +
  xlab("Number of rounds") +
  geom_hline(aes(yintercept = -Inf), color = "black") +
  scale_x_continuous(breaks = round(seq(min(df$id), max(df$id), by = 1), 1)) +
  scale_y_continuous(breaks = round(seq(0, 30, by = 5))) +
  #scale_colour_manual(values =c("#999999", "#000085") ) +
  scale_fill_manual(values = c("#293352", "#00AFBB"))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.position = c(0.19, 0.85),
        legend.text = element_text(size = 8),
        legend.title = element_blank())


# Statistical tests for signficance of two analyzed cases
test <- chisq.test(table(bar_data$humans, bar_data$ï..agents))
test
monte_carlo_test <- chisq.test(cbind(bar_data$humans, bar_data$ï..agents),sim=TRUE,B=20000)
monte_carlo_test

