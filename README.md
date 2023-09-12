# Ohtani_22_23
This project analyzes the pitching performance of Shohei Ohtani during the 2022 and 2023 MLB seasons. The analysis includes visualizations of Ohtani's pitches, strikeouts, and pitch distribution.


# Load Packages

library(plyr) #Data manipulation and aggregation
library(dplyr) #Data manipulation and transformation
library(devtools) # Tools for package development
library(DT) # Interactive data tables
library(ggplot2) # Data visualization using the Grammar of Graphics
library(ggrepel) # Prevent overlapping text labels in ggplot2
library(ggthemes) # Themes and color palettes for ggplot2
library(gridExtra) # Arrange and combine grid-based plots
library(janitor) # Data cleaning and table formatting
library(plotly) # Interactive data visualization 
library(stringr) # String manipulation and text pattern matching
library(tidyr) # Data tidying and reshaping
library(tidyselect) # Helper functions for tidyverse selection
library(tidyverse) # A collection of packages for data manipulation and visualization
library(data.table) # Fast data manipulation and querying
library(reactable) # Interactive data tables for Shiny apps
library(ggpubr) # Publication-ready ggplot2 plots

## Create the "is_strikeout" column
Angels_2022 <- Angels_2022 %>%
  mutate(is_strikeout = ifelse(events == "strikeout", TRUE, FALSE))

Angels_2023 <- Angels_2023 %>%
  mutate(is_strikeout = ifelse(events == "strikeout", TRUE, FALSE))


#rename column "pitch_name" to "Pitch Type" in the data frame "Angels_2022",assigns the result to a new data frame "Ohtani_1_22."
Ohtani_1_22 <- Angels_2022 %>%
  rename(`Pitch Type` = pitch_name)

Ohtani_1_23 <- Angels_2022 %>%
  rename(`Pitch Type` = pitch_name)

# filter data frame "Ohtani_1_22" to include only rows where the "player_name" column is equal to "Ohtani, Shohei."
Ohtani_Subset_22 <- Ohtani_1_22 %>%
  filter(player_name == "Ohtani, Shohei")


# Create a subset of the data for Ohtani's pitches on specific date (4/7/22)
Ohtani_1_22 <- subset(Ohtani_Subset_22, select = c(player_name,game_date, `Pitch Type`, plate_z, plate_x,stand,is_strikeout))

# Create a subset with only strikeout events
Ohtani_2_22 <- subset(Ohtani_Subset_22, is_strikeout == TRUE, select = c(player_name, game_date, `Pitch Type`, plate_z, plate_x,stand, is_strikeout))



# Ohtani 4/7/2022 (1st start of season)

#creates a scatter plot named for Manoah's 1st Start of 2022 using ggplot2 
#based on the data frame "Ohtani_1_22." 
#plots points on the x and y coordinates, 
#assigns color of each point determined by the "Pitch Type" column. 
#color scheme for the points is specified using custom colors for different pitch types. 
#plot is customized with point size and other aesthetics.
Ohtani_ST1_22 <- Ohtani_1_22%>%
  filter(game_date == "4/7/2022")%>%
  ggplot(Ohtani_1_22, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  # create boundaries for MLB strike zone and home plate
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  
  # customize the appearance of a ggplot2 plot-- Center plot & Title, removes major/minor gridlines,
  # removes background color
  # Sets x & y axis limits
  #Sets chart title
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani 4/7/22")

print(Ohtani_ST1_22)


# Ohtani 2nd start (4/14/2022)
Ohtani_ST2_22 <- Ohtani_1_22%>%
  filter(game_date == "4/14/2022")%>%
  ggplot(Ohtani_1, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei 4/14/22")

print(Ohtani_ST2_22)

#Combines both charts
ggarrange(Ohtani_ST1_22, Ohtani_ST2_22,nrow = 1, ncol = 2)

# Just Strikeouts by start -- Charts


# Calculate K counts by Pitch Type and batter hand
pitch_counts <- Ohtani_2_22 %>%
  group_by(`Pitch Type`, stand) %>%
  summarize(count = n())


# 4/7/2022
Ohtani_4_7_22 <- Ohtani_2_22%>%
  filter(game_date == "4/7/2022")%>%
  ggplot(Ohtani_2_22, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani K's 4/7/22") 

print(Ohtani_4_7_22)


# bar chart for the distribution of Pitch Type
SO_4_7_22 <- Ohtani_2_22 %>%
  filter(game_date == "4/7/2022") %>%
  ggplot(aes(x = `Pitch Type`, fill = stand)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(
    title = "K's by Pitch Type by Batter Handedness",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c("R" = "blue", "L" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(SO_4_7_22)


# 4/14/22
Ohtani_4_14_22 <- Ohtani_2_22%>%
  filter(game_date == "4/14/2022")%>%
  ggplot(Ohtani_2_22, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani K's 4/14/22")

print(Ohtani_4_14_22)

#Combines both charts
ggarrange(Ohtani_4_7_22, Ohtani_4_14_22,nrow = 1, ncol = 2)

# Create a bar chart for the distribution of Pitch Type

#4/7/22
SO_4_7_22 <- Ohtani_2_22 %>%
  filter(game_date == "4/7/2022") %>%
  ggplot(aes(x = `Pitch Type`, fill = stand)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(
    title = "Ohtani K's by Pitch Type by Batter Handedness 4/7/22",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c("R" = "blue", "L" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(SO_4_7_22)



#4/14/22
SO_4_14_22 <- Ohtani_2_22 %>%
  filter(game_date == "4/14/2022") %>%
  ggplot(aes(x = `Pitch Type`, fill = stand)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(
    title = "Ohtani K's by Pitch Type by Batter Handedness 4/14/22",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c("R" = "blue", "L" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(SO_4_14_22)


#Combines both charts
ggarrange(SO_4_7_22, SO_4_14_22,nrow = 1, ncol = 2)


# Filter the data for 'stand' = 'R' and create a bar chart for strikeouts by batter hand & pitch type
SO_chart_RHB <- Ohtani_2_22 %>%
  filter(stand == "R") %>%
  ggplot(aes(x = `Pitch Type`, fill = `Pitch Type`)) +
  geom_bar() +
  labs(
    title = "Ohtani K's by Pitch Type 2022 (RHB)",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c(
    Changeup = "blue", `4-Seam Fastball` = "black",
    Slider = "orange", Curveball = "red",
    Cutter = "green", Sinker = "grey",
    `Split-Finger` = "purple", Sweeper = "pink"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1)  # Slant x-axis labels
  )

print(SO_chart_RHB)


# Filter the data for 'stand' = 'L' and create a bar chart
SO_chart_LHB <- Ohtani_2_22 %>%
  filter(stand == "L") %>%
  ggplot(aes(x = `Pitch Type`, fill = `Pitch Type`)) +
  geom_bar() +
  labs(
    title = "Ohtani K's by Pitch Type 2022 (LHB)",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c(
    Changeup = "blue", `4-Seam Fastball` = "black",
    Slider = "orange", Curveball = "red",
    Cutter = "green", Sinker = "grey",
    `Split-Finger` = "purple", Sweeper = "pink"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

print(SO_chart_LHB)

#Combines both charts
ggarrange(SO_chart_RHB, SO_chart_LHB,nrow = 1, ncol = 2)





# Calculate counts by Pitch Type and batter hand
SO_pitch_counts_2 <- Ohtani_1_22 %>%
  group_by(`Pitch Type`) %>%
  summarize(count = n())

# Calculate the percentages and round to the nearest hundredth
SO_pitch_pct <- SO_pitch_counts_2 %>%
  mutate(Percentage = round((count / sum(count)) * 100, 0))

# Create a pie chart
ggplot(SO_pitch_pct, aes(x = "", y = Percentage, fill = `Pitch Type`, label = scales::percent(Percentage / 100))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Percentage / 100)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = " Ohtani Pitch Distribution 2022",
       fill = "Pitch Type") +
  theme_void() +
  theme(legend.position = "right") 





#2023

## Create the "is_strikeout" column
Angels_2023 <- Angels_2023 %>%
  mutate(is_strikeout = ifelse(events == "strikeout", TRUE, FALSE))


#rename column "pitch_name" to "Pitch Type" in the data frame "Angels_2022",assigns the result to a new data frame "Ohtani_1_23."
Ohtani_1_23 <- Angels_2023 %>%
  rename(`Pitch Type` = pitch_name)


# filter data frame "Ohtani_1_22" to include only rows where the "player_name" column is equal to "Ohtani, Shohei."
Ohtani_Subset_23 <- Ohtani_1_23 %>%
  filter(player_name == "Ohtani, Shohei")



Ohtani_1_23 <- subset(Ohtani_Subset_23, select = c(player_name,game_date, `Pitch Type`, plate_z, plate_x,stand,is_strikeout))


Ohtani_2_23 <- subset(Ohtani_Subset_23, is_strikeout == TRUE, select = c(player_name, game_date, `Pitch Type`, plate_z, plate_x,stand, is_strikeout))



# Ohtani 3/30/2023 (1st start of season)
Ohtani_ST1_23 <- Ohtani_1_23%>%
  filter(game_date == "3/30/2023")%>%
  ggplot(Ohtani_1_23, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+

  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  
 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani 3/30/23 vs OAK")

print(Ohtani_ST1_23)


# Ohtani 2nd start (4/05/2023)
Ohtani_ST2_23 <- Ohtani_1_23%>%
  filter(game_date == "4/5/2023")%>%
  ggplot(Ohtani_1, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani 4/5/23 vs SEA" )

print(Ohtani_ST2_23)


#Combines both charts
ggarrange(Ohtani_ST1_23, Ohtani_ST2_23,nrow = 1, ncol = 2)

# Just Strikeouts by start -- Charts

pitch_counts <- Ohtani_2_23 %>%
  group_by(`Pitch Type`, stand) %>%
  summarize(count = n())


# 3/30/2023
Ohtani_3_30_23 <- Ohtani_2_23%>%
  filter(game_date == "3/30/2023")%>%
  ggplot(Ohtani_2_23, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani K's 3/30/23 vs OAK") 

print(Ohtani_3_30_23)


# bar chart for the distribution of Pitch Type


SO_3_30_23 <- Ohtani_2_23 %>%
  filter(game_date == "3/30/2023") %>%
  ggplot(aes(x = `Pitch Type`, fill = stand)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(
    title = "Ohtani K's by Pitch Type by Batter Handedness",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c("R" = "blue", "L" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(SO_3_30_23)


# 4/5/23
Ohtani_4_5_23 <- Ohtani_2_23%>%
  filter(game_date == "4/5/2023")%>%
  ggplot(Ohtani_2_23, mapping = aes(x=plate_x, y= plate_z)) +
  geom_point(aes(color = `Pitch Type`),size = 2) +
  scale_color_manual(values = c(Changeup = "blue", `4-Seam Fastball` = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                `Split-Finger` = "purple",Sweeper = "pink"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Shohei Ohtani K's 4/5/23 vs SEA")

print(Ohtani_4_5_23)

#Combines both charts
ggarrange(Ohtani_3_30_23, Ohtani_4_5_23,nrow = 1, ncol = 2)


# Create a bar chart for the distribution of Pitch Type

#4/5/23
SO_4_5_23 <- Ohtani_2_23 %>%
  filter(game_date == "4/5/2023") %>%
  ggplot(aes(x = `Pitch Type`, fill = stand)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(
    title = "Ohtani K's by Pitch Type by Batter Handedness",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c("R" = "blue", "L" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(SO_4_5_23)



#Combines both charts
ggarrange(SO_3_30_23, SO_4_5_23,nrow = 1, ncol = 2)


# Filter the data for 'stand' = 'R' and create a bar chart for strikeouts by batter hand & pitch type
SO_chart_RHB_23 <- Ohtani_2_23 %>%
  filter(stand == "R") %>%
  ggplot(aes(x = `Pitch Type`, fill = `Pitch Type`)) +
  geom_bar() +
  labs(
    title = "Ohtani K's by Pitch Type 2023 (RHB)",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c(
    Changeup = "blue", `4-Seam Fastball` = "black",
    Slider = "orange", Curveball = "red",
    Cutter = "green", Sinker = "grey",
    `Split-Finger` = "purple", Sweeper = "pink"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1)  # Slant x-axis labels
  )

print(SO_chart_RHB_23)



# Filter the data for 'stand' = 'L' and create a bar chart
SO_chart_LHB_23 <- Ohtani_2_23 %>%
  filter(stand == "L") %>%
  ggplot(aes(x = `Pitch Type`, fill = `Pitch Type`)) +
  geom_bar() +
  labs(
    title = "Ohtani K's by Pitch Type 2023 (LHB)",
    x = "Pitch Type",
    y = "Count"
  ) +
  scale_fill_manual(values = c(
    Changeup = "blue", `4-Seam Fastball` = "black",
    Slider = "orange", Curveball = "red",
    Cutter = "green", Sinker = "grey",
    `Split-Finger` = "purple", Sweeper = "pink"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

print(SO_chart_LHB_23)


#Combines both charts
ggarrange(SO_chart_RHB_23, SO_chart_LHB_23,nrow = 1, ncol = 2)

# Calculate counts by Pitch Type and batter hand
SO_pitch_counts_23 <- Ohtani_1_23 %>%
  group_by(`Pitch Type`) %>%
  summarize(count = n())

# Calculate the percentages and round to the nearest hundredth
SO_pitch_pct_23 <- SO_pitch_counts_23 %>%
  mutate(Percentage = round((count / sum(count)) * 100, 0))


# Create a pie chart
ggplot(SO_pitch_pct_23, aes(x = "", y = Percentage, fill = `Pitch Type`, label = scales::percent(Percentage / 100))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Percentage / 100)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = " Ohtani Pitch Distribution 2023",
       fill = "Pitch Type") +
  theme_void() +
  theme(legend.position = "right") 

