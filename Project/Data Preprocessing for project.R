library(readr)
dataset <- read.csv("NeighborNosh_Form_Responses _updated.csv")
colnames(dataset) <- c("Timestamp", "Email", "Occupation", "Name", "Gender", "State", "Locality", "Frequency_Homemade_Food", "Factors_Influence_Order", "Travel_Distance", "Favorite_Dishes", "Subcription_Preference", "Ordering_Situations", "Importance_Cooking_Style", "Loyalty_Factors", "Eco_Friendly_Preference", "Additional_Services_Interest", "name", "state", "locality", "interest_providing_food", "motivation_participation", "days_per_week_preparation", "confident_dishes", "centralized_handling", "special_diet_capability")
library(ggplot2)
library(tidyr)

#Dataset split 
#student_data_split
start_student_index <- which(names(dataset) == "Email")
end_student_index <- which(names(dataset) == "Additional_Services_Interest")

#housewife_data_split
start_housewife_index <- which(names(dataset) == "name") # or "Name" depending on the exact naming convention
end_housewife_index <- which(names(dataset) == "special_diet_capability")

# Create the student data frame
df_students <- dataset[, start_student_index:end_student_index]

# Create the housewife data frame
df_housewives <- dataset[, start_housewife_index:end_housewife_index]


#due to some issues the na values were switched to empty strings so we are updating the data set excluding the empty values
df_newHouseWives <- df_housewives[df_housewives$name!="",]
df_newHouseWives

#excluding a dummy value that was present inside the dataset
df_newHouseWives<-df_newHouseWives[-1,]

df_students_filtered <- df_students [ df_students$Occupation != "House Wife",]


#separating the Additional Services Interest column into two columns as both the values have same meaning
df_students_filtered<-separate(df_students_filtered,col=Additional_Services_Interest, into =c("Additional_Services_Interest","Miscl."),sep=",")
df_students_filtered

# Data Visualization of student data
#Bar graph of Favorite Dishes of Students
vibrant_colors <- c("#FF6F61", "#6B5B95", "#88B04B", "#fbb4ae", "#92A8D1")

# Create the bar plot with the vibrant color palette and add numbers to each bar
ggplot(df_students_filtered, aes(x = Favorite_Dishes, fill = Favorite_Dishes)) +
  geom_bar(stat = "count", color = "white") +  # Create bars with counts
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Add count labels above bars
  labs(title = "Favorite Dishes of Students", x = "", y = "Frequency") +
  scale_fill_manual(values = vibrant_colors) +  # Apply vibrant colors
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Bar graphtravel distance

# Define a custom color palette
custom_colors <- c("#ff9999", "#99ccff", "#f3c300", "#66cdaa", "#c39797")

# Create the bar plot with the custom color palette and numbers on bars
ggplot(df_students_filtered, aes(x = Travel_Distance, fill = Travel_Distance)) +
  geom_bar(color = "white") + 
  geom_text(stat = 'count', aes(label = stat(count)), vjust = -0.5, color = "black", size = 3) +  # Add numbers on bars
  labs(title = "Travel Distance", x = "", y = "Frequency") +
  scale_fill_manual(values = custom_colors) +  # Use the custom color palette
  theme_minimal()


# Data Visualization of housewives data


# Count the frequency of days per week preparation

library(RColorBrewer)

# Count the frequency of days per week preparation
counts <- table(df_newHouseWives$days_per_week_preparation)
data <- as.data.frame(counts)

# Define a color palette from RColorBrewer
custom_colors <- brewer.pal(length(unique(data$Freq)), "Set2")

library(ggplot2)
library(RColorBrewer)

# Count the frequency of days per week preparation
counts <- table(df_newHouseWives$days_per_week_preparation)
data <- as.data.frame(counts)
names(data) <- c("DaysPerWeek", "Freq")  # Rename columns for clarity

# Define a color palette from RColorBrewer
custom_colors <- brewer.pal(length(unique(data$DaysPerWeek)), "Set2")

# Create the donut chart
ggplot(data, aes(x = "", y = Freq, fill = DaysPerWeek)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  labs(title = "Days per Week Preparation") +
  scale_fill_manual(values = custom_colors) +  # Custom color palette
  annotate("text", x = 0, y = 0, label = " ", size = 10, color = "white") +  # Add label for the center
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8))  # Adjust legend text size

# Display the plot
print(ggplot2::last_plot())





#Bar graph confident_dishes

mixed_colors <- c("#F4A261", "#E63946", "#92DCE5", "#B678FF")

# Create the bar plot with the mixed color palette
ggplot(df_newHouseWives, aes(x = confident_dishes, y = ..count.., fill = confident_dishes)) + 
  geom_bar(color = "white") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +  # Add labels to the bars
  labs(title = "Confidence in Dishes", x = "", y = "Frequency") +
  scale_fill_manual(values = mixed_colors) +  # Use the mixed color palette
  theme_minimal()


# Create the pie chart for Additional Services
counts <- table(df_students_filtered$Additional_Services_Interest)
data <- as.data.frame(counts)
ggplot(data, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") + 
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  labs(title = "Additional Services") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Additional_Services_Interest))))

# Creating the Pie Chart for Subscriptions
countsSubs <- table(df_students_filtered$Subcription_Preference)
dataSubs <- as.data.frame(countsSubs)
ggplot(dataSubs, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") + 
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  labs(title = "Subscription Preferences") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Subcription_Preference))))

# Write the students data frame to a CSV file
write.csv(df_students_filtered, "df_students_filtered.csv", row.names = FALSE)
# Write the housewives data frame to a CSV file
write.csv(df_newHouseWives, "df_newHouseWives.csv", row.names = FALSE)

#design colour piechart in Frequency_Homemade_Food
dataSubs$Frequency_Homemade_Food <- factor(dataSubs$Var1)

ggplot(dataSubs, aes(x = "", y = Freq, fill = Frequency_Homemade_Food)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  labs(title = "Frequency of Homemade Food") +
  scale_fill_manual(values = vibrant_colors)







#design colour piechart in motivation_participation

countsSubs <- table(df_newHouseWives$motivation_participation)
dataSubs <- as.data.frame(countsSubs)
names(dataSubs) <- c("Motivation for Participation", "Freq")  # Rename columns for clarity

ggplot(dataSubs, aes(x = "", y = Freq, fill = `Motivation for Participation`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  labs(title = "Motivation for Participation",
       fill = "Motivation for Participation") +  # Set the legend title
  scale_fill_manual(values = rainbow(length(unique(df_newHouseWives$motivation_participation))))


##Bar graph Frequency_Homemade_Food
ggplot(df_students_filtered, aes(x = Frequency_Homemade_Food, fill = Frequency_Homemade_Food)) + 
  geom_bar(color = "white") + 
  labs(title = "Frequency_Homemade_Food", x = "", y = "Frequency") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Frequency_Homemade_Food))))

333333
#bar motivation_participation
ggplot(df_newHouseWives, aes(x = motivation_participation, fill = motivation_participation)) + 
  geom_bar(color = "white") + 
  labs(title = "motivation_participation", x = "", y = "Frequency") +
  scale_fill_manual(values = rainbow(length(unique(df_newHouseWives$motivation_participation))))

# CLUSTERED bar Factors_Influence_Order


ggplot(df_students_filtered, aes(x = Frequency_Homemade_Food, fill = Factors_Influence_Order)) + 
  geom_bar(color = "white") + 
  labs(title = "Factors_Influence_Order", x = "", y = "Frequency") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Factors_Influence_Order))))



ggplot(df_students_filtered, aes(x = Frequency_Homemade_Food, fill = Factors_Influence_Order)) + 
  geom_bar(color = "white") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), color = "black") + # Add count labels
  labs(title = "Factors Influencing Order by Frequency of Homemade Food", x = "", y = "Frequency") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Factors_Influence_Order))))
#DDINT WORKED
library(dplyr)
ggplot(df_students_filtered, aes(x = Travel_Distance, fill = factor(Travel_Distance))) + 
  geom_histogram(color = "white", bins = 30) + 
  labs(title = "Travel Distance", x = "Travel Distance", y = "Frequency") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Travel_Distance))))

library(ggplot2)

# Assuming df_students_filtered contains two columns: Travel_Distance and Frequency

ggplot(df_students_filtered, aes(x = Travel_Distance, y = Frequency, fill = factor(Travel_Distance))) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(title = "Travel Distance Histogram", x = "Travel Distance", y = "Frequency") +
  scale_fill_manual(values = rainbow(length(unique(df_students_filtered$Travel_Distance))))


  


# Ensure your data frame 'df_students_filtered' has the 'Gender' column as a factor
df_students_filtered$Gender <- as.factor(df_students_filtered$Gender)
df_students_filtered$Importance_Cooking_Style <- as.factor(df_students_filtered$Importance_Cooking_Style)

# Create a clustered bar chart
ggplot(df_students_filtered, aes(x = Importance_Cooking_Style, fill = Gender)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Importance of Cooking Style by Gender", x = "Importance of Cooking Style", y = "Count") +
  scale_fill_manual(values = c("darkblue", "darkred")) + # Assign dark colors
  theme_minimal()

ggplot(df_students_filtered, aes(x = Importance_Cooking_Style, fill = Gender)) +
  geom_bar(position = "dodge", stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = 0.9), vjust = -0.5) + # Add labels
  labs(title = "Importance of Cooking Style by Gender", x = "Importance of Cooking Style", y = "Count") +
  scale_fill_manual(values = c("darkblue", "darkred")) + # Assign dark colors
  theme_minimal()

# Simulating a Gender column for demonstration
set.seed(123)  # For reproducible random data
df_students_filtered$Gender <- sample(c("Male", "Female"), size = nrow(df_students_filtered), replace = TRUE)

# Viewing the modified dataset
head(df_students_filtered)

summary_df <- df_students_filtered %>%
  group_by(Importance_Cooking_Style, Loyalty_Factors) %>%
  summarise(Count = n(), .groups = 'drop')

print(summary_df)
#Counts of Loyalty Factors by Importance of Cooking Style 

ggplot(summary_df, aes(x = Importance_Cooking_Style, y = Count, fill = Loyalty_Factors)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Counts of Loyalty Factors by Importance of Cooking Style",
       x = "Importance of Cooking Style", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


library(ggplot2)
#with number labelling 

# Summary dataframe
summary_df <- df_students_filtered %>%
  group_by(Importance_Cooking_Style, Loyalty_Factors) %>%
  summarise(Count = n(), .groups = 'drop')

# Print summary dataframe
print(summary_df)

# Plot with labels
ggplot(summary_df, aes(x = Importance_Cooking_Style, y = Count, fill = Loyalty_Factors)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), vjust = -0.5, position = position_dodge(width = 0.9)) +  # Add labels on top of bars
  theme_minimal() +
  labs(title = "Counts of Loyalty Factors by Importance of Cooking Style",
       x = "Importance of Cooking Style", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



library(dplyr)

# Assuming df_students_filtered is your dataset
summary_df <- df_students_filtered %>%
  count(Gender, Eco_Friendly_Preference)



ggplot(summary_df, aes(x = Gender, y = n, fill = Eco_Friendly_Preference)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +  # Add labels on top of bars
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Counts of Eco-Friendly Preference by Gender", 
       x = "Gender", 
       y = "Count")
