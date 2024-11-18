#loading the data 
data<-read.csv("C:\\Users\\HP\\Desktop\\final pro\\accident r cleaning.csv", header = TRUE)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(scales)
library(plotly)



##To see the datatype of each column and the format
str(data)

##Describe the statically things in the data
summary(data)

##Duplicates removing
data<- data %>%distinct()

##Change datatype
data$ACCIDENT_DATE <- as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y")


##To check the datatype after the update
str(data)

##Filling missing rows in sensors
data$Sensors[is.na(data$Sensors)] <- 'yes'
##Check that all columns have complete cells
colSums(is.na(data))


##Check if their is outlier in speed_zone
boxplot(data$SPEED_ZONE, main="The Outlier of speed zone", ylab= 'Speed_zone')


##clean function to clean the speed 
clean <- function(df, column) {
  df <- df %>%
    mutate(!!sym(column) := ifelse(!!sym(column) > 120, !!sym(column) %/% 10, !!sym(column)))
  return(df)
}
##Use the clean function on speed_zone column
data <- clean(data, "SPEED_ZONE")


##Calculate the mean of SPEED_ZONE
mean_speedzone <- mean(data[["SPEED_ZONE"]], na.rm = TRUE)
##Replace NA values in SPEED_ZONE with the mean
data[["SPEED_ZONE"]] <- ifelse(is.na(data[["SPEED_ZONE"]]), mean_speedzone, data[["SPEED_ZONE"]])

 
##Calculate the median of severity, ignoring NA
median_SEVERITY <- median(data[["SEVERITY"]], na.rm = TRUE)
##Replace NA values in severity
data[["SEVERITY"]] <- ifelse(is.na(data[["SEVERITY"]]), median_SEVERITY, data[["SEVERITY"]])


##change speed type to integer
data$SPEED_ZONE<- as.integer(data$SPEED_ZONE)

##check the speed if the outlier is gone
boxplot(data$SPEED_ZONE, main="The Outlier of speed zone", ylab= 'Speed')

##check if the missing values have been filled
colSums(is.na(data))


##ADD COLUMN LIGHT_CONDITION_DESC
light_condition <- function(light) {
  if (light == 1) {
    return('Daylight AM')
  } else if (light == 2) {
    return('Dawn')
  } else if (light == 3) {
    return('Dusk')
  } else if (light == 4) {
    return('Dark – Street lights on')
  } else if (light == 5) {
    return('Dark – Street lights off')
  } else if (light == 6) {
    return('Dark – No street lights')
  } else if (light == 9) {
    return('Overcast')
  } else {
    return(NA) 
  }
}
##apply the function
data <- data %>%
  mutate(light_condition_desc = sapply(LIGHT_CONDITION, light_condition))



# Function to divide time into 4 periods
classify_time_period <- function(time_str) {
  time <- as.POSIXct(time_str, format = "%I:%M %p")
  hour <- as.numeric(format(time, "%H"))
  
  if (hour >= 6 & hour < 12) {
    return("Morning")
  } else if (hour >= 12 & hour < 18) {
    return("Afternoon")
  } else if (hour >= 18 & hour < 24) {
    return("Evening")
  } else {
    return("Night")
  }
}

# Applying the function to a column
data$Period <- sapply(data$ACCIDENT_TIME, classify_time_period)


##Define the rma_mapping
rma_mapping <- c(
  'Local Road' = 1,
  'Arterial Highway' = 2,
  'Arterial Other' = 3,
  'Freeway' = 4,
  'Non Arterial' = 5
)

#Using recode to add RMA_ID:
data$RMA_ID <- recode(data$RMA, !!!rma_mapping)


##Defining the geometry_map 
geometry_mapping <- c(
  'T intersection' = 1,
  'Not at intersection' = 2,
  'Cross intersection' = 3,
  'Multiple intersection' = 4,
  'Unknown' = 5,
  'Y intersection' = 6,
  'Dead end' = 7,
  'Road closure' = 8,
  'Private property' = 9
)
##add the road_geometry_id with geometry_map using recode
data$Road_Geo_ID <- recode(data$ROAD_GEOMETRY_DESC, !!!geometry_mapping)


##Defining the severity_map 
severity_map <- c(
  '1' = 'Minor',
  '2' = 'Moderate',
  '3' = 'Severe',
  '4' = 'Fatal'
)
##Replace the severity with severity_map using recode
data$SEVERITY <- recode(as.character(data$SEVERITY), !!!severity_map)


##To see the added columns
colnames(data)

##Function to delete unneeded columns
delete_column <- function(data, column_name) {
  if (column_name %in% colnames(data)) {
    data[[column_name]] <- NULL 
  }
  return(data)
}
##Applying the function of deleting columns
data <- delete_column(data, "DCA_CODE")


##Correlation
cor_matrix<-cor(data %>% select(where(is.numeric)))
print(cor_matrix)


# Reshape the correlation matrix into long format for ggplot2
cor_melt <- melt(cor_matrix)

# Create the heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +                                  # Heatmap tiles
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limits = c(-1, 1),        # Scale from -1 to 1
                       name = "Correlation") +   # Add legend title
  labs(title = "Correlation Matrix", x = "", y = "") + # Add title and remove axis labels
  theme_minimal() +                              # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


#---------------------------------------------------------------------------------------------------------

##ANALYSIS AND VISUALISATION## 

#---------------------------------------------------------------------------------------------------------
#which year has the highest number of accidents

data %>%
  mutate(accident_year = format(ACCIDENT_DATE, "%Y")) %>%  
  group_by(accident_year) %>%                              
  summarise(accident_count = n()) %>%                      
  arrange(desc(accident_count))                            


accident_date<- data %>%
  mutate(accident_year = format(ACCIDENT_DATE, "%Y")) %>%  
  group_by(accident_year) %>%                              
  summarise(accident_count = n()) %>%                      
  arrange(accident_year)

# Plot the time series chart
ggplot(accident_date, aes(x = as.numeric(accident_year), y = accident_count)) +
  geom_line() +                   
  geom_point() +                   
  labs(title = "Number of Accidents per Year", 
       x = "Year", 
       y = "Number of Accidents") +  
  theme_minimal() +                
  theme(plot.title = element_text(hjust = 0.5)) 

#-------------------------------------------------------------------------------------------------------------
#The number of accidents at each day

accident_summary <- data %>%
  group_by(DAY_WEEK_DESC) %>%                 
  summarise(accident_count = n()) %>%         
  arrange(desc(accident_count))

# Visualisation
ggplot(accident_summary, aes(x = DAY_WEEK_DESC, y = accident_count)) +
  geom_bar(stat = "identity") +       
  labs(title = "Number of Accidents per Day", 
       x = "Days", 
       y = "Number of Accidents") +   
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  
  theme_minimal() +                  
  theme(plot.title = element_text(hjust = 0.5)) + # Center the title
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  scale_y_continuous(labels = comma) # Format y-axis labels with commas
#_____________________________________________________________________________________________________
# The number of accidents at each period for day

time_acc <- data %>%
  group_by(Period) %>%
  summarise(ACCIDENT_NO = n(), .groups = 'drop')

# Create a donut chart
ggplot(time_acc, aes(x = "", y = ACCIDENT_NO, fill = Period)) +
  geom_bar(stat = "identity", width = 0.5, color = "white") + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  theme(legend.position = "right") +
  labs(title = "Number of Accidents by Period of Day") +
  geom_text(aes(label = percent(ACCIDENT_NO / sum(ACCIDENT_NO))), 
            position = position_stack(vjust = 0.5), color = "white") +
  theme(plot.title = element_text(hjust = 0.5)) +  
  geom_blank(aes(y = 0)) 
#----------------------------------------------------------------------------------------------------------
#The number of accidents at each speed

speed_zone_summary <- data %>%
  group_by(SPEED_ZONE) %>%              
  summarise(accident_count = n()) %>%   
  arrange(desc(accident_count))         
print(speed_zone_summary)


# Create a bar chart for accident counts by speed zone
ggplot(speed_zone_summary, aes(x = reorder(SPEED_ZONE, -accident_count), y = accident_count, fill = SPEED_ZONE)) +
  geom_bar(stat = "identity") +                 
  labs(title = "Accident Counts by Speed Zone", 
       x = "Speed Zone", 
       y = "Number of Accidents") + 
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  # Position text above the bars
  theme_minimal() +                             
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#____________________________________________________________________________________________________________
#The number of accidents at each severity type

severity_summary <- data %>%
  group_by(SEVERITY) %>%               
  summarise(accident_count = n()) %>%  
  arrange(desc(accident_count))        
print(severity_summary)

# Create a bar chart
ggplot(severity_summary, aes(x = reorder(SEVERITY, -accident_count), y = accident_count, fill = SEVERITY)) +
  geom_bar(stat = "identity") +                  
  labs(title = "Accident Counts by Severity", 
       x = "Severity", 
       y = "Number of Accidents") +              
  theme_minimal() +                              
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#___________________________________________________________________________________________________________
#The number of accidents at each light condition

light_condition_summary <- data %>%
  group_by(light_condition_desc) %>%          
  summarise(accident_count = n()) %>%          
  arrange(desc(accident_count))                
print(light_condition_summary)

# Create a bar chart
ggplot(light_condition_summary, aes(x = reorder(light_condition_desc, -accident_count), y = accident_count, fill = light_condition_desc)) +
  geom_bar(stat = "identity") +                
  labs(title = "Accident Counts by Light Condition", 
       x = "Light Condition", 
       y = "Number of Accidents") +  
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  
  theme_minimal() +                            
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#________________________________________________________________________________________________________
# which Accident_Type has the highest number of accidents

accident_type_summary <- data %>%
  group_by(ACCIDENT_TYPE_DESC) %>%             
  summarise(accident_count = n()) %>%          
  arrange(desc(accident_count))                
print(accident_type_summary)

# Create a bar chart
ggplot(accident_type_summary, aes(x = reorder(ACCIDENT_TYPE_DESC, -accident_count), y = accident_count, fill = ACCIDENT_TYPE_DESC)) +
  geom_bar(stat = "identity") +                  
  labs(title = "Accident Counts by Type", 
       x = "Accident Type", 
       y = "Number of Accidents") + 
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  
  theme_minimal() +                              
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#_________________________________________________________________________________________________________
#which Rma has the highest number of accidents

rma_summary <- data %>%
  group_by(RMA) %>%                           
  summarise(accident_count = n()) %>%        
  arrange(desc(accident_count))               
print(rma_summary)

# Create a bar chart
ggplot(rma_summary, aes(x = reorder(RMA, -accident_count), y = accident_count, fill = RMA)) +
  geom_bar(stat = "identity") +                  
  labs(title = "Accident Counts by RMA", 
       x = "RMA", 
       y = "Number of Accidents") +
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  
  theme_minimal() +                             
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#____________________________________________________________________________________________________________
# which Road geometry has the highest number of accidents

road_geometry_summary <- data %>%
  group_by(ROAD_GEOMETRY_DESC) %>%   
  summarise(accident_count = n()) %>% 
  arrange(desc(accident_count))        
print(road_geometry_summary)

# Create a bar plot
ggplot(road_geometry_summary, aes(x = reorder(ROAD_GEOMETRY_DESC, -accident_count), y = accident_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Accident Count by Road Geometry",
       x = "Road Geometry Description",
       y = "Accident Count") +
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#____________________________________________________________________________________________________________
#show what is the severity of the accident for each road

# Create the pivot table
severityroad_acc <- data %>%
  group_by(RMA, SEVERITY) %>%
  summarise(ACCIDENT_NO = n(), .groups = 'drop') %>%  
  pivot_wider(names_from = SEVERITY, values_from = ACCIDENT_NO, values_fill = list(ACCIDENT_NO = 0)) 

# Melt the DataFrame to long format for Plotly
severityroad_long <- severityroad_acc %>%
  pivot_longer(-RMA, names_to = "Severity", values_to = "Number of Accidents")

# Creating the bar plot with specified width and height
fig <- plot_ly(
  data = severityroad_long,
  x = ~RMA,  # Types of roads
  y = ~`Number of Accidents`, 
  color = ~Severity,  
  type = 'bar', 
  text = ~`Number of Accidents`,  
  textposition = 'outside',  
  height = 600,  
  width = 1000   
) %>%
  layout(
    title = "Number of Accidents on Road by Severity",  # Set title
    xaxis = list(title = "Type of Road", tickangle = -45),  # X-axis title and angle
    yaxis = list(title = "Number of Accidents", showgrid = TRUE),  # Y-axis title
    plot_bgcolor = 'rgba(0, 0, 0, 0)'  # Background color
  )
packageVersion("plotly")

# Display the figure
fig
#___________________________________________________________________________________________________
#show what is the severity of the accident for each speed

# Create the pivot table
speeds_acc <- data %>%
  group_by(SPEED_ZONE, SEVERITY) %>%
  summarise(ACCIDENT_NO = n(), .groups = 'drop') %>%  
  pivot_wider(names_from = SEVERITY, values_from = ACCIDENT_NO, values_fill = list(ACCIDENT_NO = 0))  

# View the pivot table
print(speeds_acc)


# Melt the DataFrame to long format for Plotly
speeds_long <- speeds_acc %>%
  pivot_longer(-SPEED_ZONE, names_to = "SEVERITY", values_to = "ACCIDENT_NO")

# Creating the bar plot with specified width and height
fig <- plot_ly(
  data = speeds_long,
  x = ~SPEED_ZONE,  
  y = ~ACCIDENT_NO,  
  color = ~SEVERITY,  
  type = 'bar',  
  text = ~ACCIDENT_NO,  
  textposition = 'outside',  
  height = 600,  
  width = 1000   
) %>%
  layout(
    title = 'Number of Accidents by Speed Zone and Severity',
    xaxis = list(title = 'Speed Zone', tickangle = -45),  
    yaxis = list(title = 'Number of Accidents', showgrid = TRUE),  
    titlefont = list(size = 16),  
    xaxis_titlefont = list(size = 12),
    yaxis_titlefont = list(size = 12),
    plot_bgcolor = 'rgba(0, 0, 0, 0)' 
  )

# Show the plot
fig


#-------------------------------------------------------------------------------------------------------------
#show in speed 60 why is the highest number

# Filter and group the data by multiple columns
summary_data <- data %>%
  filter(SPEED_ZONE == 60) %>%                          
  group_by(ROAD_GEOMETRY_DESC, ACCIDENT_TYPE_DESC) %>%  
  summarise(count = n()) %>%                             
  arrange(desc(count))                                   
print(summary_data)

# Create a heatmap using geom_tile
ggplot(summary_data, aes(x =  ACCIDENT_TYPE_DESC, y = ROAD_GEOMETRY_DESC , fill = count)) +
  geom_tile() +                                         
  labs(title = "Accidents by Type, Road Geometry", 
       x = "Accident Type", 
       y = "Road Geometry") +                           
  scale_fill_gradient(low = "lightblue", high = "red") + 
  theme_minimal() +                                     
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#-------------------------------------------------------------------------------------------------------------
#show why Accident_Type Collision with vehicle is the highest number

# Filter and group the data by multiple columns
summary_data2 <- data %>%
  filter(ACCIDENT_TYPE_DESC == "Collision with vehicle") %>%    
  group_by(SEVERITY, light_condition_desc) %>%  
  summarise(count = n()) %>%                                      
  arrange(desc(count))                                             
print(summary_data2)


# Create a stacked bar chart
ggplot(summary_data2, aes(x = light_condition_desc, y = count, fill = SEVERITY)) +
  geom_bar(stat = "identity") +                     
  labs(title = "Accident Severity by Light Condition", 
       x = "Light Condition", 
       y = "Number of Accidents") +                 
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = count),         
            position = position_stack(vjust = 0.5),  
            color = "black") +  
  theme_minimal() +                               
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#----------------------------------------------------------------------------------------------------------
#Before 2016, a sensor was not available.

summary_time_period <- data %>%
  mutate(time_period = case_when(
    ACCIDENT_DATE >= as.Date('2012-01-01') & ACCIDENT_DATE < as.Date('2016-01-01') ~ 'from 2012 to 2016',
    ACCIDENT_DATE >= as.Date('2016-01-01') & ACCIDENT_DATE < as.Date('2020-01-01') ~ 'from 2016 to 2020',
    TRUE ~ NA_character_  
  )) %>%
  group_by(time_period) %>%
  summarise(accident_count = n(), .groups = 'drop') %>%
  filter(!is.na(time_period))
print(summary_time_period)


ggplot(summary_time_period, aes(x = time_period, y = accident_count, fill = time_period)) +
  geom_bar(stat = "identity") +                   
  labs(title = "Number of Accidents by Time Period", 
       x = "Time Period", 
       y = "Number of Accidents") + 
  geom_text(aes(label = percent(accident_count / sum(accident_count))), 
            vjust = -0.5) +  
  scale_fill_brewer(palette = "Set2") +          
  theme_minimal() +                              
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#_______________________________________________________________________________________________________________

