library(mongolite)
library(dplyr)
library(ggplot2)

# Connect to MongoDB
data = mongo(db = 'Mydb', collection = 'Mycollection')

# Count total documents
data$count()

# Count documents where "Primary Type" is "BATTERY" and "Domestic" is true
data$count('{"Primary Type":"BATTERY", "Domestic":true}')

# Fetch one document
data$iterate()$one()

# Find specific fields in the data
data$find('{"Primary Type":"BATTERY", "Domestic":true}', '{"Primary Type":1 , "_id":0, "Date":1}')

# Aggregate crime count by location
value = data$aggregate('[
  {"$group": {"_id": "$Location Description", "Count": {"$sum": 1}}},
  {"$sort": {"Count": -1}} 
]')

# Remove NA values
value = na.omit(value)

# Sort values in descending order
sorted_val = value %>% arrange(desc(Count))

# Get top 1 location with highest crime count
sorted_val = head(sorted_val, 10)

# Convert to DataFrame for ggplot
sorted_val = as.data.frame(sorted_val)

# Plot data using ggplot2
ggplot(sorted_val, aes(x = reorder(`_id`, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color="black") +
  labs(title = "Total Crimes by Location", x = "Location", y = "Total Crimes") +
  theme_minimal() +
  coord_flip()# Flips x and y axes for better visualization


library(lubridate)
domestic = data$find('{"Domestic":true}', '{"_id":0 , "Domestic":1 , "Date":1}')
domestic
library(hms)
domestic = as.data.frame(domestic)
domestic$Date = mdy_hms(domestic$Date)
domestic$Date
domestic$weekdays = weekdays(domestic$Date)
domestic$weekdays
domestic$hours = hour(domestic$Date)
domestic$month = month(domestic$Date)
domestic$month


weekdayCount= as.data.frame(table(domestic$weekdays))
weekdayCount$Var1 = factor(weekdayCount$Var1, ordered=TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
weekdayCount$Var1
ggplot(weekdayCount , aes(x=Var1 , y=Freq , group = 1))+
  geom_line(color="blue", size=1, linetype="dashed")


graphData = as.data.frame(table(domestic$weekdays , domestic$hours))
graphData

ggplot(graphData , aes(x=Var2 , y=Freq , group=Var1 , color=Var1))+geom_line()+scale_y_continuous(limits=c(0,600))+  # Corrected closing parenthesis
  labs(title = "Line Graph of Hours vs Values", 
       x = "Hours", 
       y = "Frequency", 
       color = "Days") +  # `color = "Days"` ensures legend is labeled correctly
  theme_minimal()
