# Question 1: Does an individual's location affect their placement?

#### 

# COMPLETED ~~~~

# Import dataset

data <- read.csv(data)
# Drop columns you don't need
data <- data[ -c(1,2,3,5,7,8,9,10,11,12,13,15)]
# rename columns to better read/understand
data <- data %>% rename(TenthGradeLocation = 1, TwelfthGradeLocation = 2, PlacementStatus = 3)
# drop columns for tenth grade dataset
data <- data[ -c(2)]
# create new columns for 10th grade Others and Central and are Placed or Not, graph, and explain
P10 <- data[data$TenthGradeLocation == 'Others' & data$PlacementStatus == "Placed",]
NP10 <- data[data$TenthGradeLocation == 'Others' & data$PlacementStatus == "Not Placed",]
tp10 <- P10 %>% count(PlacementStatus)
np10 <- NP10 %>% count(PlacementStatus)
total10 <- rbind(tp10, np10)
total10 <- total10 %>% rename(Location = 2)
plot1 <-ggplot(data=total10, aes(x=PlacementStatus, y=Location)) + geom_bar(fill="light blue", stat="identity")
plot1

# create new columns for 12th grade Others and Central and are Placed or Not, graph, and explain
P12 <- data[data$TenthGradeLocation == 'Central' & data$PlacementStatus == "Placed",]
NP12 <- data[data$TenthGradeLocation == 'Central' & data$PlacementStatus == "Not Placed",]
tp12 <- P12 %>% count(PlacementStatus)
np12 <- NP12 %>% count(PlacementStatus)
total12 <- rbind(tp12, np12)
total12 <- total12 %>% rename(Location = 2)
plot2 <-ggplot(data=total12, aes(x=PlacementStatus, y=Location)) + geom_bar(fill="light blue", stat="identity")
plot2

####

# COMPLETED ~~

# Question 2: Does an individual's Work Experience affect how much their Reported Salary is?

# Reattach original dataset 

# Drop columns you don't need
data2 <- data[ -c(1,2,3,4,5,6,7,8,9,11,12,13,14)]
# rename columns to better read/understand
data2 <- data2 %>% rename(WorkExperience = 1, Salary = 2)
# Check for na's and remove if needed
sum(is.na(data$Salary))
data2 <- data2 %>% drop_na(Salary)
# Visualize the data and make an inference in a comment below with your code
options(scipen = 999) # to disable scientific notation
ggplot(data2, aes(x=WorkExperience, y=Salary)) + geom_boxplot(color="blue", fill="blue", 
alpha=0.2, notch=TRUE, notchwidth = 0.8, outlier.colour="light blue", outlier.fill="black", 
outlier.size=3) + coord_flip() + ggtitle("Work Experience and Salary")
# It appears having work experience does impact ones salary due to having a higher starting salary


