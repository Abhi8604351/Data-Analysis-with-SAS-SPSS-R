library(ggplot2)

# Load the data
data <- read.csv("university_student_dataset.csv")

# Create the bar chart
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Number of Students by Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") # Optional: adds nice colors

library(ggplot2)

# Load the data
data <- read.csv("university_student_dataset.csv")

# Create the bar chart
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Number of Students by Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") # Optional: adds nice colors

library(ggplot2)

# Load the data
data <- read.csv("university_student_dataset.csv")

# Create a histogram for Attendance_Percentage
ggplot(data, aes(x = Attendance_Percentage)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Attendance Percentage",
       x = "Attendance Percentage (%)",
       y = "Frequency") +
  theme_minimal()


library(ggplot2)

# Load the data
data <- read.csv("university_student_dataset.csv")

# Create the boxplot using Specialization
ggplot(data, aes(x = Specialization, y = CGPA, fill = Specialization)) +
  geom_boxplot() +
  labs(title = "CGPA Comparison by Specialization",
       x = "Specialization",
       y = "CGPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotates labels for readability