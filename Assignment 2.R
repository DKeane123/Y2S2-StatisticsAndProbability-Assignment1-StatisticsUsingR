############################################################
# Statistics Assignment 2 (Stats and Probability)
#
# Daniel Keane / ID: 20098745 / Programme: Applied Computing
#
# R commands to address the eight questions pertaining to the
# flow of traffic.
############################################################

# Load the data from the harddrive
assignmentCSV = read.table("customers.csv", sep= ",", header = TRUE)

# Allow the entering of flow
attach(assignmentCSV)

############################################################
#
# Q1 #######################################################
#
# Plot
boxplot(flow ~ classification, horizontal=TRUE)

# Test
summary(aov(flow~classification))
tapply(flow,classification,shapiro.test)
#
#
############################################################


############################################################
#
# Q2 #######################################################
#
# Plot
boxplot(flow ~ services, horizontal=TRUE)

# Test
summary(aov(flow~services))
#
#
############################################################


############################################################
#
# Q3 #######################################################
#
# Plot
table(tolled, classification) -> tab
row.names(tab) = c(1,2)
colnames(tab) = c(1,2,3)
barplot(tab, xlab="Classification", ylab="Tolled")
legend("topright", legend=c("tolled","untolled"), fill=c("gray20", "gray80"))

# Test
summary(aov(classification~tolled))
#
#
############################################################


############################################################
#
# Q4 #######################################################
#
# Plot
plot(flow ~ population)

# Test
cor.test(flow, population)
#
#
############################################################


############################################################
#
# Q5 #######################################################
#
# Plot
points(previously, residences)
plot(flow, residences, col = "blue", xlab = "Flow Rates", ylab = "Residences")
points(previously, residences, col= "red")
legend("topright", legend=c("flow","previous"), fill=c("red", "blue"))

# Test

#
#
############################################################


############################################################
#
# Q6 #######################################################
#
# Plot
boxplot(flow~tolled)

# Test
t.test(flow~tolled)
#
#
############################################################


############################################################
#
# Q7 #######################################################
#
# Plot
plot(flow, residences)

# Test
t.test(flow~tolled)
#
#
############################################################


############################################################
#
# Q8 #######################################################
#
# Plot


# Test

#
#
############################################################
