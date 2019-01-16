# Load the dplyr package
library(dplyr)

# Load the hflights package
install.packages("hflights")
library(hflights)

# Call both head() and summary() on hflights
head(hflights)
summary(hflights)


#tbl(tibble) - um tipo especial de dataframe, muito mais facil de olhar os dados e de trabalhar com ele


# Convert the hflights_df data.frame into a hflights tbl
hflights_tbl <- as_tibble(hflights)

# Display the hflights tbl
hflights_tbl

# Create the object carriers
carriers <- hflights$UniqueCarrier

# Both the dplyr and hflights packages are loaded into workspace
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Add the Carrier column to hflights
hflights$Carrier <- lut[hflights$UniqueCarrier]

# Glimpse at hflights
glimpse(hflights)

# The hflights tbl you built in the previous exercise is available in the workspace.

# The lookup table
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

# Add the Code column
hflights$Code <- lut[hflights$CancellationCode]

# Glimpse at hflights
glimpse(hflights)

# Print out a tbl with the four columns of hflights related to delay
hflights %>% select(ActualElapsedTime, AirTime, ArrDelay, DepDelay)

# Print out the columns Origin up to Cancelled of hflights
as_tibble(hflights %>% select(Origin:Cancelled))

# Answer to last question: be concise!
hflights %>% select(Year:DayOfWeek, ArrDelay:Diverted)

# both hflights and dplyr are available

# Finish select call so that ex1d matches ex1r
ex1r <- hflights[c("TaxiIn", "TaxiOut", "Distance")]
ex1d <- select(hflights, TaxiIn, TaxiOut, Distance)

# Finish select call so that ex2d matches ex2r
ex2r <- hflights[c("Year", "Month", "DayOfWeek", "DepTime", "ArrTime")]
ex2d <- select(hflights, Year, Month, DayOfWeek, DepTime, ArrTime)

# Finish select call so that ex3d matches ex3r
ex3r <- hflights[c("TailNum", "TaxiIn", "TaxiOut")]
ex3d <- select(hflights, TailNum, TaxiIn, TaxiOut)

  
# Add the new variable ActualGroundTime to a copy of hflights and save the result as g1.
g1 <- hflights %>% mutate(ActualGroundTime = ActualElapsedTime - AirTime)

# Add the new variable GroundTime to g1. Save the result as g2.
g2 <- g1 %>% mutate(GroundTime = TaxiIn + TaxiOut)
g2$ActualGroundTime == g2$GroundTime
# Add the new variable AverageSpeed to g2. Save the result as g3.
g3 <- g2 %>% mutate(AverageSpeed = 60 * Distance / AirTime)

# Print out g3
g3



# Add a second variable loss_ratio to the dataset: m1
m1 <- mutate(hflights, loss = ArrDelay - DepDelay, loss_ratio = loss / DepDelay)

# Add the three variables as described in the third instruction: m2
m2 <- mutate(hflights, TotalTaxi = TaxiIn + TaxiOut, ActualGroundTime = ActualElapsedTime - AirTime, Diff = TotalTaxi - ActualGroundTime)

# All flights that traveled 3000 miles or more
hflights %>% filter(Distance >= 3000)

# All flights flown by one of JetBlue, Southwest, or Delta
hflights %>% filter(UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))

# All flights where taxiing took longer than flying
hflights %>% filter(TaxiIn + TaxiOut > AirTime)


# All flights that departed before 5am or arrived after 10pm
filter(hflights, DepTime < 500 | ArrTime > 2200)

# All flights that departed late but arrived ahead of schedule
filter(hflights, DepDelay > 0 & ArrDelay < 0)

# All flights that were cancelled after being delayed
filter(hflights, DepDelay > 0 & Cancelled == 1)


# Select the flights that had JFK as their destination: c1
c1 <- hflights %>% filter(Dest == "JFK")

# Combine the Year, Month and DayofMonth variables to create a Date column: c2
c2 <- mutate(c1, Date = paste(Year,Month, DayofMonth, sep = "-"))

# Print out a selection of columns of c2
c2 %>% select(Date, DepTime, ArrTime, TailNum)


# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

# Arrange dtc by departure delays
dtc %>% arrange(DepDelay)

# Arrange dtc so that cancellation reasons are grouped
dtc %>% arrange(CancellationCode)

# Arrange dtc according to carrier and departure delays
dtc %>% arrange(UniqueCarrier, DepDelay)

# dplyr and the hflights tbl are available

# Arrange according to carrier and decreasing departure delays
hflights %>% arrange(UniqueCarrier, desc(DepDelay))

# Arrange flights by total delay (normal order).
hflights %>% arrange(DepDelay + ArrDelay)
