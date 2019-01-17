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


# Print out a summary with variables min_dist and max_dist
summarize(hflights, min_dist = min(Distance), max_dist = max(Distance))

# Print out a summary with variable max_div
summarize(filter(hflights, Diverted == 1), max_div = max(Distance))


# Remove rows that have NA ArrDelay: temp1
temp1 <- filter(hflights, !is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarize(temp1, earliest = min(ArrDelay), average = mean(ArrDelay), latest = max(ArrDelay), sd = sd(ArrDelay))

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2 <- filter(hflights, !is.na(TaxiIn) & !is.na(TaxiOut))# hflights is available with full names for the carriers

# Print the maximum taxiing difference of temp2 with summarize()
summarize(temp2, max_taxi_diff = max(TaxiOut - TaxiIn))

library(hflights)
# Generate summarizing statistics for hflights
summarize(hflights,
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest))

# All American Airline flights
aa <- filter(hflights, UniqueCarrier == "American")

# Generate summarizing statistics for aa 
summarize(aa,
          n_flights = n(),
          n_canc = sum(Cancelled),
          avg_delay = mean(ArrDelay, na.rm=TRUE))


# Write the 'piped' version of the English sentences.
hflights %>% mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarize(avg = mean(diff))



# Chain together mutate(), filter() and summarize()
hflights %>% mutate(RealTime = ActualElapsedTime + 100, mph = (Distance * 60 /RealTime) ) %>%
  filter(!is.na(mph), mph < 70) %>%
  summarize(n_less = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

# Finish the command with a filter() and summarize() call
hflights %>%
  mutate(
    RealTime = ActualElapsedTime + 100, 
    mph = 60 * Distance / RealTime
  ) %>%
  filter(mph< 105 | Cancelled == 1 | Diverted == 1) %>%
  summarize(n_non = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))



# Count the number of overnight flights
hflights %>% filter(!is.na(DepTime) & !is.na(ArrTime) & DepTime > ArrTime) %>%
  summarize(num = n())

# Make an ordered per-carrier summary of hflights
hflights %>%
  group_by(UniqueCarrier) %>%
  summarize(
    p_canc = sum(Cancelled) * 100 / n(),
    avg_delay = mean(ArrDelay, na.rm = TRUE)
  ) %>%
  arrange(avg_delay, p_canc)

# dplyr is loaded, hflights is loaded with translated carrier names

# Ordered overview of average arrival delays per carrier
hflights %>% filter(!is.na(ArrDelay) & ArrDelay > 0) %>%
  group_by(UniqueCarrier) %>%
  summarize(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)

# dplyr and hflights (with translated carrier names) are pre-loaded

# How many airplanes only flew to one destination?
hflights %>%
  group_by(TailNum) %>%
  summarize(n = n_distinct(Dest)) %>%
  filter(n == 1) %>%
  summarize(nplanes = n())

# Find the most visited destination for each carrier
hflights %>%
  group_by(UniqueCarrier, Dest) %>%
  summarize(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)


library(data.table)
hflights2 <- as.data.table(hflights)

# Use summarize to calculate n_carrier
hflights2 %>% summarize(ncarrier = n_distinct(UniqueCarrier))


# Set up a connection to the mysql database
my_db <- src_mysql(dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")

# Reference a table within that source: nycflights
nycflights <- tbl(my_db, "dplyr")

# glimpse at nycflights
glimpse(nycflights)

# Ordered, grouped summary of nycflights
nycflights %>%
  group_by(carrier) %>%
  summarize(n_flights = n(),
            avg_delay = mean(arr_delay)) %>%
  arrange(avg_delay)
