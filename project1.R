library(ggplot2)
library(caret)
library(dplyr)
library(readr)
library(highcharter)
library(rpivotTable)

# Dataset
covid <- read.csv(file.choose(), header = T)
summary(covid)
newcov <- covid[-c(10001:306429),]
str(newcov)

# Converting character string to DATE
newcov$ObservationDate <- as.Date(newcov$ObservationDate, format = "%m/ %d/ %Y")

# Aggregating data
confirmed_cases <- aggregate(Confirmed ~ ObservationDate, newcov, sum)
death_cases <- aggregate(Deaths ~ ObservationDate, newcov ,sum)
recovered_cases <- aggregate(Recovered ~ ObservationDate, newcov ,sum)

totalConfirmed <- confirmed_cases$Confirmed[length(confirmed_cases$Confirmed)]
totalDeaths <- death_cases$Deaths[length(death_cases$Deaths)]
totalRecovered <- recovered_cases$Recovered[length(recovered_cases$Recovered)]

# Scatter plot
ggplot() +
  geom_point(data = confirmed_cases, aes(x = ObservationDate, y = Confirmed, color
                                         = 'Confirmed Cases')) +
  geom_point(data = death_cases, aes(x = ObservationDate, y = Deaths, color =
                                       'Death Cases')) +
  geom_point(data = recovered_cases, aes(x = ObservationDate, y = Recovered, color
                                         = 'Recovered Cases')) +
  xlab("Date") + 
  ylab("No. of Cases") +
 # scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  scale_y_continuous(limits = c(0, 350000), breaks = seq(0, 350000, 50000)) +
  labs( color = "Types of cases") + 
  ggtitle("COVID-19 cases") 

# Growth Rate of Covid cases
C_growrate = confirmed_cases %>% arrange(ObservationDate) %>%
  mutate(
    date = ObservationDate,
    grow = Confirmed - lag(Confirmed),
    grow_rate = grow/lag(grow) * 100)

# Eliminating Rows with NA
C_growrate$grow_rate[is.na(C_growrate$grow_rate)] <- mean(C_growrate$grow_rate, na.rm = TRUE)
C_growrate$grow[is.na(C_growrate$grow)] <- mean(C_growrate$grow, na.rm = TRUE)
str(C_growrate)

# Regression Model with static core 
static_time <- as.numeric(format(Sys.time(), "%s"))
model <- lm(grow_rate ~ ObservationDate, data = C_growrate)
end_time <- as.numeric(format(Sys.time(), "%s"))

summary(model)


# Regression Model with static core 
ncores <- detectCores(logical=FALSE) # Number of cores

registerDoParallel(ncores) # Starting parallel process

parallel_time <- as.numeric(format(Sys.time(), "%s"))
model <- lm(grow_rate ~ ObservationDate, data = C_growrate)
end_time <- as.numeric(format(Sys.time(), "%s"))

summary(model)

# plotting of Regression model
ggplot(data = C_growrate, aes(x = ObservationDate, y = grow_rate, group = 1)) + 
  geom_line(color = 'red') +
  geom_smooth(formula = y ~ x, method = "lm", se = F, col = 'black') +
  #scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  xlab("Date") + 
  ylab ( "Growth Rate") + 
  ggtitle("Confirmed Cases Growth rate")


### Similarly we can do the plotting for death cases and recovered cases.

# Death Rate of Covid cases
D_growrate = death_cases %>% arrange(ObservationDate) %>%
  mutate(
    date = ObservationDate,
    grow = Deaths - lag(Deaths),
    grow_rate = grow/lag(grow) * 100)

# plotting of Regression model
ggplot(data = D_growrate, aes(x = ObservationDate, y = grow_rate, group = 1) ) + 
  geom_line(color = 'blue') +
  geom_smooth(formula = y ~ x, method = "lm", se = T, col = 'black') +
  #scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  xlab("Date") + 
  ylab ( "Growth Rate") + 
  ggtitle("Confirmed Cases Growth rate")

# Cases in Countries 
Countries <- c("Mainland China","Italy","Spain","Germany","US","India")
C_cases <- newcov %>% filter(Country.Region %in% Countries) %>% 
           group_by(ObservationDate, Country.Region) %>% summarise(totalConfirmed = sum(Confirmed), .groups = 'drop')

China = C_cases %>% filter(Country.Region == "Mainland China")
Italy = C_cases %>% filter(Country.Region == "Italy")
Spain = C_cases %>% filter(Country.Region == "Spain")
Germany = C_cases %>% filter(Country.Region == "Germany")
US = C_cases %>% filter(Country.Region == "US")
India = C_cases %>% filter(Country.Region == "India")

China_grow = China %>% group_by(Country.Region) %>%
  arrange(ObservationDate) %>%
  mutate(
    date = ObservationDate,
   grow = totalConfirmed - lag(totalConfirmed))
China_grow$ObservationDate <- as.Date(China_grow$ObservationDate, format = "%m/ %d/ %Y")

Italy_grow = Italy %>% group_by(Country.Region) %>%
  arrange(ObservationDate) %>%
  mutate(
    date = ObservationDate,
    grow = totalConfirmed - lag(totalConfirmed))

Spain_growth = Spain %>% group_by(Country.Region) %>%
  arrange(ObservationDate) %>%
  mutate(
    date = ObservationDate,
    grow = totalConfirmed - lag(totalConfirmed))

Germany_grow = Germany %>% group_by(Country.Region) %>%
  arrange(ObservationDate) %>%
  mutate(
    Diff_date = ObservationDate,
    grow = totalConfirmed - lag(totalConfirmed))

US_grow = US %>% group_by(Country.Region) %>%
  arrange(ObservationDate) %>%
  mutate(
    Diff_date = ObservationDate,
    grow = totalConfirmed - lag(totalConfirmed))

India_grow = India %>% group_by(Country.Region) %>%
  arrange(ObservationDate) %>%
  mutate(
    Diff_date = ObservationDate,
    grow = totalConfirmed - lag(totalConfirmed))

# Covid-19 Cases growth in China
ggplot(data = China_grow, aes(x = ObservationDate , y = grow, group = 1)) +
  geom_line(color = "black") + 
  #scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  xlab("Date") +
  ylab("Cases per day") +
  ggtitle("COVID-19 cases per day trend of China")

# Covid-19 cases growth in top Countries
ggplot() +
geom_line(data = Italy_grow, aes(x = ObservationDate , y = grow, color = "Italy", group = 1), size = 1) + 
geom_line(data = Spain_growth, aes(x = ObservationDate , y = grow, color = "Spain", group = 1), size = 1) +
geom_line(data = Germany_grow, aes(x = ObservationDate , y = grow, color = "Germany", group = 1), size = 1) +
geom_line(data = US_grow, aes(x = ObservationDate , y = grow, color = "US", group = 1), size = 1) +
geom_line(data = India_grow, aes(x = ObservationDate , y = grow, color = "India", group = 1), size = 1) +
  #scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  xlab("Date") +
  ylab("Cases per day")+ 
  ggtitle("COVID-19 cases per day trend of Other Countries")

# Mapping of Confirmed Cases in  US State
map <- read.csv(file.choose(), header = T)
summary(map)

US_map <- map %>% 
  group_by(Province_State) %>% 
  summarise(count= n())
summary(US_map)


highchart() %>% 
  hc_title(text = "Confirmed Cases in US") %>% 
  hc_subtitle(text = "time_series_covid_19_deaths_US.csv") %>% 
  hc_add_series_map(usgeojson, US_map,
                    name = "Province_State",
                    value = "count",
                    dataLabels = list(enabled = T, format = "{point.name}"),
                    joinBy = c("woename", "Province_State")) %>% 
  hc_mapNavigation(enabled = T)

# Death cases in US Using Pivot Table
heat <- read.csv(file.choose(), header = T)
rpivotTable(heat,
            aggregatorName = "count",
            cols = "count",
            rows = "Province_State",
            rendererName = "Heatmap",
            rainbow(50))
