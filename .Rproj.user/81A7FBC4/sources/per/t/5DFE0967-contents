confirmedraw <- read.csv("C:/Users/munde/Documents/BIDMiniProject/time_series_covid19_confirmed_global.csv")
deathsraw <- read.csv("C:/Users/munde/Documents/BIDMiniProject/time_series_covid19_deaths_global.csv")
recoveredraw <- read.csv("C:/Users/munde/Documents/BIDMiniProject/time_series_covid19_recovered_global.csv")

library(tidyr)
library(dplyr)
library(ggplot2)

confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
summary(confirmed)

country <- full_join(confirmed, deaths) %>% full_join(recovered)
country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
# Create new variable: number of days
country <- country %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)


world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
# Extract specific country: Italy
italy <- country %>% filter(Country.Region=="Italy")

india <- country %>% filter(Country.Region=="India")

summary(country)
by(country$confirmed, country$Country.Region, summary)
by(country$cumconfirmed, country$Country.Region, summary)
by(country$deaths, country$Country.Region, summary)
by(country$recovered, country$Country.Region, summary)
summary(world)
summary(italy)
summary(india)


# World confirmed, deaths and recovered
str(world)
world %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=date, y=Cases, colour=Type)) + geom_bar(stat="identity", width=0.2, fill="white") +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Date", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5))

# World confirmed
ggplot(world, aes(x=days, y=confirmed)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

# World confirmed with counts in log10 scale
ggplot(world, aes(x=days, y=confirmed)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases  (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

# Confirmed by country for select countries with counts in log10 scale
countryselection <- country %>% filter(Country.Region==c("US", "Italy", "China", "France", "United Kingdom", "Germany"))
ggplot(countryselection, aes(x=days, y=confirmed, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

# Matrix of line graphs of confirmed, deaths and recovered for select countries in log10 scale
str(countryselection)
countryselection %>% gather("Type", "Cases", -c(date, days, Country.Region)) %>%
  ggplot(aes(x=days, y=Cases, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cases by Country", x= "Days", y= "Daily cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10") +
  facet_grid(rows=vars(Type))

