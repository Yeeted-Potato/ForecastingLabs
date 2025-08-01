library(fpp3)
library(readxl)
library(readr)
tute1 <- read_csv("tute1.csv")
View(tute1)


tute1[,2]
tute1[,"Sales"]
tute1[5,]
tute1[1:10,3:4]
tute1[1:10,2] <- 0
tute1[1:10,2]
tute1[1:20,]

(100+2)/3
5*10^2
1/0
0/0
(0i-9)^(1/2)
sqrt(2 * max(-10,0.2,4.5))
x <- sqrt(2 * max(-10, 0.2, 4.5)) + 100
x
log(100)
log(100, base = 10)


#Plot one time series
aus_retail |>
  filter(`Series ID`=="A3349640L") |>
  autoplot(Turnover)

#Producing some forecasts
aus_retail |>
  filter(`Series ID`=="A3349640L") |>
  model(ETS(Turnover)) |>
  forecast(h = "2 years")

#tsibble example
mydata <- tsibble(
  year = 2015:2019,
  y = c(123, 39, 78, 52, 110),
  index = year
)
mydata

z <- tibble(Month = paste(2019, month.abb[1:5]),
            Observation = c(50, 23, 34, 30,25))
z

#Question 1 (FPP3 1.8 Exercises: Question 1) For cases 3 and 4 in Section 1.5
#list the possible predictor variables that might be useful
#assuming that the relevant data are available.

#Quantity of car type being sold on the market

aus_production |> autoplot(Beer)
autoplot(pelt)
autoplot(gafa_stock)
autoplot(vic_elec)

x <- vic_elec$Time
y <- vic_elec$Demand
plot(x, y, main = "Vic Electric", xlab = "Time[30M]", ylab = "Demand")

interval(aus_production)
interval(pelt)
interval(gafa_stock)
interval(vic_elec)

AAPLstock <- gafa_stock[gafa_stock$Symbol == "AAPL", ]
AMZNstock <- gafa_stock[gafa_stock$Symbol == "AMZN", ]
FBstock <- gafa_stock[gafa_stock$Symbol == "FB", ]
GOOGstock <- gafa_stock[gafa_stock$Symbol == "GOOG", ]

APPLEmax_price <- AAPLstock |> filter(High == max(High))
APPLEmax_price

AMZNmax <- AMZNstock %>% filter(High == max(High))
AMZNmax

FBmax <- FBstock %>% filter(High == max(High))
FBmax

GOOGmax <- GOOGstock %>% filter(High == max(High))
GOOGmax

gafa_stock |> 
  group_by(Symbol) |>
  filter(Close == max(Close))

mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  duplicates(index = Quarter)
mytimeseries

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

mytimeseries

y <- us_employment$Employed
autoplot(us_employment)
gg_season(us_employment)
gg_lag(us_employment)
ACF(us_employment)

us_employment |> 
  filter(Title == "Total Private") |>
  gg_season(Employed) |>
  autoplot(us_employment)

x <- aus_production$Bricks
autoplot(x)
gg_season(x)
gg_lag(x)
ACF(x)

AMZN <- gafa_stock |> filter(Symbol == "AMZN")
AMZN %>% autoplot(Close)

AAPL <- gafa_stock %>% filter(Symbol == "AAPL")
AAPL %>% autoplot(Close)

GOOG <- gafa_stock %>% filter(Symbol == "GOOG")
GOOG %>% autoplot(Close)

FB <- gafa_stock %>% filter(Symbol == "FB")
FB %>% autoplot(Close)

olympic_running %>% filter(Year > 1932)

y <- tsibble( Year = 2015:2019, Observation = c(132, 39, 78, 25, 110), index = Year )
y


y <- tsibble( Year = 2015:2019, Observation = c(123, 39, 78, 52, 110), index = Year )
y
