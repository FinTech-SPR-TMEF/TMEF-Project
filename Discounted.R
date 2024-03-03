# file reading
inflation <- read.csv(file="united-states-inflation-rate-cpi.csv", header=TRUE)
inflation <- select(inflation, -X)
pre <- read.csv(file="merged_review_pre.csv", header=TRUE)
post <- read.csv(file="merged_review_after.csv", header=TRUE)

inflation$Year <- format(as.Date(inflation$date, format="%d/%m/%Y"), "%Y")

pre$Year <- as.numeric(as.character(pre$Year))
inflation$Year <- as.numeric(as.character(inflation$Year))

inflation <- inflation[order(inflation$Year), ]

base_inflation <- inflation[inflation$Year == 2000, "InflationRate"]

inflation$CumulativeInflationRate <- ifelse(inflation$Year == 2000, 1, NA)

for (i in 42:nrow(inflation)) {
  inflation$CumulativeInflationRate[i] <- inflation$CumulativeInflationRate[i-1] * (1 + inflation$InflationRate[i] / 100)
}

inflation$DiscountFactor <- ifelse(inflation$Year >= 2000, 1 / inflation$CumulativeInflationRate, NA)
#Pre
pre <- merge(pre, inflation[, c("Year", "DiscountFactor")], by = "Year", all.x = TRUE)

pre$DiscountedRevenue <- pre$revenue * pre$DiscountFactor
pre$DiscountedBudget <- pre$budget * pre$DiscountFactor

#print(pre$DiscountedRevenue)
#print(pre$DiscountedBudget)

#After
post <- merge(post, inflation[, c("Year", "DiscountFactor")], by = "Year", all.x = TRUE)

post$DiscountedRevenue <- post$revenue * post$DiscountFactor
post$DiscountedBudget <- post$budget * post$DiscountFactor

#print(post$DiscountedRevenue)
#print(post$DiscountedBudget)
