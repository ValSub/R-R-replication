# SETUP

# Install package
install.packages("dplyr")
install.packages("readxl")

# Load the package
library(dplyr)
library(readxl)


# STAGE 1: LOAD MEETING LEVEL DATA
# This is the data behind Table 1 of the paper. Each row is one Federal
# Reerve meeting, with the interet rate decision made at that meeting
# plus the Fed's own forecasts for the economy going into that meeting.

df <- read_excel(
  "/Users/valliammaisubramanian/Downloads/116025-V1/RomerandRomerDataAppendix.xls",
  sheet = "DATA BY MEETING"
)

# The date column (MTGDATE) is stored as number, e.g.
# 11469 means 1/14/69. Month isn't padded to
# 2 digits, so we have to count from the right hand side of the number
# to correctly pull out year, day, and month.
df <- df %>%
  mutate(
    date_str = as.character(MTGDATE),
    n = nchar(date_str),
    year2 = as.numeric(substr(date_str, n - 1, n)),
    day = as.numeric(substr(date_str, n - 3, n - 2)),
    month = as.numeric(substr(date_str, 1, n - 4)),
    full_year = 1900 + year2,
    meeting_date = as.Date(paste(full_year, month, day, sep = "-"))
  )

# Print the first 6 meeting date. Should start at 1969-01-14, which is
# the first FOMC meeting date given in the actual paper 
# (checks our date parsing above actually worked)
head(df$meeting_date)

# Print every column name in the dataset, just to see what we're working with
names(df)

# Converting text to numbers
df <- df %>%
  mutate(across(
    c(OLDTARG, GRADM, GRAD0, GRAD1, GRAD2,
      IGRDM, IGRD0, IGRD1, IGRD2,
      GRAYM, GRAY0, GRAY1, GRAY2,
      IGRYM, IGRY0, IGRY1, IGRY2,
      GRAU0, DTARG, RESID),
    as.numeric
  ))

sapply(df, class)

# Check if only handful of NAs
colSums(is.na(df))

# The idea: the Fed's interet rate decision at each meeting (DTARG) can
# partly be explained just by what the Fed already expected to happen to
# the economy (their own forecasts for growth, inflation, unemployment).
# Whatever part of the rate decision is LEFT OVER after accounting for
# those forecasts is treated as a genuine "shock", not just a
# predictable reaction to expected economic conditions
model1 <- lm(
  DTARG ~ OLDTARG +
    GRAYM + GRAY0 + GRAY1 + GRAY2 +
    IGRYM + IGRY0 + IGRY1 + IGRY2 +
    GRADM + GRAD0 + GRAD1 + GRAD2 +
    IGRDM + IGRD0 + IGRD1 + IGRD2 +
    GRAU0,
  data = df
)

# regression results
summary(model1)

# Only 264 of the 273 meetings had complete data, so 9 got dropped
# automatically by the regression. This puts the reidual
# value back into the correct row for each meeting, and leave it blank
# for the 9 meetings that got dropped
df$my_shock <- NA
df$my_shock[as.numeric(names(residuals(model1)))] <- residuals(model1)

# Compare the calculated shock serie against the shock serie the
# original paper already provide in the ReID column, to check
# if the answer is same as Romer and Romer 
comparison <- df %>% select(meeting_date, RESID, my_shock)

# Print the first 10 rows of that comparison side by side
head(comparison, 10)

# Calculate the correlation between the calc shock serie and the paper's own
# shock serie. A value of 1 = identical
cor(df$RESID, df$my_shock, use = "complete.obs")


# STAGE 2: OUTPUT regression
# Switch from meeting level data to monthly data
# Need to see how the shock serie affects the industrial
# production and price PPI over time.

df_month <- read_excel(
  "/Users/valliammaisubramanian/Downloads/116025-V1/RomerandRomerDataAppendix.xls",
  sheet = "DATA BY MONTH"
)

# Column name 
names(df_month)

# Print first few rows  
head(df_month)

# Change text to numbers 
df_month <- df_month %>%
  mutate(across(
    c(DFF, PCIPNSA, PCWCP, PCPPINSA, PCCPINSA, PCPCEGSA),
    as.numeric
  ))

# Make sure the DATE column is a proper date type, not a date and time
# combined (if not error in calc)
df_month <- df_month %>%
  mutate(DATE = as.Date(DATE))

# Builds 36 new columns, one for each of the past 36 months' shock value
# Monetary policy doen't affect the economy instantly, it take months to filter through 
for (i in 1:36) {
  df_month[[paste0("shock_lag", i)]] <- dplyr::lag(df_month$RESID, i)
}

# Print few rows to check the lagging worked correctly
df_month %>% select(DATE, RESID, shock_lag1, shock_lag2) %>% head(5)

# Lagging industrial production growth itself
for (i in 1:24) {
  df_month[[paste0("output_lag", i)]] <- dplyr::lag(df_month$PCIPNSA, i)
}

# Retricts the data to exactly the same time window the original paper used (January 1970 to December 1996)
df_month_reg <- df_month %>%
  filter(DATE >= as.Date("1970-01-01") & DATE <= as.Date("1996-12-01")) %>%
  mutate(month_num = droplevels(as.factor(format(DATE, "%m")))) # to control for seasonal patterns

# Print the number of rows and columns left after filtering -> should be 324 rows
dim(df_month_reg)

# Build the regression formula automatically (not seperately for each lag)
shock_terms <- paste0("shock_lag", 1:36)
output_terms <- paste0("output_lag", 1:24)
rhs <- paste(c("month_num", output_terms, shock_terms), collapse = " + ")
formula2 <- as.formula(paste("PCIPNSA ~", rhs))

# Run the regression: how doe industrial production growth repond to
# monetary policy shocks from 1 to 36 months ago, while controlling for
# output's own past behaviour and the calendar month
model2 <- lm(formula2, data = df_month_reg)

# regression results
summary(model2)


# STAGE 2: PRICE regression
# how price repond to monetary policy shocks. The paper use 48 months of shock lags here
# instead of 36, because they found price react more slowly than output doe.

# We already built shock_lag1 through shock_lag36 above, so we just add
# the extra 12 months needed (37 through 48)
for (i in 37:48) {
  df_month[[paste0("shock_lag", i)]] <- dplyr::lag(df_month$RESID, i)
}

# Lags of the price serie itself, same logic as the output lags before,
# controls for price having their own natural momentum
for (i in 1:24) {
  df_month[[paste0("price_lag", i)]] <- dplyr::lag(df_month$PCPPINSA, i)
}

# Rebuild the filtered dataset again
df_month_reg <- df_month %>%
  filter(DATE >= as.Date("1970-01-01") & DATE <= as.Date("1996-12-01")) %>%
  mutate(month_num = droplevels(as.factor(format(DATE, "%m"))))

# Same formula building for price and 48 lags
shock_terms48 <- paste0("shock_lag", 1:48)
price_terms <- paste0("price_lag", 1:24)
rhs3 <- paste(c("month_num", price_terms, shock_terms48), collapse = " + ")
formula3 <- as.formula(paste("PCPPINSA ~", rhs3))

# Run the price regression
model3 <- lm(formula3, data = df_month_reg)

# Print results
summary(model3)


# FIGURE 2: CUMULATIVE OUTPUT response
# The regression above give us 36 separate coefficients, one for each
# month's lag -> hard to interpret on its own.
# Need a single running total showing how much a one-off shock this
# month would still be dragging output down (or up) in each of the
# following 48 months. Create a loop that simulate this month by month.

b <- coef(model2)[paste0("output_lag", 1:24)] # pulls out just the output lag coefficients
c_coef <- coef(model2)[paste0("shock_lag", 1:36)] # pulls out just the shock lag coefficients

H <- 48 # how many months into the future we want to simulate
y <- numeric(H) # empty vector to store the simulated growth rate for each month

for (t in 1:H) {
  ar_part <- 0
  # adds up the effect of output's own past value echoing forward
  for (i in 1:24) {
    if (t - i >= 1) ar_part <- ar_part + b[i] * y[t - i]
  }
  # adds the direct effect of the shock at month t, if within the first 36 months
  shock_part <- if (t <= 36) c_coef[t] else 0
  y[t] <- ar_part + shock_part
}

# turns the monthly growth rate into a running total
cum_response <- cumsum(y) * 100

# Print the 48 monthly value, recreate Figure 2 from the paper
cum_response


# FIGURE 4: CUMULATIVE PRICE response
# Same idea as above, just using the price regression instead of
# the output regression, and 48 lags instead of 36

b_p <- coef(model3)[paste0("price_lag", 1:24)]
c_p <- coef(model3)[paste0("shock_lag", 1:48)]

H <- 48
p <- numeric(H)

for (t in 1:H) {
  ar_part <- 0
  for (i in 1:24) {
    if (t - i >= 1) ar_part <- ar_part + b_p[i] * p[t - i]
  }
  shock_part <- if (t <= 48) c_p[t] else 0
  p[t] <- ar_part + shock_part
}

cum_response_p <- cumsum(p) * 100

# Print the 48 monthly value, recreate Figure 4 from the paper
cum_response_p


# EXTENSION: EXTENDING THE SAMPLE TO 2007
# The original paper stops in 1996 because the Fed's internal forecasts
# are only released publicly after a long delay. Other reearchers
# (Wieland and Yang, 2020) built an extended version of the shock serie
# using the same method, running through 2007. We use their published shock serie here

shocks_extended <- read.table(
  "/Users/valliammaisubramanian/Downloads/RR_monetary_shock_monthly.txt",
  header = TRUE,
  sep = ","
)

# Print the column name in this new file
names(shocks_extended)

# Print the first few rows
head(shocks_extended)

# Print the earliet and latet date in the file
range(shocks_extended$date)

# The date column looks like "1969m1", so we split it into a year part
# and a month part, then build a real date from those two piece
shocks_extended <- shocks_extended %>%
  mutate(
    year = as.numeric(substr(date, 1, 4)), # first 4 characters are the year
    month = as.numeric(sub(".*m", "", date)), # remove everything up to and including the "m", leaving just the month number
    parsed_date = as.Date(paste(year, month, "01", sep = "-"))
  )

# Print the first few parsed date, should start at 1969-01-01
head(shocks_extended$parsed_date)

# Print the last few parsed date, should end around 2007
tail(shocks_extended$parsed_date)

# Install and load the package to pull economic data directly from FRED 
install.packages("fredr")
library(fredr)

file.edit("~/.Renviron")

# Connect to FRED using a personal API key 
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Pull monthly industrial production data, not seasonally adjusted,
# matching the same type of series the original paper used
ip_data <- fredr(
  series_id = "IPB50001N",
  observation_start = as.Date("1966-01-01"),
  observation_end = as.Date("2007-12-01")
)

# Pull monthly producer price index data for finished goods, again not
# seasonally adjusted, matching the paper's choice
ppi_data <- fredr(
  series_id = "WPUFD49207",
  observation_start = as.Date("1966-01-01"),
  observation_end = as.Date("2007-12-01")
)

# Print the first few rows of each pulled data set, to check they look sensible
head(ip_data)
head(ppi_data)

# Print the earliest and latest date available in each series
range(ip_data$date)
range(ppi_data$date)

# Combine industrial production, price, and the extended shock series
# into one data set, matched up by date
df_ext <- ip_data %>%
  select(date, ip = value) %>%
  inner_join(ppi_data %>% select(date, ppi = value), by = "date") %>%
  inner_join(shocks_extended %>% select(date = parsed_date, resid_full), by = "date") %>%
  arrange(date)

# Convert the raw index levels into growth rate (percentage change from
# the previous month) -> same format as the original paper's data
# used, rather than the raw level of the index
df_ext <- df_ext %>%
  mutate(
    log_ip = log(ip),
    log_ppi = log(ppi),
    d_log_ip = log_ip - dplyr::lag(log_ip, 1), # output growth rate
    d_log_ppi = log_ppi - dplyr::lag(log_ppi, 1) # price growth rate
  )

# Print the first few rows of the combined, cleaned dataset
head(df_ext)

# Print how many rows and columns we ended up with
dim(df_ext)

# Same lag building as before, just applied to this new extended dataset
for (i in 1:48) {
  df_ext[[paste0("shock_lag", i)]] <- dplyr::lag(df_ext$resid_full, i)
}
for (i in 1:24) {
  df_ext[[paste0("output_lag", i)]] <- dplyr::lag(df_ext$d_log_ip, i)
}
for (i in 1:24) {
  df_ext[[paste0("price_lag", i)]] <- dplyr::lag(df_ext$d_log_ppi, i)
}

# Restrict the extended sample to the same starting point as the original
# paper (1970), but run all the way through to the end of 2007
df_ext_reg <- df_ext %>%
  filter(date >= as.Date("1970-01-01") & date <= as.Date("2007-12-01")) %>%
  mutate(month_num = droplevels(as.factor(format(date, "%m"))))

# Print the number of rows and columns
dim(df_ext_reg)

# Extended output regression, same structure as before but using data
# through 2007 instead of stopping at 1996
shock_terms_36 <- paste0("shock_lag", 1:36)
output_terms_24 <- paste0("output_lag", 1:24)
rhs_ext <- paste(c("month_num", output_terms_24, shock_terms_36), collapse = " + ")
formula_ext <- as.formula(paste("d_log_ip ~", rhs_ext))

model2_ext <- lm(formula_ext, data = df_ext_reg)

# Print extended output regression results
summary(model2_ext)

# Same cumulative response simulation as Figure 2, but using the extended
# sample's coefficients 
b_ext <- coef(model2_ext)[paste0("output_lag", 1:24)]
c_ext <- coef(model2_ext)[paste0("shock_lag", 1:36)]

H <- 48
y_ext <- numeric(H)

for (t in 1:H) {
  ar_part <- 0
  for (i in 1:24) {
    if (t - i >= 1) ar_part <- ar_part + b_ext[i] * y_ext[t - i]
  }
  shock_part <- if (t <= 36) c_ext[t] else 0
  y_ext[t] <- ar_part + shock_part
}

cum_response_ext <- cumsum(y_ext) * 100

# Print the extended sample's output response, to compare 
cum_response_ext

# Extended price regression through 2007
shock_terms_48 <- paste0("shock_lag", 1:48)
price_terms_24 <- paste0("price_lag", 1:24)
rhs_ext_p <- paste(c("month_num", price_terms_24, shock_terms_48), collapse = " + ")
formula_ext_p <- as.formula(paste("d_log_ppi ~", rhs_ext_p))

model3_ext <- lm(formula_ext_p, data = df_ext_reg)

# Print extended price regression results
summary(model3_ext)

# Same cumulative response simulation, but for price in the extended sample
b_ext_p <- coef(model3_ext)[paste0("price_lag", 1:24)]
c_ext_p <- coef(model3_ext)[paste0("shock_lag", 1:48)]

H <- 48
p_ext <- numeric(H)

for (t in 1:H) {
  ar_part <- 0
  for (i in 1:24) {
    if (t - i >= 1) ar_part <- ar_part + b_ext_p[i] * p_ext[t - i]
  }
  shock_part <- if (t <= 48) c_ext_p[t] else 0
  p_ext[t] <- ar_part + shock_part
}

cum_response_ext_p <- cumsum(p_ext) * 100

# Print the extended sample's price response, to compare against the
# original 1970-1996 result
cum_response_ext_p

png("~/Downloads/R-R-replication/output/figure2_output_response.png", width = 800, height = 500)
plot(1:48, cum_response, type = "l", col = "blue",
     main = "Effect of a Monetary Policy Shock on Industrial Production",
     xlab = "Months after shock", ylab = "Percent change in output")
dev.off()

png("~/Downloads/R-R-replication/output/figure4_price_response.png", width = 800, height = 500)
plot(1:48, cum_response_p, type = "l", col = "red",
     main = "Effect of a Monetary Policy Shock on Prices",
     xlab = "Months after shock", ylab = "Percent change in PPI")
dev.off()

