## Final Code


#Setup
setwd("~/Documents/Springboard/Capstone")
raw <- read.csv("raw.csv", header = TRUE, stringsAsFactors = FALSE)
library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)
library(mosaic)
library(choroplethrMaps)
library(choroplethr)

# Snippet of data
head(raw)

# Data wrangling
dim(raw)
str(raw)

# Trim variable names to make them more workable
colnames(raw) <- c("drg", "id", "provider_name", "street_address", "city", "state", "zip", "ref_region", 
                   "total_discharges", "avg_cov_charges", "avg_tot_payments", "avg_medicare_payments")

# Split DRG into 'code' and definition'
hosp_data <- separate(raw,drg, c("code", "definition"), sep="-")
hosp_data$code <- as.factor(hosp_data$code)

# Convert to numeric
hosp_data$avg_cov_charges <- sub("$", " ",hosp_data$avg_cov_charges, fixed=T)
hosp_data$avg_cov_charges <- as.numeric(gsub(',', '', hosp_data$avg_cov_charges))

hosp_data$avg_tot_payments <- sub("$", " ",hosp_data$avg_tot_payments, fixed=T)
hosp_data$avg_tot_payments <- as.numeric(gsub(',', '', hosp_data$avg_tot_payments))

hosp_data$avg_medicare_payments <- sub("$", " ",hosp_data$avg_medicare_payments, fixed=T)
hosp_data$avg_medicare_payments <- as.numeric(gsub(',', '', hosp_data$avg_medicare_payments))
str(hosp_data)



## Subset top 10 diagnosis
drg10 <- c("870 ", "207 ", "853 ", "329 ", "246 ", "460 ", "238 ", "252 ", "469 ", "480 ") 
hosp10 <- hosp_data[hosp_data$code %in% drg10,]
hosp10 <- unite(hosp10, "DRG", code, definition, sep="-", remove = FALSE)

# Summary of variables
summary(hosp_data)

##Summary by state for costliest DRG by state
hosp_top <- hosp_data  %>% filter(code=="870 ")
top_cov_state <- aggregate(avg_cov_charges~code + state, hosp_top, mean)
top_disch_state <- aggregate(total_discharges~code + state, hosp_top, mean)
top_med_state <- aggregate(avg_medicare_payments~code + state, hosp_top, mean)


## Derived variables by code and state
cov_by_state <- aggregate(avg_cov_charges~code + state, hosp10, mean)

## Heatmap
p <- ggplot(cov_by_state, aes(code, state)) + geom_tile(aes(fill = avg_cov_charges), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
base_size <- 8
p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + theme(legend.position = "none", axis.ticks = element_blank(), axis.text.x = element_text(size = base_size * 0.8, angle = 300, hjust = 0))

## Map Cov, payments, differences(percent) for most expensive


## Map for billed by state for costliest diagnosis
percent_cov <- top_cov_state %>% group_by(code) %>% summarise('20%'=quantile(avg_cov_charges, probs=0.20),'40%'=quantile(avg_cov_charges, probs=0.4), '60%'=quantile(avg_cov_charges, probs=0.60), '80%'=quantile(avg_cov_charges, probs=0.80),avg=mean(avg_cov_charges))
percent_cov1 <- mutate(top_cov_state, perc_level = derivedFactor(
  "1" = (avg_cov_charges<=106026.4), "2" = (avg_cov_charges>106026.4 & avg_cov_charges<=123719), "3" = (avg_cov_charges>123719 & avg_cov_charges<=147073.6), "4" = (avg_cov_charges>147073.6 & avg_cov_charges<=171771.3), "5" = (avg_cov_charges>171771.3),
  .method = "first",
  .default = 0
))
data("state.regions")
percent_cov2 <- select(percent_cov1, state, perc_level)
percent_cov2$perc_level <- as.numeric(as.factor(percent_cov2$perc_level))
names(percent_cov2) <- c("region", "value")
percent_cov2$region <- state.regions[match(percent_cov2$region, state.regions$abb),1]
c <- state_choropleth(percent_cov2, legend = "Percentile Value", num_colors = 1)


## Map for medicare payments by state for costliest diagnosis
percent_med <- top_med_state %>% group_by(code) %>% summarise('20%'=quantile(avg_medicare_payments, probs=0.20),'40%'=quantile(avg_medicare_payments, probs=0.4), '60%'=quantile(avg_medicare_payments, probs=0.60), '80%'=quantile(avg_medicare_payments, probs=0.80),avg=mean(avg_medicare_payments))
percent_med1 <- mutate(top_med_state, perc_level = derivedFactor(
  "1" = (avg_medicare_payments<=36654.46), "2" = (avg_medicare_payments>36654.46 & avg_medicare_payments<=37748.82), "3" = (avg_medicare_payments>37748.82 & avg_medicare_payments<=40926.78), "4" = (avg_medicare_payments>40926.78 & avg_medicare_payments<=46493.76), "5" = (avg_medicare_payments>46493.76),
  .method = "first",
  .default = 0
))
data("state.regions")
percent_med2 <- select(percent_med1, state, perc_level)
percent_med2$perc_level <- as.numeric(as.factor(percent_med2$perc_level))
names(percent_med2) <- c("region", "value")
percent_med2$region <- state.regions[match(percent_med2$region, state.regions$abb),1]
m <- state_choropleth(percent_med2, legend = "Percentile Value", num_colors = 1)


### Percentage of paid of billed
diff_top <-cbind(top_cov_state, avg_medicare_payments = top_med_state$avg_medicare_payments)


percentage_top_state <- mutate(diff_top, percent_payment=((avg_cov_charges - avg_medicare_payments)/avg_cov_charges*100))
percent_diff <- percentage_top_state %>% group_by(code) %>% summarise('20%'=quantile(percent_payment, probs=0.20),'40%'=quantile(percent_payment, probs=0.4), '60%'=quantile(percent_payment, probs=0.60), '80%'=quantile(percent_payment, probs=0.80),avg=mean(percent_payment))
percent_diff1 <- mutate(percentage_top_state, perc_level = derivedFactor(
  "1" = (percent_payment<=61.80235), "2" = (percent_payment>61.80235 & percent_payment<=67.55472), "3" = (percent_payment>67.55472 & percent_payment<=72.42385), "4" = (percent_payment>72.42385 & percent_payment<=77.30398), "5" = (percent_payment>77.30398),
  .method = "first",
  .default = 0
))
data("state.regions")
percent_diff2 <- select(percent_diff1, state, perc_level)
percent_diff2$perc_level <- as.numeric(as.factor(percent_diff2$perc_level))
names(percent_diff2) <- c("region", "value")
percent_diff2$region <- state.regions[match(percent_diff2$region, state.regions$abb),1]
p <- state_choropleth(percent_diff2, legend = "Percentile Value", num_colors = 1)










