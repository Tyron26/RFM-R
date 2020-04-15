#install.packages("rfm")
library(rfm)

DT = read.csv("OnlineRetail.csv")
str(DT)

DT[, 2] <- as.Date(as.character(DT[,2]),"%Y%m%d")
str(DT)

RFMobj <- rfm_table_order(DT, customer_id = ID, order_date = Date,
                          revenue = Amount, analysis_date = max(DT$Date))
RFMDT = RFMobj$rfm
View(RFMDT)

rfm_rm_plot(RFMobj)
rfm_fm_plot(RFMobj)
rfm_rf_plot(RFMobj)

rfm_heatmap(RFMobj)
rfm_bar_chart(RFMobj)

################################ 
## Predicting retention Rate
################################

DT = DT[order(DT$Date,decreasing = TRUE), ]

History_endDate =  as.Date("2018-05-31")

Forecast_endDate =  as.Date("2018-06-30")

################################ 
## Split the data into history and forecast
################################
HistDT = DT[DT$Date <= History_endDate, ]

ForecastDT = DT[DT$Date > History_endDate, ] 

Hist_RFM = rfm_table_order(data = HistDT, 
                           customer_id = ID, 
                           order_date = Date,
                           revenue = Amount, 
                           analysis_date = History_endDate)

Hist_RFMDT = Hist_RFM$rfm

ForecastID = ForecastDT$ID[!duplicated(ForecastDT$ID)]

# Generate the indicator whether the customer buy anything from the website.

Hist_RFMDT$Buy = ifelse(Hist_RFMDT$customer_id %in% ForecastID, 1, 0)

Reg = lm(Buy~ recency_score + frequency_score +
           monetary_score, data = Hist_RFMDT)

summary(Reg)

Input = data.frame(recency_score = c(5,1,1), 
                   frequency_score = c(1,5,1), 
                   monetary_score = c(1,1,5))

predict(Reg, Input) 


Input = data.frame(recency_score = c(1), 
                   frequency_score = c(1), 
                   monetary_score = c(1))

predict(Reg, Input) 

## Running Logistic Regression

LogitReg = glm(Buy~recency_score+frequency_score+ monetary_score,family=quasibinomial(link='logit'),data=Hist_RFMDT)
summary(LogitReg)

predict(LogitReg, Input, type="response") 