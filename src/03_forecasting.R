# All material for forecasting is taken from https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/


library(plotly)
plot_ly(atveju_skaicius, 
                x= ~Category, y = ~cases_7day,
                name = '2015',
                type = 'scatter',
                mode = 'lines',
                color = I("#204663")) %>%
  layout(title = list(text = 'Number of Covid19 cases detecter in Lithuania'),
         legend = list(title=list(text='<b> Date </b>'),x = 0.82, y = 1, bgcolor = 'rgba(32, 70, 99,0.5)'),
         plot_bgcolor = "#f4fbfa",
         xaxis = list(title = 'Date', rangeslider = list(type = "date")), 
         yaxis = list(title = 'Number of cases')
  )


tsData = ts(atveju_skaicius$V1, start = c(2020,2), frequency = 365)

components.ts = decompose(tsData)
plot(components.ts)

library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)


acf(tsData,lag.max=34) 


fitARIMA <- arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA) 
confint(fitARIMA)

library(forecast)
auto.arima(tsData, trace=TRUE) 


predict(fitARIMA,n.ahead = 5)
futurVal <- forecast(fitARIMA,h=10, level=c(99.5))
plot(futurVal)
