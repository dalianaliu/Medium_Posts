library(ggplot2)
library(RJDBC)
library(stringr)
library(data.table)
library(gdata)
library(plyr)
library(reshape2)
library(lubridate)
library(zoo)
library(forecast)

rm(list=ls())

### 1.Process Data and Demostrate Example ###
zillow_file<-"Your_Directory/zillow_data.csv" 
# zillow_data.csv is the file you download here https://www.zillow.com/research/data/#other-metrics #
# choose the file that is "Median List Price ($)" and column is "city" #
rent_1b_wide<-fread(zillow_file) #updated till end of 2017

col_all<-colnames(rent_1b_wide)
rent_1b<-melt(rent_1b_wide, id.vars=col_all[c(1:5)])
setnames(rent_1b, old=c("variable","value"), new=c("time","rent"))
rent_1b[,id:=paste(RegionName,CountyName,Metro,sep="_")]
rent_1b[,year:=as.numeric(str_split_fixed(rent_1b$time,"-",2)[,1])][,mon:=as.numeric(str_split_fixed(rent_1b$time,"-",2)[,2])]
rent_1b$time_var = as.Date(as.yearmon(paste(rent_1b$year, rent_1b$mon,"01", sep = "-")))

# Use Sunnyvale as example #
head(rent_1b[RegionName == "Sunnyvale"][!is.na(rent)])
# Observe that data start from 2012-07
test_d<-rent_1b[RegionName == "Sunnyvale"][!is.na(rent)]$rent
# Tranform data into time seires format
test_ts<-ts(test_d, start = c(2012, 7), frequency = 12)
# plot time series
plot(test_ts)

# plot the season, trend and randomness decomposition
plot(stl(test_ts, s.window="period"))

# plot forecast using stlf
forecast_stl<-stlf(test_ts, s.window="period")
plot(forecast_stl)

### 2. Create Function to Analyze Each City ###
forecast_result_table<-function(long_data,list,dir){
  #rm(list = ls())
  trend_table<-data.table()
  for(i in 1:length(list)){
    region_data<-long_data[id==list[i]][!is.na(rent)]
    if(nrow(region_data)>=36){
      # Only look at cities with at least 3 years' data #
      uid = list[i]
      print(paste(uid,">=36",sep=" "))
      start_year<-min(region_data$year)
      start_mon<-min(region_data$mon)  
      region_ts<-ts(region_data$rent,start=c(start_year,start_mon),frequency=12)
      
      region_fit <- tslm(region_ts ~ trend + season)
      region_coef<-data.table(summary(region_fit)$coefficients)
      names(region_coef)<-c("est","stde","t_value","p_value")
      region_coef[,if_sig:=ifelse(p_value<0.05,1,0)]
      sig_num<-sum((region_coef[3:13]$if_sig))
      r2_adj<-summary(region_fit)[[9]]
      slope_est<-region_coef[2]$est
      slope_p<-region_coef[2]$p_value 
      
      ts_fit <- stlm(region_ts, s.window="period")$fitted
      forecast_ts<-stlf(region_ts, s.window="period",h=24, level = 95)
      for_values<-data.frame(time=round(time(forecast_ts$mean),  3),  value_forecast=as.data.frame(forecast_ts$mean),  dev=as.data.frame(forecast_ts$upper)-as.data.frame(forecast_ts$mean))
      names(for_values)<-c("time","value_forecast","dev") 
      yr_increase<-for_values[13,]$value_forecast-for_values[1,]$value_forecast
      
      rt_201801<-round(for_values[1,]$value_forecast)
      rt_201901<-round(for_values[13,]$value_forecast)
      city<-region_data[1]$RegionName
      metro<-region_data[1]$Metro
      state<-region_data[1]$State
      temp_table<-data.table(t(c(uid,city,metro,state,sig_num,yr_increase,rt_201801,rt_201901,slope_est,slope_p,r2_adj)))
      trend_table<-rbindlist(list(trend_table,temp_table))
    }
  }
  names(trend_table)<-c("uid","city","metro","state","season_num","yr_increase","rt_18","rt_19","prev_slope","slope_p","model_p")
  return(trend_table)
}

id_list<-unique(rent_1b$id)
long_form_data<-rent_1b
dir = "your_directory"
result_table<-forecast_result_table(rent_1b,id_list,dir)

# Top 20 Cities with greated predicted increase in rent
list_increase<-result_table[order(-as.numeric(yr_increase))][1:20]

# 20 Cities with no predicted change
list_flat<-result_table[order(abs(as.numeric(yr_increase)))][1:20]


### 3. Use ggplot2 for Decomposition Plot ###
library(ggseas)

season_plt<-ggsdc(test_d, aes(x = time_var, y = rent),method = "decompose", start = c(test_d[1]$year, test_d[1]$mon), frequency = 12,
                  facet.titles = c("Actual Rent", "Trend", "Seasonality", "Randomness")) +
  theme(strip.text = element_text(size= 15)) +
  geom_line( colour = "lightcoral", size = 0.6) +
  labs(x = "Time") +
  labs(y = "Rent") +
  ggtitle("Time Series Decomposition") +
  theme(plot.title = element_text(size=20,hjust = 0.5))

plot_dir = "your directory to save plots"
png(paste0(plot_dir,"A_decomposition.png"),
    width     = 6,
    height    = 6,
    units     = "in",
    res       = 1200,
    pointsize = 0.5)
print(season_plt)
dev.off()

### 4. Use ggplot to Plot Cities' Rent Increase ###
gg_forecast_plot<-function(dataset,id_x){
  inc_table<-data.table()
  data_i<-dataset[id == id_x][!is.na(rent)]
  data_ts<-ts(data_i$rent, start = c(min(data_i$year), data_i[1]$mon), frequency = 12)
  
  ts_fit <- stlm(data_ts, s.window="period")$fitted
  forecast_ts<-stlf(data_ts, s.window="period",h=24, level = 95)
  for_values<-data.frame(dt=as.Date(time(forecast_ts$mean)),  value_forecast=as.data.frame(forecast_ts$mean),  dev=as.data.frame(forecast_ts$upper)-as.data.frame(forecast_ts$mean))
  names(for_values)<-c("dt","value_forecast","dev")
  fitted_values<-data.frame(dt=as.Date(time(forecast_ts$fitted)),  value_fitted=as.data.frame(forecast_ts$fitted)$x)
  actual_values<-data.frame(dt=as.Date(time(forecast_ts$x)),  Actual=c(forecast_ts$x))
  
  yr_increase<-for_values[13,]$value_forecast-for_values[1,]$value_forecast
  tmp_table<-data.table(t(c(id_x,yr_increase)))
  inc_table<-rbindlist(list(inc_table,tmp_table))
  
  graphset<-merge(actual_values,  fitted_values,  by='dt',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='dt')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  graphset.melt<-melt(graphset[, c('dt', 'Actual', 'Fitted')], id='dt')
  graphset.melt$time<-c(time(forecast_ts$x),time(forecast_ts$mean))
  graphset$time<-c(time(forecast_ts$x),time(forecast_ts$mean))
  graphset<-data.table(graphset)
  #plot#
  n.ahead=4
  CI=.95
  error.ribbon='aquamarine4'
  line.size=1
  line_tp = "solid"
  dark_c = "mediumorchid2"
  light_c = "lightpink"
  line_sz = 0.8
  mon_increase = round(as.numeric(result_table1[uid == id_x]$yr_increase,0))
  p<-ggplot(data = graphset.melt,aes(dt,value)) + 
    geom_line(aes(colour=factor(variable),size=factor(variable)))+
    scale_color_manual(values=c("#666666","#33CCCC")) + 
    scale_size_manual(values=c(0.8, 1.0))+
    geom_ribbon(data=graphset, aes(x=dt, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + 
    #geom_vline(xintercept=max(as.Date(time(forecast_ts$x))),  lty=2) + 
    xlab('Time') + 
    ylab('Rent') + 
    scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
    labs( title= ifelse(data_i$Metro==data_i$RegionName,paste(data_i$RegionName,sep=""),paste(data_i$RegionName," (",data_i$Metro, " Metro)" ,sep="")),
          subtitle=paste0("Monthly rent is estimated to increase $",mon_increase," after a year; ",
                          mon_increase," x 12 = $",mon_increase*12))+
    theme(legend.title=element_blank())+ 
    theme(plot.title = element_text(size=19,hjust = 0.5)) +
    theme(plot.subtitle=element_text(size=16, hjust=0.5, color="mediumorchid3")) + 

    theme(legend.key.width = unit(0.75,"cm")) +
    theme(legend.title=element_blank())+ 
    theme(legend.text = element_text(size = 12)) +
    theme(axis.text=element_text(size=12, face="bold"),)+
    
    geom_segment(aes(x = as.Date(graphset[dt=="2019-01-01"]$dt),
                     y = floor(graphset[dt=="2018-01-01"]$Fitted),
                     xend = as.Date(graphset[dt=="2019-01-01"]$dt),
                     yend = floor(graphset[dt=="2019-01-01"]$Fitted)), color= dark_c,lty=line_tp, size = line_sz) +
    geom_segment(aes(x = as.Date(graphset[dt=="2019-12-01"]$dt),
                     y = floor(graphset[dt=="2018-01-01"]$Fitted),
                     xend = as.Date(graphset[dt=="2019-12-01"]$dt),
                     yend = floor(graphset[dt=="2019-01-01"]$Fitted)), color=dark_c,lty=line_tp, size =line_sz)  +
    geom_segment(aes(x = as.Date(graphset[dt=="2019-01-01"]$dt),
                     y = floor(graphset[dt=="2018-01-01"]$Fitted),
                     xend = as.Date(graphset[dt=="2019-12-01"]$dt),
                     yend = floor(graphset[dt=="2018-01-01"]$Fitted)), color=dark_c,lty=line_tp, size =line_sz) +
    geom_segment(aes(x = as.Date(graphset[dt=="2019-01-01"]$dt),
                     y = floor(graphset[dt=="2019-01-01"]$Fitted),
                     xend = as.Date(graphset[dt=="2019-12-01"]$dt),
                     yend = floor(graphset[dt=="2019-01-01"]$Fitted)), color=dark_c,lty=line_tp, size =line_sz) +
    
    geom_segment(aes(x = as.Date(graphset[dt=="2018-01-01"]$dt),
                     y = floor(min(graphset$Fitted)),
                     xend = as.Date(graphset[dt=="2018-01-01"]$dt),
                     yend = floor(graphset[dt=="2018-01-01"]$Fitted)), color=light_c,lty=line_tp, size =line_sz) +
    
    geom_segment(aes(x = as.Date(graphset[dt=="2018-01-01"]$dt),
                     y = floor(min(graphset$Fitted)),
                     xend = as.Date(graphset[dt=="2018-01-01"]$dt),
                     yend = floor(graphset[dt=="2018-01-01"]$Fitted)), color=light_c,lty=line_tp, size =line_sz) +
    
    geom_segment(aes(x = as.Date(graphset[dt=="2018-01-01"]$dt),
                     y = floor(min(graphset$Fitted)),
                     xend = as.Date(graphset[dt=="2018-01-01"]$dt),
                     yend = floor(graphset[dt=="2018-01-01"]$Fitted)), color=light_c,lty=line_tp, size =line_sz) +
    
    geom_segment(aes(x = as.Date(graphset[dt=="2019-12-01"]$dt),
                     y = floor(min(graphset$Fitted)),
                     xend = as.Date(graphset[dt=="2019-12-01"]$dt),
                     yend = floor(graphset[dt=="2018-01-01"]$Fitted)), color=light_c,lty=line_tp, size =line_sz) +
    geom_segment(aes(x = as.Date(graphset[dt=="2018-01-01"]$dt),
                     y =  floor(graphset[dt=="2018-01-01"]$Fitted),
                     xend = as.Date(graphset[dt=="2019-01-01"]$dt),
                     yend = floor(graphset[dt=="2018-01-01"]$Fitted)), color=light_c,lty=line_tp, size =line_sz) +
    geom_text(data = subset(graphset, dt%in%c(as.Date("2018-01-01"),as.Date("2019-01-01"))), aes(y = Fitted, label=round(Fitted)), vjust = -0.4, hjust = 0.2 ,size = 5 )#, fontface ="bold")           
          
  return(p)
}

# Get the cities that you are interested in by whether their rents are increasing from Step 2's results
# Top 20 Cities with greated predicted increase in rent
list_increase<-result_table[order(-as.numeric(yr_increase))][1:20]
id_increase<-list_increase$uid

# 20 Cities with no predicted change
list_flat<-result_table[order(abs(as.numeric(yr_increase)))][1:20]
id_flat<-list_increase$uid

id_list =  list_flat # or id_flat

for(i in (1:length(id_list))){
  city_plot<-gg_forecast(rent_1b,id_list[i])
  id1<-gsub("/", "_", id_list[i])
  id1<-gsub(" ", "_", id_list[i])
  png(paste0(dir,id1,".png"),
      width     = 8,
      height    = 6,
      units     = "in",
      res       = 1200,
      pointsize = 0.5)
  print(city_plot)
  dev.off()
}

### 5. Use ggplot for the Final Line Segment Plot ###
data<-result_table[order(-as.numeric(yr_increase))][1:20][order(as.numeric(rt_19))]
data$category <- as.character(data$plot_nm)
data$category <- reorder(data$plot_nm, as.numeric(data$rt_19))
dark_c = "mediumorchid2"
light_c = "lightpink"

seg_plot<-ggplot(data, aes(y = category)) +
  labs(x = "Rent",y = "City") +
  geom_segment(aes(x = as.numeric(data$rt_18),
                   y = category,
                   xend = as.numeric(data$rt_19),
                   yend = category),
               size = 1) +
 geom_point(aes(x =as.numeric(data$rt_18),
               color = "2018"),
           size = 1.5, shape = 15) +
  geom_point(aes(x = as.numeric(data$rt_19),
                 color = "2019"),
             size = 1.5, shape = 15) +  
  scale_color_discrete(name = "Year  ") +
  theme(legend.position = "bottom") +
  geom_text(data= data[,list(category,rt_19,yr_increase)], aes(x=as.numeric(data$rt_19), y = category, label=paste("$",round(as.numeric(yr_increase)),sep="")), hjust = -0.2 ,size = 3.2 )+#, fontface ="bold")  
  labs( title= "Top 20 Cities with the greatest leap of rent next year")+
  theme(legend.title=element_blank())+ 
  theme(plot.title = element_text(size=19,hjust = 0.5)) +  
  theme(legend.key.width = unit(0.75,"cm")) +
  theme(legend.title=element_blank())+ 
  theme(legend.text = element_text(size = 12)) +
  theme(axis.text=element_text(size=10, face="bold"),)

png(paste0(plot_dir,"All20.png"),
    width     = 10,
    height    = 6,
    units     = "in",
    res       = 1200,
    pointsize = 0.5)
print(seg_plot)
dev.off()
