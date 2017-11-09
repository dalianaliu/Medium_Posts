library(ggplot2)
library(stringr)
library(data.table)
library(gdata)
library(plyr)
library(reshape2)
library(lubridate)
library(zoo)
library(ggrepel)
rm(list=ls())

### Functions to use a linear regression model to fit each city's data ###
#   Count how many months are significant from the month with the lowest rent  #
batch_rent<-function(data_set){
  col_all<-colnames(data_set)
  region_rent<-list()
  seasonal_list<-data.table()
  for (i in 1:nrow(data_set)){
    region_data<-data_set$id
    assign(region_data,data_set[i])
    region_rent[[i]]<-melt(get(region_data), id.vars=c(col_all[c(1:5)],"id"))[!is.na(value)]
    print(region_rent[[i]]$id)
    if(nrow(region_rent[[i]])>=36){
      setnames(region_rent[[i]], old=c("variable","value"), new=c("time","rent"))
      region_rent[[i]][,year:=as.numeric(str_split_fixed(region_rent[[i]]$time,"-",2)[,1])][,mon:=as.numeric(str_split_fixed(region_rent[[i]]$time,"-",2)[,2])]
      region_rent[[i]]$time1 = as.Date(as.yearmon(paste(region_rent[[i]]$year, region_rent[[i]]$mon,"01", sep = "-")))
      
      mm<-region_rent[[i]][,mean(rent),by=mon][order(mon)]
      mm[,min_rent:=min(V1)][,max_rent:=max(V1)][,ifmin:=ifelse(V1==min_rent,0,1)]
      m<-t(mm$ifmin)
      
      year_min<-min(region_rent[[i]]$year)
      region_rent[[i]][,t:=(year-year_min)*12+mon]
      region_rent[[i]][,t_2:=t*t]
      region_rent[[i]][,m1:=m[1]*ifelse(mon==1,1,0)]
      region_rent[[i]][,m2:=m[2]*ifelse(mon==2,1,0)]
      region_rent[[i]][,m3:=m[3]*ifelse(mon==3,1,0)]
      region_rent[[i]][,m4:=m[4]*ifelse(mon==4,1,0)]
      region_rent[[i]][,m5:=m[5]*ifelse(mon==5,1,0)]
      region_rent[[i]][,m6:=m[6]*ifelse(mon==6,1,0)]
      region_rent[[i]][,m7:=m[7]*ifelse(mon==7,1,0)]
      region_rent[[i]][,m8:=m[8]*ifelse(mon==8,1,0)]
      region_rent[[i]][,m9:=m[9]*ifelse(mon==9,1,0)]
      region_rent[[i]][,m10:=m[10]*ifelse(mon==10,1,0)]
      region_rent[[i]][,m11:=m[11]*ifelse(mon==11,1,0)]
      region_rent[[i]][,m12:=m[12]*ifelse(mon==12,1,0)]
      
      #model0 = lm(rent ~ t+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12, data = region_rent[[i]])
      model1 = lm(rent ~ t+t_2+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12, data = region_rent[[i]])
      
      region_rent[[i]]$m1_fitted <-model1$fitted.values
      #region_rent[[i]][,m1_fittied:=model1$coefficients[1] + model1$coefficients[2]*t + model1$coefficients[3]*m2 +  model1$coefficients[4]*m3 +  model1$coefficients[5]*m4 +  model1$coefficients[6]*m5+ model1$coefficients[7]*m6 +  model1$coefficients[8]*m7 +  model1$coefficients[9]*m8 +   model1$coefficients[10]*m9 +  model1$coefficients[11]*m10 +  model1$coefficients[12]*m11 + model1$coefficients[13]*m12  ]
      region_rent[[i]][,seasonal:=m1_fitted - model1$coefficients[2]*t - model1$coefficients[3]*t_2 ]
      base<-min(region_rent[[i]]$rent)
      region_rent[[i]][,trend:=model1$coefficients[1] + model1$coefficients[2]*t + model1$coefficients[3]*t_2 ]
      
      seasonal_list_temp<-data.table(t(c(model1$coefficients[1:3],0,model1$coefficients[4:14])))
      setnames(seasonal_list_temp,c("intct","slope1","slope2","1","2","3","4","5","6","7","8","9","10","11","12"))
      seasonal_list_temp$RegionName<-region_rent[[i]][1]$RegionName
      seasonal_list_temp$SizeRank<-region_rent[[i]][1]$SizeRank
      seasonal_list_temp$CountyName<-region_rent[[i]][1]$CountyName
      seasonal_list_temp$King<-region_rent[[i]][1]$King
      
      seasonal_list_temp$max_coef<-max(model1$coefficients[4:14])
      seasonal_list_temp$min_coef<-min(model1$coefficients[4:14])
      
      seasonal_list_temp$max_mon<-mm[V1==max_rent]$mon
      seasonal_list_temp$min_mon<-mm[V1==min_rent]$mon
      seasonal_list_temp$max_rent<-mm[V1==max_rent]$V1
      seasonal_list_temp$min_rent<-mm[V1==min_rent]$V1
      seasonal_list_temp[,t_pvalue:=coef(summary(model1))[, "Pr(>|t|)"][2]][,t2_pvalue:=coef(summary(model1))[, "Pr(>|t|)"][3]]
      n_coef<-nrow(coef(summary(model1)))
      seasonal_list_temp[,n_sig_month:=sum(coef(summary(model1))[, "Pr(>|t|)"][4:n_coef]<=0.05)]
      seasonal_list_temp<-cbind(seasonal_list_temp,t(coef(summary(model1))[, "Pr(>|t|)"][4:n_coef]))
      
      seasonal_list<-rbindlist(list(seasonal_list_temp,seasonal_list),use.names=TRUE,fill=TRUE)
    }
  }
  return(seasonal_list)
}

get_region_rent<-function(data_set){
  col_all<-colnames(data_set)
  region_rent<-list()
  seasonal_list<-data.table()
  rent_all<-list()
  for (i in 1:nrow(data_set)){
    region_data<-data_set$id
    assign(region_data,data_set[i])
    region_rent[[i]]<-melt(get(region_data), id.vars=c(col_all[c(1:5)],"id"))[!is.na(value)]
    print(region_rent[[i]]$id)
    if(nrow(region_rent[[i]])>=36){
      setnames(region_rent[[i]], old=c("variable","value"), new=c("time","rent"))
      region_rent[[i]][,year:=as.numeric(str_split_fixed(region_rent[[i]]$time,"-",2)[,1])][,mon:=as.numeric(str_split_fixed(region_rent[[i]]$time,"-",2)[,2])]
      region_rent[[i]]$time1 = as.Date(as.yearmon(paste(region_rent[[i]]$year, region_rent[[i]]$mon,"01", sep = "-")))
      
      mm<-region_rent[[i]][,mean(rent),by=mon][order(mon)]
      mm[,min_rent:=min(V1)][,max_rent:=max(V1)][,ifmin:=ifelse(V1==min_rent,0,1)]
      m<-t(mm$ifmin)
      
      year_min<-min(region_rent[[i]]$year)
      region_rent[[i]][,t:=(year-year_min)*12+mon]
      region_rent[[i]][,t_2:=t*t]
      region_rent[[i]][,m1:=m[1]*ifelse(mon==1,1,0)]
      region_rent[[i]][,m2:=m[2]*ifelse(mon==2,1,0)]
      region_rent[[i]][,m3:=m[3]*ifelse(mon==3,1,0)]
      region_rent[[i]][,m4:=m[4]*ifelse(mon==4,1,0)]
      region_rent[[i]][,m5:=m[5]*ifelse(mon==5,1,0)]
      region_rent[[i]][,m6:=m[6]*ifelse(mon==6,1,0)]
      region_rent[[i]][,m7:=m[7]*ifelse(mon==7,1,0)]
      region_rent[[i]][,m8:=m[8]*ifelse(mon==8,1,0)]
      region_rent[[i]][,m9:=m[9]*ifelse(mon==9,1,0)]
      region_rent[[i]][,m10:=m[10]*ifelse(mon==10,1,0)]
      region_rent[[i]][,m11:=m[11]*ifelse(mon==11,1,0)]
      region_rent[[i]][,m12:=m[12]*ifelse(mon==12,1,0)]
      
      model1 = lm(rent ~ t+t_2+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12, data = region_rent[[i]])
      
      region_rent[[i]]$m1_fitted <-model1$fitted.values
      region_rent[[i]][,seasonal:=m1_fitted - model1$coefficients[2]*t - model1$coefficients[3]*t_2 ]
      base<-min(region_rent[[i]]$rent)
      region_rent[[i]][,trend:=model1$coefficients[1] + model1$coefficients[2]*t + model1$coefficients[3]*t_2 ]
      
      rent_all<-rbindlist(list(rent_all,region_rent[[i]]),use.names=T,fill=T)
      
    }
  }
  return(rent_all)
}

### Functions to plot all the cities to visualize the seasonality 
get_city_plot<-function(city_data,id){
  adj = ifelse(min(city_data$rent) - min(city_data$seasonal), min(city_data$rent) - min(city_data$seasonal),0)
  seasonal_min=min(city_data$seasonal)
  seasonal_max=max(city_data$seasonal)
  low_m <-city_data[seasonal==seasonal_min]$mon
  high_m <-city_data[seasonal==seasonal_max]$mon
  city_plot<-ggplot(city_data, aes(x = time1)) + 
    geom_line(aes(y = trend),colour ="#CCCCCC", size = 1.2) +
    geom_line(aes(y = rent),colour="#666666", size = 0.8) +
    geom_line(aes(y = m1_fitted), colour = "#33CCCC",,size = 1.5) + 
    geom_text(data = subset(city_data, mon%in%c(high_m)), aes(y = m1_fitted, label=mon), vjust = -2, size = 5, colour ="#FF6666" , fontface ="bold") +
    geom_text(data = subset(city_data, mon%in%c(low_m)), aes(y = m1_fitted, label=mon), vjust = 2, size = 5, colour ="#33CCFF" , fontface ="bold") +
    ylim(min(city_data$rent-50), max(city_data$rent+50)) +
    ylab(label="Rent") + 
    xlab(label = "Month") +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y-%m") +
    ggtitle(paste(city_data$RegionName)) +
    theme(plot.title = element_text(size=50,hjust = 0.5))
  return(city_plot)
}

batch_plot<-function(data_set,dir){
  col_all<-colnames(data_set)
  region_rent<-list()
  seasonal_list<-data.table()
  for (i in 1:nrow(data_set)){
    region_data<-data_set$id
    assign(region_data,data_set[i])
    region_rent[[i]]<-melt(get(region_data), id.vars=c(col_all[c(1:5)],"id"))[!is.na(value)]
    if(nrow(region_rent[[i]])>=36){
      setnames(region_rent[[i]], old=c("variable","value"), new=c("time","rent"))
      region_rent[[i]][,year:=as.numeric(str_split_fixed(region_rent[[i]]$time,"-",2)[,1])][,mon:=as.numeric(str_split_fixed(region_rent[[i]]$time,"-",2)[,2])]
      region_rent[[i]]$time1 = as.Date(as.yearmon(paste(region_rent[[i]]$year, region_rent[[i]]$mon,"01", sep = "-")))
      
      mm<-region_rent[[i]][,mean(rent),by=mon][order(mon)]
      mm[,min_rent:=min(V1)][,max_rent:=max(V1)][,ifmin:=ifelse(V1==min_rent,0,1)]
      m<-t(mm$ifmin)
      
      year_min<-min(region_rent[[i]]$year)
      region_rent[[i]][,t:=(year-year_min)*12+mon]
      region_rent[[i]][,t_2:=t*t]
      region_rent[[i]][,m1:=m[1]*ifelse(mon==1,1,0)]
      region_rent[[i]][,m2:=m[2]*ifelse(mon==2,1,0)]
      region_rent[[i]][,m3:=m[3]*ifelse(mon==3,1,0)]
      region_rent[[i]][,m4:=m[4]*ifelse(mon==4,1,0)]
      region_rent[[i]][,m5:=m[5]*ifelse(mon==5,1,0)]
      region_rent[[i]][,m6:=m[6]*ifelse(mon==6,1,0)]
      region_rent[[i]][,m7:=m[7]*ifelse(mon==7,1,0)]
      region_rent[[i]][,m8:=m[8]*ifelse(mon==8,1,0)]
      region_rent[[i]][,m9:=m[9]*ifelse(mon==9,1,0)]
      region_rent[[i]][,m10:=m[10]*ifelse(mon==10,1,0)]
      region_rent[[i]][,m11:=m[11]*ifelse(mon==11,1,0)]
      region_rent[[i]][,m12:=m[12]*ifelse(mon==12,1,0)]
      
      model1 = lm(rent ~ t+t_2+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12, data = region_rent[[i]])
      
      region_rent[[i]]$m1_fitted <-model1$fitted.values
      region_rent[[i]][,seasonal:=m1_fitted - model1$coefficients[2]*t - model1$coefficients[3]*t_2 ]
      base<-min(region_rent[[i]]$rent)
      region_rent[[i]][,trend:=model1$coefficients[1] + model1$coefficients[2]*t + model1$coefficients[3]*t_2 ]
      
      city_plot<-get_city_plot(region_rent[[i]],region_rent[[i]]$id)
      png(
        paste0(dir,region_rent[[i]]$id,".png"),
        width     = 10,
        height    = 8,
        units     = "in",
        res       = 800,
        pointsize = 0.5
      )
      print(city_plot)
      dev.off()
      
      seasonal_list_temp<-data.table(t(c(model1$coefficients[1:3],0,model1$coefficients[4:14])))
      setnames(seasonal_list_temp,c("intct","slope1","slope2","1","2","3","4","5","6","7","8","9","10","11","12"))
      seasonal_list_temp$RegionName<-region_rent[[i]][1]$RegionName
      seasonal_list_temp$SizeRank<-region_rent[[i]][1]$SizeRank
      seasonal_list_temp$CountyName<-region_rent[[i]][1]$CountyName
      seasonal_list_temp$King<-region_rent[[i]][1]$King
      
      seasonal_list_temp$max_coef<-max(model1$coefficients[4:14])
      seasonal_list_temp$min_coef<-min(model1$coefficients[4:14])
      
      seasonal_list_temp$max_mon<-mm[V1==max_rent]$mon
      seasonal_list_temp$min_mon<-mm[V1==min_rent]$mon
      seasonal_list_temp$max_rent<-mm[V1==max_rent]$V1
      seasonal_list_temp$min_rent<-mm[V1==min_rent]$V1
      seasonal_list_temp[,t_pvalue:=coef(summary(model1))[, "Pr(>|t|)"][2]][,t2_pvalue:=coef(summary(model1))[, "Pr(>|t|)"][3]]
      n_coef<-nrow(coef(summary(model1)))
      seasonal_list_temp[,n_sig_month:=sum(coef(summary(model1))[, "Pr(>|t|)"][4:n_coef]<=0.05)]
      seasonal_list_temp<-cbind(seasonal_list_temp,t(coef(summary(model1))[, "Pr(>|t|)"][4:n_coef]))
      
      seasonal_list<-rbindlist(list(seasonal_list_temp,seasonal_list),use.names=TRUE,fill=TRUE)
    }
  }
  return(seasonal_list)
}

### Process Data ###
zillow_file<-"Your_Directory/zillow_data.csv" 
# zillow_data.csv is the file you download here https://www.zillow.com/research/data/#other-metrics #
# choose the file whose row is "Median List Price ($)" and column is "city" #
# you can play with different data but might need to change some columns of the functions I created below #
rent_1b_wide<-fread(zillow_file) 
rent_1b_wide[,id:=paste(RegionName,CountyName,sep="_")]
rent_1b_wide_top<-rent_1b_wide[SizeRank<=100] 
col_all<-colnames(rent_1b_wide_top)

### Fit the linear regression model for all the cities ###
#   Create a column called "n_sig_month" to count how many months' rents are statistically significantly different from the month with lowest rent  #
rent_results_top<-batch_rent(rent_1b_wide_top)

### Select the cities with at least 3 months' rents significantly different from the month with lowest rent ###
#   Visualize the seasonality, trend and actualy rent for these city to examine the assumptions that they show strong seasonality  #
season_dir<-"Your directory to save these plots from cities might have stronger seasonality"
rent_results_top[order(-n_sig_month)]
season_list<-rent_results_top[n_sig_month>=3][order(-n_sig_month)]$id
batch_plot(rent_1b_wide_top[id%in%season_list],season_dir)

###  Create the plots as my Medium post  ###
#    This part requires manual adjustment to make the bar plot fit well at the lower right corner of the seasonality lines.    #
dir1<-"Your directory to save the plots below"
season_plot_publish<-function(city_data,id,season_y_low,season_y_high,plot_left_date,plot_right_date,plot_low,plot_high){
    adj = ifelse(min(city_data$rent) - min(city_data$seasonal), min(city_data$rent) - min(city_data$seasonal),0)
    seasonal_min=min(city_data$seasonal)
    seasonal_max=max(city_data$seasonal)
    low_m <-city_data[seasonal==seasonal_min]$mon
    high_m <-city_data[seasonal==seasonal_max]$mon
  
    season_data = city_data[year=="2016"]
    season_data[,season_max:=max(seasonal),by="id"]
    season_data[,season_min:=min(seasonal),by="id"]
    season_data[,season_gap:=(season_max-season_min)]
    rent_gap = round(season_data[1]$season_gap)
  
    season_plot<-ggplot(season_data, aes(x=mon, y=seasonal)) +
    geom_bar(stat = "identity",fill= "#33CCCC") +
    geom_text(data = subset(season_data, mon%in%c(high_m)), aes(y = seasonal, label="High"), vjust = -1, size = 3, colour ="black" , fontface ="bold") +
    geom_text(data = subset(season_data, mon%in%c(low_m)), aes(y = seasonal, label="Low"), vjust = -1, size = 3, colour ="black" , fontface ="bold") +
    scale_x_discrete(name ="Month", 
                     limits=c(season_data$mon))+
    coord_cartesian(ylim = c(season_y_low, season_y_high))+
    ylab(label="Rent") + 
    xlab(label = "Month") +
    ggtitle("Seasonality") +
    theme(plot.title = element_text(size=15,hjust = 0.5))
  
    plot_data_wide <- city_data[,list(time1,rent,m1_fitted,trend)]
    names(plot_data_wide) <- c("time1","Actual","Simulated","Trend")
  
    city_data_long<-melt(plot_data_wide, id.vars=c("time1"))
    names(city_data_long)<-c("time1","type","value")
  
    city_plot<-ggplot(city_data_long,aes(time1,value,color=type,size=type)) +
    geom_line()+
    scale_color_manual(values=c("#666666","#33CCCC","#CCCCCC")) + 
    scale_size_manual(values=c(0.8, 1.5, 1.2))+
    theme(legend.title=element_blank())+ 
    theme(legend.text = element_text(size = 12))+ 
    theme(legend.key.size=unit(0.75,"cm"))+
    geom_text(data = subset(city_data, mon%in%c(high_m)), aes(y = m1_fitted, label=mon), vjust = -2, size = 5, colour ="#FF6666" , fontface ="bold") +
    geom_text(data = subset(city_data, mon%in%c(low_m)), aes(y = m1_fitted, label=mon), vjust = 2, size = 5, colour ="#33CCFF" , fontface ="bold") +
    ylim(min(city_data$rent-100), max(city_data$rent+50)) +
    ylab(label="Rent") + 
    xlab(label = "Month") +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y-%m") +
    annotation_custom(ggplotGrob(season_plot),
                      xmin =  as.numeric(as.Date(plot_left_date)),
                      xmax =  as.numeric(as.Date(plot_right_date)), ymin = plot_low, ymax=plot_high)+
    labs( title=paste(city_data$RegionName),
          subtitle=paste0("Seasonal Gap is $",rent_gap))+
    theme(plot.title = element_text(size=30,hjust = 0.5))+
    theme(plot.subtitle=element_text(size=20, hjust=0.5, color="#666666"))
  
    return(city_plot)
}

all_rent_data <-get_region_rent(rent_1b_wide_top)

###  Boston  ###   
plot_nm <-"Boston"
plot_high<-1700
plot_low<-2000
season_y_low<-1580
season_y_high<-1880
plot_left_date<-'2014-07-01'
plot_right_date<-'2017-09-01'

bos_plot<-season_plot_publish(all_rent_data[RegionName==plot_nm],all_rent_data[RegionName==plot_nm][1]$id,1700,2000,'2014-07-01','2017-09-01',1550,1850)
bos_plot
png(
  paste0(dir1,plot_nm,".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(bos_plot)
dev.off()

###  Columbus  ### 
season_y_low<-400
season_y_high<-550
plot_left_date<-'2015-11-01'
plot_right_date<-'2017-08-01'
plot_high<-650
plot_low<-470
Columbus_plot<-season_plot_publish(all_rent_data[RegionName=="Columbus"],all_rent_data[RegionName=="Columbus"][1]$id,400,550,'2015-07-01','2017-09-01',425,640)
Columbus_plot
png(
  paste0(dir1,"Columbus",".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(Columbus_plot)
dev.off()

###  Woodbridge  ### 
wb_plot<-season_plot_publish(all_rent_data[RegionName=="Woodbridge"],all_rent_data[RegionName=="Woodbridge"][1]$id,1180,1285,'2015-07-01','2017-09-01',1035,1175)
wb_plot
png(
  paste0(dir1,"Woodbridge",".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(wb_plot)
dev.off()

###  Chicago  ### 
ch_plot<-season_plot_publish(all_rent_data[RegionName=="Chicago"],all_rent_data[RegionName=="Chicago"][1]$id,1000,1200,'2015-01-01','2017-09-01',1150,1425)
ch_plot
png(
  paste0(dir1,"Chicago",".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(ch_plot)
dev.off()

###  Minneapolis  ### 
mi_plot<-season_plot_pub2_publish(all_rent_data[RegionName=="Minneapolis"],all_rent_data[RegionName=="Minneapolis"][1]$id,1200,1450,'2016-04-01','2017-09-01',1060,1250)
mi_plot
png(
  paste0(dir1,"Minneapolis",".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(mi_plot)
dev.off()

###  Philadelphia  ### 
ph_plot<-season_plot_publish(all_rent_data[RegionName=="Philadelphia"],all_rent_data[RegionName=="Philadelphia"][1]$id,1365,1550,'2015-01-15','2017-12-01',870,1110)
ph_plot
png(
  paste0(dir1,"Philadelphia",".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(ph_plot)
dev.off()

###  Worcester  ### 
wo_plot<-season_plot_publish(all_rent_data[RegionName=="Worcester"],all_rent_data[RegionName=="Worcester"][1]$id,800,900,'2015-04-01','2017-09-01',720,890)
wo_plot
png(
  paste0(dir1,"Worcester",".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(wo_plot)
dev.off()

###  Cincinnati  ### 
plot_nm <-"Cincinnati"
ci_plot<-season_plot_publish(all_rent_data[RegionName==plot_nm],all_rent_data[RegionName==plot_nm][1]$id,500,580,'2016-05-01','2017-09-01',400,600)
ci_plot
png(
  paste0(dir1,plot_nm,".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(ci_plot)
dev.off()

###  Omaha  ###
plot_nm <-"Omaha"
om_plot<-season_plot_publish(all_rent_data[RegionName==plot_nm],all_rent_data[RegionName==plot_nm][1]$id,450,520,'2015-07-01','2017-09-01',420,650)
om_plot
png(
  paste0(dir1,plot_nm,".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(om_plot)
dev.off()

###  Seattle  ###
plot_nm <-"Seattle"
sea_plot<-season_plot_publish(all_rent_data[RegionName==plot_nm],all_rent_data[RegionName==plot_nm][1]$id,980,1080,'2015-01-01','2017-09-01',950,1430)
sea_plot
png(
  paste0(dir1,plot_nm,".png"),
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 1200,
  pointsize = 0.5
)
print(sea_plot)
dev.off()
