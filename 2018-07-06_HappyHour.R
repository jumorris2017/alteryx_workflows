
#load libraries
library(data.table)
library(PerformanceAnalytics)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
hh <- fread(paste0(data_dir,"/happyhour_ce.csv"))


# #remove extra dates
# hh <- hh[FSCL_WK_IN_YR_NUM!=35]
# hh <- hh[!DAY_ABBR_NM %in% c("SA","SU")]

#add flag for hh days
hh[, hhflag := 0]
hh[FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44)&DAY_ABBR_NM=='TH', hhflag := 1]
hh[FSCL_WK_IN_YR_NUM==37&DAY_ABBR_NM=='FR', hhflag := 1]

#create SO agg
hh[QSTN_ID %in% c("Q2_1","Q2_3","Q2_4","Q2_5","Q2_6","Q2_7"), so_flag := 1]

#aggregate
hhcc <- hh[QSTN_ID=="Q2_2", list(TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
          cc_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
   by=c("DAY_PART","FSCL_YR_NUM","hhflag")]
hhsp <- hh[QSTN_ID=="Q2_1", list(
            sp_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
           by=c("DAY_PART","FSCL_YR_NUM","hhflag")]
hhso <- hh[so_flag==1, list(
            so_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
           by=c("DAY_PART","FSCL_YR_NUM","hhflag")]

#merge
hh <- Reduce(function(x,y) {merge(x, y, by=c("DAY_PART","FSCL_YR_NUM","hhflag"), all=T)}, list(hhcc,hhso,hhsp)) 

#swing wide by day part and year
hh <- dcast.data.table(hh, hhflag ~ DAY_PART + FSCL_YR_NUM, value.var=c("TOTAL_RSPNS","cc_score","so_score","sp_score"))
hh <- setorder(hh,-hhflag)

#calculate deltas
hh[, cc_score_am_delta := cc_score_am_2018-cc_score_am_2017]
hh[, cc_score_pm_delta := cc_score_pm_2018-cc_score_pm_2017]

hh[, so_score_am_delta := so_score_am_2018-so_score_am_2017]
hh[, so_score_pm_delta := so_score_pm_2018-so_score_pm_2017]

hh[, sp_score_am_delta := sp_score_am_2018-sp_score_am_2017]
hh[, sp_score_pm_delta := sp_score_pm_2018-sp_score_pm_2017]

#calculate happy hour impact
# hh[hhflag==1,cc_score_pm_delta]-hh[hhflag==0,cc_score_pm_delta]
# hh[hhflag==1,so_score_pm_delta]-hh[hhflag==0,so_score_pm_delta]
# hh[hhflag==1,sp_score_pm_delta]-hh[hhflag==0,sp_score_pm_delta]
# 
# hh[hhflag==1,cc_score_am_delta]-hh[hhflag==0,cc_score_am_delta]
# hh[hhflag==1,so_score_am_delta]-hh[hhflag==0,so_score_am_delta]
# hh[hhflag==1,sp_score_am_delta]-hh[hhflag==0,sp_score_am_delta]

hhfunc <- function(a,b,c,d) {
  (a-b) - (c-d)
}

hhfunc(hh[hhflag==1,cc_score_pm_delta],hh[hhflag==1,cc_score_am_delta],
       hh[hhflag==0,cc_score_pm_delta],hh[hhflag==0,cc_score_am_delta])

hhfunc(hh[hhflag==1,so_score_pm_delta],hh[hhflag==1,so_score_am_delta],
       hh[hhflag==0,so_score_pm_delta],hh[hhflag==0,so_score_am_delta])

hhfunc(hh[hhflag==1,sp_score_pm_delta],hh[hhflag==1,sp_score_am_delta],
       hh[hhflag==0,sp_score_pm_delta],hh[hhflag==0,sp_score_am_delta])


#write.csv
write.csv(hh,file=paste0(data_dir,"/happyhour_results.csv"))


#pulse
pv <- fread(paste0(data_dir,"/happyhour_pv.csv"))

#add flag for hh days
pv[, hhflag := 0]
pv[SHIFT_FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44)&DAY_ABBR_NM=='TH', hhflag := 1]
pv[SHIFT_FSCL_WK_IN_YR_NUM==37&DAY_ABBR_NM=='FR', hhflag := 1]

#code into hourly vs sm job roles
pv[JOB_ID==50000117, job_role := "sm"]
pv[JOB_ID %in% c(50000362,50000358,50018175), job_role := "hourly"]
pv <- na.omit(pv,cols="job_role")
pv[, weighted_avg := TOTAL_RESP*AVG_RESP_ID]

#aggregate / calculated weighted average
pv <- pv[, list(TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
                AVG_RESP = 
                round(sum(weighted_avg,na.rm=T)/sum(TOTAL_RESP,na.rm=T),2)),
           by=c("job_role","DAY_PART","SHIFT_FSCL_YR_NUM","hhflag")]

#set order
pv <- setorder(pv,job_role,DAY_PART,-hhflag)

#swing wide by day part and year
pv <- dcast.data.table(pv, hhflag ~ job_role + DAY_PART + SHIFT_FSCL_YR_NUM, value.var=c("AVG_RESP","TOTAL_RESP"))
pv <- setorder(pv,-hhflag)

#calculate deltas
pv[, hourly_am_delta := AVG_RESP_hourly_am_2018-AVG_RESP_hourly_am_2017]
pv[, hourly_pm_delta := AVG_RESP_hourly_pm_2018-AVG_RESP_hourly_pm_2017]

pv[, sm_am_delta := AVG_RESP_sm_am_2018-AVG_RESP_sm_am_2017]
pv[, sm_pm_delta := AVG_RESP_sm_pm_2018-AVG_RESP_sm_pm_2017]

hhfunc <- function(a,b,c,d) {
  (a-b) - (c-d)
}

hhfunc(pv[hhflag==1,hourly_pm_delta],pv[hhflag==1,hourly_am_delta],
       pv[hhflag==0,hourly_pm_delta],pv[hhflag==0,hourly_am_delta])

hhfunc(pv[hhflag==1,sm_pm_delta],pv[hhflag==1,sm_am_delta],
       pv[hhflag==0,sm_pm_delta],pv[hhflag==0,sm_am_delta])

#write.csv
write.csv(pv,file=paste0(data_dir,"/happyhour_pv_results.csv"))


#happy hour part 2
# hh34 <- fread(paste0(data_dir,"/hh_fw_34.csv"))
# hh36 <- fread(paste0(data_dir,"/hh_fw_36.csv"))
# hh37 <- fread(paste0(data_dir,"/hh_fw_37.csv"))
# hh38 <- fread(paste0(data_dir,"/hh_fw_38.csv"))
# hh39 <- fread(paste0(data_dir,"/hh_fw_39.csv"))
# #rbindlist
# l = list(hh34,hh36,hh37,hh38,hh39)
# hhdt <- rbindlist(l, use.names=TRUE, fill=TRUE)


#happy hour part 3
# hhd <- fread(paste0(data_dir,"/hh_results_byday.csv"))
hhd <- fread(paste0(data_dir,"/happyhour_ce.csv"))

#create SO agg
hhd[QSTN_ID %in% c("Q2_2"), so_flag := 0]
hhd[QSTN_ID %in% c("Q2_1","Q2_3","Q2_4","Q2_5","Q2_6","Q2_7"), so_flag := 1]

#add flag for hh days 
hhd[FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44), hh_week_flag := 1] #thurs hh
hhd[FSCL_WK_IN_YR_NUM==37, hh_week_flag := 2] # fri hh
hhd[FSCL_WK_IN_YR_NUM==35, hh_week_flag := 3] #no hh

#aggregate
hhd <- hhd[, list(
  tb_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
  by=c("DAY_IN_CAL_WK_NUM","FSCL_YR_NUM","hh_week_flag","so_flag")]

#swing wide by so flag
hhd <- dcast.data.table(hhd, DAY_IN_CAL_WK_NUM + FSCL_YR_NUM + hh_week_flag ~ so_flag,
                        value.var="tb_score")
setnames(hhd,c("0","1"),c("cc","so"))

#reverse order of year variable for plot
hhd[FSCL_YR_NUM==2018, prior_year := 0]
hhd[FSCL_YR_NUM==2017, prior_year := 1]

#set up line chart
pdata <- hhd
px <- hhd[, DAY_IN_CAL_WK_NUM]
py <- hhd[, cc*100]
groupvar <- hhd[, prior_year]
colourvar <- hhd[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "CC Score"
tlabel <- "Customer Connection Score by Day of Week "
sublabel <- "No apparent lasting impact of happy hour"
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37; No happy hour: FW 35"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH","No HH") 
lname2 <- "Year"
llabels2 <- c("2018","2017")
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), linetype=factor(groupvar), group=interaction(groupvar, colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  scale_y_continuous(limits=c(29,35),breaks=c(29:35)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)


#set up line chart
pdata <- hhd
px <- hhd[, DAY_IN_CAL_WK_NUM]
py <- hhd[, so*100]
groupvar <- hhd[, prior_year]
colourvar <- hhd[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "SO Score"
tlabel <- "Store Operations Score by Day of Week"
sublabel <- "No apparent lasting impact of happy hour"
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37; No happy hour: FW 35"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH","No HH") 
lname2 <- "Year"
llabels2 <- c("2018","2017")
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), linetype=factor(groupvar), group=interaction(groupvar, colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  scale_y_continuous(limits=c(59,65)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)


#happy hour part 4 - PM only
hhd <- fread(paste0(data_dir,"/happyhour_ce.csv"))

#create SO agg
hhd[QSTN_ID %in% c("Q2_2"), so_flag := 0]
hhd[QSTN_ID %in% c("Q2_1","Q2_3","Q2_4","Q2_5","Q2_6","Q2_7"), so_flag := 1]

#add flag for hh days 
hhd[FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44), hh_week_flag := 1] #thurs hh
hhd[FSCL_WK_IN_YR_NUM==37, hh_week_flag := 2] # fri hh
hhd[FSCL_WK_IN_YR_NUM==35, hh_week_flag := 3] #no hh

#aggregate
hhdpm <- hhd[DAY_PART=='pm'&FSCL_WK_IN_YR_NUM!=35, list(
  tb_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
  by=c("DAY_IN_CAL_WK_NUM","FSCL_YR_NUM","hh_week_flag","so_flag")]

#swing wide by so flag
hhdpm <- dcast.data.table(hhdpm, DAY_IN_CAL_WK_NUM + FSCL_YR_NUM + hh_week_flag ~ so_flag,
                        value.var="tb_score")
setnames(hhdpm,c("0","1"),c("cc","so"))

#reverse order of year variable for plot
hhdpm[FSCL_YR_NUM==2018, prior_year := 0]
hhdpm[FSCL_YR_NUM==2017, prior_year := 1]

#set up line chart
pdata <- hhdpm
px <- hhdpm[, DAY_IN_CAL_WK_NUM]
py <- hhdpm[, cc*100]
groupvar <- hhdpm[, prior_year]
colourvar <- hhdpm[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "CC Score"
tlabel <- "PM (3pm-close) Customer Connection Score by Day of Week "
sublabel <- "No apparent lasting impact of happy hour"
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH") 
lname2 <- "Year"
llabels2 <- c("2018","2017")
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), linetype=factor(groupvar), group=interaction(groupvar, colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  scale_y_continuous(limits=c(26,31),breaks=c(26:31)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)


#set up line chart
pdata <- hhdpm
px <- hhdpm[, DAY_IN_CAL_WK_NUM]
py <- hhdpm[, so*100]
groupvar <- hhdpm[, prior_year]
colourvar <- hhdpm[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "SO Score"
tlabel <- "PM (3pm-close) Store Operations Score by Day of Week"
sublabel <- "No apparent lasting impact of happy hour"
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH") 
lname2 <- "Year"
llabels2 <- c("2018","2017")
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), linetype=factor(groupvar), group=interaction(groupvar, colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  #scale_y_continuous(limits=c(59,65)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)



#happy hour part 5 - UPLH
hhu <- fread(paste0(data_dir,"/happyhour_uplh.csv"))

#add flag for hh days 
hhu[FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44), hh_week_flag := 1] #thurs hh
hhu[FSCL_WK_IN_YR_NUM==37, hh_week_flag := 2] # fri hh
hhu[FSCL_WK_IN_YR_NUM==35, hh_week_flag := 3] #no hh

#aggregate
hhu <- hhu[DAY_IN_CAL_WK_NUM %in% c(2:6), list(
  UPLH = round(sum(TOT_UNITS,na.rm=T)/sum(TOT_HOURS,na.rm=T),1)),
  by=c("DAY_IN_CAL_WK_NUM","FSCL_YR_NUM","hh_week_flag")]

#reverse order of year variable for plot
hhu[FSCL_YR_NUM==2018, prior_year := 0]
hhu[FSCL_YR_NUM==2017, prior_year := 1]

#set up line chart
pdata <- hhu
px <- hhu[, DAY_IN_CAL_WK_NUM]
py <- hhu[, UPLH]
groupvar <- hhu[, prior_year]
colourvar <- hhu[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "UPLH"
tlabel <- "UPLH by Day of Week"
sublabel <- ""
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37; No happy hour: FW 35"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH","No HH") 
lname2 <- "Year"
llabels2 <- c("2018","2017")
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), linetype=factor(groupvar), group=interaction(groupvar, colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  #scale_y_continuous(limits=c(59,65)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)

#happy hour part 5 - UPLH
hhu <- fread(paste0(data_dir,"/happyhour_uplh.csv"))

#add flag for hh days 
hhu[FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44), hh_week_flag := 1] #thurs hh
hhu[FSCL_WK_IN_YR_NUM==37, hh_week_flag := 2] # fri hh
hhu[FSCL_WK_IN_YR_NUM==35, hh_week_flag := 3] #no hh


#aggregate
hhupm <- hhu[DAY_PART=='PM'&DAY_IN_CAL_WK_NUM %in% c(2:6)&hh_week_flag<=2, list(
  UPLH = round(sum(TOT_UNITS,na.rm=T)/sum(TOT_HOURS,na.rm=T),1)),
  by=c("DAY_IN_CAL_WK_NUM","FSCL_YR_NUM","hh_week_flag")]

#set up line chart
pdata <- hhupm
px <- hhupm[, DAY_IN_CAL_WK_NUM]
py <- hhupm[, UPLH]
colourvar <- hhupm[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "UPLH"
tlabel <- "PM (3pm-close) UPLH by Day of Week"
sublabel <- ""
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH") 
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), group=factor(colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  scale_y_continuous(limits=c(19,24)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)

#aggregate
hhuam <- hhu[DAY_PART=='AM'&DAY_IN_CAL_WK_NUM %in% c(2:6)&hh_week_flag<=2, list(
  UPLH = round(sum(TOT_UNITS,na.rm=T)/sum(TOT_HOURS,na.rm=T),1)),
  by=c("DAY_IN_CAL_WK_NUM","FSCL_YR_NUM","hh_week_flag")]

#set up line chart
pdata <- hhuam
px <- hhuam[, DAY_IN_CAL_WK_NUM]
py <- hhuam[, UPLH]
colourvar <- hhuam[, hh_week_flag]
#labels
xlabels <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
#set labels
xlabel <- ""
ylabel <- "UPLH"
tlabel <- "AM (Open-3pm) UPLH by Day of Week"
sublabel <- ""
caption <- "U.S. Company-Operated Stores\nThursday happy hours: FW 34, 36, 38, 39, 40, 41, 42, 43, 44; Friday happy hour: FW 37"
#manual legend labels
lname1 <- "Happy Hour Day"
llabels1 <- c("Thursday HH","Friday HH") 
#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1.25, aes(x=factor(px), y=py, colour=factor(colourvar), group=factor(colourvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  scale_y_continuous(limits=c(34,41),breaks=c(34:41)) +
  geom_vline(aes(xintercept = 4)) + geom_vline(aes(xintercept = 5)) +
  #geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=.32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)



#model it by day
hhd <- fread(paste0(data_dir,"/hh_results_byday.csv"))
hhd <- hhd[QSTN_ID=="Q2_2"]
pv <- fread(paste0(data_dir,"/happyhour_pv.csv"))
hhu <- fread(paste0(data_dir,"/happyhour_uplh.csv"))
hhu <- hhu[DAY_IN_CAL_WK_NUM %in% c(2:6)]

#agg by hourly and sm, then swing wide

#add flag for hh days
pv[, hhflag := 0]
pv[SHIFT_FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42,43,44)&DAY_ABBR_NM=='TH', hhflag := 1]
pv[SHIFT_FSCL_WK_IN_YR_NUM==37&DAY_ABBR_NM=='FR', hhflag := 1]

#code into hourly vs sm job roles
pv[JOB_ID==50000117, job_role := "sm"]
pv[JOB_ID %in% c(50000362,50000358,50018175), job_role := "hourly"]
pv <- na.omit(pv,cols="job_role")
pv[, weighted_avg := TOTAL_RESP*AVG_RESP_ID]

#aggregate / calculated weighted average
pv <- pv[, list(TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
                AVG_RESP = 
                  round(sum(weighted_avg,na.rm=T)/sum(TOTAL_RESP,na.rm=T),2)),
         by=c("job_role","DAY_PART","SHIFT_FSCL_YR_NUM","SHIFT_FSCL_WK_IN_YR_NUM","DAY_ABBR_NM","hhflag")]

#set order
pv <- setorder(pv,job_role,DAY_PART,-hhflag)

#swing wide by day part and year
pv <- dcast.data.table(pv, hhflag + DAY_PART + SHIFT_FSCL_YR_NUM + SHIFT_FSCL_WK_IN_YR_NUM + 
                         DAY_ABBR_NM ~ job_role, 
                       value.var=c("AVG_RESP"))
setnames(pv,c("hourly","sm","SHIFT_FSCL_YR_NUM","SHIFT_FSCL_WK_IN_YR_NUM"),
         c("pv_hourly","pv_sm","FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"))
pv[, DAY_PART := toupper(DAY_PART)]

#reduce variables
hhd[, DAY_PART := toupper(DAY_PART)]
hhd <- hhd[, .(DAY_PART,TB_SCORE,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,DAY_ABBR_NM)]
hhu[, UPLH := round(TOT_UNITS/TOT_HOURS,2)]
hhu <- hhu[, .(DAY_PART,UPLH,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,DAY_ABBR_NM)]

#merge
newDT <- Reduce(function(x,y) {merge(x,y,by=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM","DAY_ABBR_NM","DAY_PART"),
                                     all=T)}, list(pv,hhu,hhd))
newDT[DAY_PART=="AM", dp_pm := 0];newDT[DAY_PART=="PM", dp_pm := 1]

#regression models
summary(lm(data=newDT, TB_SCORE ~ hhflag))
summary(lm(data=newDT, pv_hourly ~ hhflag))
summary(lm(data=newDT, pv_sm ~ hhflag))

summary(lm(data=newDT, TB_SCORE ~ hhflag + dp_pm + UPLH))
summary(lm(data=newDT, pv_hourly ~ hhflag + UPLH))
summary(lm(data=newDT, pv_sm ~ hhflag + UPLH))

summary(lm(data=newDT, TB_SCORE ~ UPLH))
summary(lm(data=newDT, pv_hourly ~ UPLH))
summary(lm(data=newDT, pv_sm ~ UPLH))

#correlation matrix
corvars <- newDT[, .(FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,hhflag,dp_pm,pv_hourly,pv_sm,UPLH,TB_SCORE)]

## correlation matrix with p-values
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
## create function to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
#flatten the table
flattenSquareMatrix(cor.prob(corvars))
#plot the data
chart.Correlation(corvars)
