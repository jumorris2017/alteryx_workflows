
#load libraries
library(data.table)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
hh <- fread(paste0(data_dir,"/happyhour_ce.csv"))


# #remove extra dates
# hh <- hh[FSCL_WK_IN_YR_NUM!=35]
# hh <- hh[!DAY_ABBR_NM %in% c("SA","SU")]

#add flag for hh days
hh[, hhflag := 0]
hh[FSCL_WK_IN_YR_NUM %in% c(34,36,38,39)&DAY_ABBR_NM=='TH', hhflag := 1]
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
pv[SHIFT_FSCL_WK_IN_YR_NUM %in% c(34,36,38,39,40,41,42)&DAY_ABBR_NM=='TH', hhflag := 1]
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
hh34 <- fread(paste0(data_dir,"/hh_fw_34.csv"))
hh36 <- fread(paste0(data_dir,"/hh_fw_36.csv"))
hh37 <- fread(paste0(data_dir,"/hh_fw_37.csv"))
hh38 <- fread(paste0(data_dir,"/hh_fw_38.csv"))
hh39 <- fread(paste0(data_dir,"/hh_fw_39.csv"))
#rbindlist
l = list(hh34,hh36,hh37,hh38,hh39)
hhdt <- rbindlist(l, use.names=TRUE, fill=TRUE)


#create SO agg
hhdt[QSTN_ID %in% c("Q2_1","Q2_3","Q2_4","Q2_5","Q2_6","Q2_7"), so_flag := 1]

#aggregate
hhdtcc <- hhdt[QSTN_ID=="Q2_2", list(TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
           cc_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
           by=c("DAY_PART","FSCL_YR_NUM","HH_ITEM")]
hhdtsp <- hhdt[QSTN_ID=="Q2_1", list(
  sp_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
  by=c("DAY_PART","FSCL_YR_NUM","HH_ITEM")]
hhdtso <- hhdt[so_flag==1, list(
  so_score = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)),
  by=c("DAY_PART","FSCL_YR_NUM","HH_ITEM")]

#happy hour part 3
hhd <- fread(paste0(data_dir,"/hh_results_byday.csv"))



#set up line chart
pdata <- hhd
px <- hhd[, DAY_IN_CAL_WK_NUM]
py <- hhd[, CC_SCORE]
groupvar <- hhd[, FSCL_WK_IN_YR_NUM]
#labels
xlabels <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
#set labels
xlabel <- ""
ylabel <- "CC Score"
tlabel <- "Customer Connection Score by Week and Day"
sublabel <- "Lasting Impact of Happy Hour"
caption <- "U.S. Company-Operated Stores"
#manual legend labels
lname <- "Fiscal Week"
llabels <- c("34","36","37","38","39") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=32, hjust=-0.5, vjust=0, label = "HH", angle=90, size=4) +
  geom_vline(aes(xintercept = 6)) + annotate(geom = "text", x=6, y=32, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #theme(axis.text.x = element_text(size=9, angle=90, hjust=1, vjust=.75)) +
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)





#set up line chart
pdata <- hhd
px <- hhd[, DAY_IN_CAL_WK_NUM]
py <- hhd[, SO_SCORE]
groupvar <- hhd[, FSCL_WK_IN_YR_NUM]
#labels
xlabels <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
#set labels
xlabel <- ""
ylabel <- "SO Score"
tlabel <- "Store Operations Score by Week and Day"
sublabel <- "Lasting Impact of Happy Hour"
caption <- "U.S. Company-Operated Stores"
#manual legend labels
lname <- "Fiscal Week"
llabels <- c("34","36","37","38","39") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels) +
  geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=62, hjust=-0.5, vjust=0, label = "HH", angle=90, size=4) +
  geom_vline(aes(xintercept = 6)) + annotate(geom = "text", x=6, y=62, hjust=0, vjust=0, label = "HH:37", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #theme(axis.text.x = element_text(size=9, angle=90, hjust=1, vjust=.75)) +
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)


