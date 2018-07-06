
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
