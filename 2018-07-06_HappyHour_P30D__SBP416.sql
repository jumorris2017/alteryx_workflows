
/*query for CC score and survey counts by daypart by store */
/*CAW*/
WITH sq AS
(SELECT DISTINCT
  sr.GUID_USER_ID
  ,TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 15 THEN 'am' 
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >= 15 THEN 'pm' 
        ELSE 'NA' 
        END) AS DAY_PART
  ,sr.QSTN_ID
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ROUND(SUM(CASE WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) / SUM(COALESCE(w.WEIGHT_RT,1)),3)*100 AS TB_SCORE
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  ,ca.DAY_ABBR_NM

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'
    
LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

  WHERE sr.QSTN_ID IN ('Q2_2','Q2_1','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM IN (2017,2018) AND ca.FSCL_WK_IN_YR_NUM IN (34,36,37,38,39) --UPDATE HERE

GROUP BY
  sr.GUID_USER_ID
  ,TO_CHAR(sr.TRANS_DTM, 'HH24') 
  --,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  ,ca.DAY_ABBR_NM
)
SELECT 
sq.QSTN_ID
,sq.DAY_PART
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS TB_SCORE
,ROUND(STDDEV(sq.TB_SCORE),2) AS SD_TB_SCORE
,sq.FSCL_YR_NUM
,sq.FSCL_WK_IN_YR_NUM
,sq.DAY_ABBR_NM
  
FROM sq

WHERE sq.DAY_PART IN ('am','pm') AND sq.DAY_ABBR_NM IN ('MO','TU','WE','TH','FR')

GROUP BY 
sq.QSTN_ID
,sq.DAY_PART
,sq.FSCL_YR_NUM
,sq.FSCL_WK_IN_YR_NUM
,sq.DAY_ABBR_NM

ORDER BY sq.FSCL_YR_NUM
,sq.FSCL_WK_IN_YR_NUM
,sq.DAY_PART
,sq.DAY_ABBR_NM



























SELECT DISTINCT
  sr.GUID_USER_ID
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  ,(CASE WHEN item.IS_GVE = 1 THEN 'HHITEM' ELSE 'NON-HHITEM' END) AS HH_ITEM
  ,TO_CHAR(sr.TRANS_DTM, 'HH24') AS DAY_HOUR
  ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 15 THEN 'AM'
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >= 15 THEN 'PM'
        ELSE NULL END) AS DAY_PART

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)

JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'

JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    AND pi.BUS_DT = '24-MAY-18' 

LEFT JOIN (SELECT ITEM_ID
  ,SUM(CASE WHEN dept_descr='ESPRESSO BEVERAGES' and pkg_sz_qty in ('VENTI',' GRANDE') THEN 1 ELSE 0 END) AS IS_GVE
  FROM appca.d_item
  GROUP BY ITEM_ID) item
ON pi.ITEM_ID = item.ITEM_ID

WHERE sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  --AND sr.STORE_NUM = 5798
  AND ca.FSCL_YR_NUM = 2018 
  AND ca.FSCL_WK_IN_YR_NUM = 34 -- MAY 24 -- CASE WHEN dept_descr='ESPRESSO BEVERAGES' and pkg_sz_qty in ('VENTI',' GRANDE') THEN 1 ELSE 0 END

GROUP BY
  sr.GUID_USER_ID
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  ,item.IS_GVE
  ,TO_CHAR(sr.TRANS_DTM, 'HH24') 