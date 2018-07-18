SELECT
   --a.CASE_ID
  --a.PRTNR_ID
  --,a.SEND_DT
  --,a.KILL_DT
  --,a.SURVEY_RESP_DT_ORIG
  a.QUESTION_ID
  ,COUNT(*) AS TOTAL_RESP
  ,ROUND(AVG(a.RESP_ID),2) AS AVG_RESP_ID
  ,ROUND(STDDEV(a.RESP_ID),2) AS SD_RESP_ID
  --,a.SEPARATED_IND
  --,a.ASSIGNED_ORG_ID
  --,a.ASSIGNED_ORG_TYPE
  --,a.ASSIGNED_ORG_ABBREV
  --,a.STORE_NUM_WORKED
  --,s.RGN_NUM AS RGN_NUM_WORKED
  --,s.RGN_DESCR AS RGN_DESC_WORKED
  ,a.JOB_ID
  --,a.SHIFT_ROLE_ID
  --,a.TZ_MISSING_IND
  --,a.SHIFT_START_DTM_LCL
  --,a.SHIFT_END_DTM_LCL
  --,a.SHIFT_START_DTM_PT
  --,a.SHIFT_END_DTM_PT
  ,a.DAY_PART
  ,c.FSCL_YR_NUM AS SHIFT_FSCL_YR_NUM
  --,c.FSCL_PER_IN_YR_NUM AS SHIFT_FSCL_PER_IN_YR_NUM
  ,c.FSCL_WK_IN_YR_NUM AS SHIFT_FSCL_WK_IN_YR_NUM
  --,c.FSCL_WK_BEG_DT AS SHIFT_FSCL_WK_BEG_DT
  ,c.DAY_ABBR_NM
FROM
(
SELECT
   sr.CASE_ID
  ,sr.PRTNR_ID
  ,sr.SEND_DT
  ,sr.KILL_DT
  ,sr.SURVEY_RESP_DT_ORIG
  ,sr.QUESTION_ID
  ,sr.RESP_ID
  ,prtnr.SAP_PRTNR_ID
  ,CASE WHEN h.ORG_ID IS NULL THEN 'Y' ELSE 'N' END AS SEPARATED_IND
  ,h.ORG_ID AS ASSIGNED_ORG_ID
  ,org.ORG_UNIT_TYPE_NM AS ASSIGNED_ORG_TYPE
  ,org.ORG_ABBREV_NM AS ASSIGNED_ORG_ABBREV
  ,tp.STORE_NUM AS STORE_NUM_WORKED
  ,tp.JOB_ID
  ,tp.SHIFT_ROLE_ID
  ,tp.TZ_MISSING_IND
  ,TO_CHAR(tp.SHIFT_START_DTM_LCL, 'HH24') AS HOUR
  ,(CASE WHEN TO_CHAR(tp.SHIFT_START_DTM_LCL, 'HH24') < 15 THEN 'am' 
    WHEN TO_CHAR(tp.SHIFT_START_DTM_LCL, 'HH24') >= 15 THEN 'pm' 
    ELSE 'NA' 
    END) AS DAY_PART
  ,tp.SHIFT_END_DTM_LCL
  ,tp.SHIFT_START_DTM_PT
  ,tp.SHIFT_END_DTM_PT
  ,ROW_NUMBER() OVER(PARTITION BY sr.CASE_ID, sr.QUESTION_ID ORDER BY tp.SHIFT_END_DTM_PT DESC) AS SHIFT_RANK
FROM SRV_DDV_RESPONSE sr

/* Get SAP Partner ID as that is what GLS uses. */
INNER JOIN D_PRTNR_VERS prtnr
  ON sr.PRTNR_ID = prtnr.PRTNR_ID
    AND prtnr.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    AND TRUNC(sr.SURVEY_RESP_DT_ORIG) BETWEEN prtnr.EFF_FROM_DT AND prtnr.EFF_TO_DT

/* Get assigned org info as of survey response date. If partner separated this will be null, so use left join so these responses aren't excluded. */
LEFT JOIN F_HEADCOUNT_VERS h
  ON sr.PRTNR_ID = h.PRTNR_ID
    AND h.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    AND TRUNC(sr.SURVEY_RESP_DT_ORIG) BETWEEN h.EFF_FROM_DT AND h.EFF_TO_DT
    
/* Get assigned org details. */
LEFT JOIN D_ORG_VERS org
  ON h.ORG_VERS_KEY = org.ORG_VERS_KEY

/* Timezone- and daylight savings-aware conversion of GLS timepunches from local time to Pacific Time
 * (Survey timestamps are in AMERICA/LOS_ANGELES timezone)
 * Adapted from code samples provided by Brad May and Tim Carter
 */
LEFT JOIN (  --begin tp
  SELECT
     gls.PRTNR_NUM
    ,s.STORE_NUM
    ,gls.JOB_ID
    ,gls.SHIFT_ROLE_ID
    ,gls.BUS_DT
    ,NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES') AS TZNAME
     /* If there isn't a valid time zone in ADT_STORE assume AMERICA/LOS_ANGELES but flag it here */
    ,CASE WHEN tzr.TZNAME IS NULL THEN 'Y' ELSE 'N' END AS TZ_MISSING_IND
    ,gls.SAFE_START_DTM AS SHIFT_START_DTM_LCL
    ,CAST((FROM_TZ(CAST(gls.SAFE_START_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('UTC'))AS DATE) AS SHIFT_START_DTM_UTC 
    ,CAST((FROM_TZ(CAST(gls.SAFE_START_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('AMERICA/LOS_ANGELES'))AS DATE) AS SHIFT_START_DTM_PT
    ,gls.SAFE_END_DTM AS SHIFT_END_DTM_LCL
    ,CAST((FROM_TZ(CAST(gls.SAFE_END_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('UTC'))AS DATE) AS SHIFT_END_DTM_UTC
    ,CAST((FROM_TZ(CAST(gls.SAFE_END_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('AMERICA/LOS_ANGELES'))AS DATE) AS SHIFT_END_DTM_PT
  FROM
    (SELECT
       PRTNR_NUM
      ,STORE_NUM
      ,JOB_ID
      ,SHIFT_ROLE_ID
      ,BUS_DT
      /* Check to make sure the start timestamp isn't during the leap forward at the start of DST, becasue it will crash the time zone conversion */
      ,TO_DATE(TO_CHAR(START_DTM,'YYYYMMDD') || 
        LPAD(
          CASE
            WHEN EXTRACT(MONTH FROM START_DTM) = 3
              AND TRUNC(START_DTM) = NEXT_DAY(NEXT_DAY(TRUNC(START_DTM, 'MM') - 1, 'Sunday'), 'Sunday')  --DST starts on 2nd Sunday
              AND TO_NUMBER(TO_CHAR(START_DTM, 'HH24MISS')) BETWEEN 20000 AND 25959
            THEN TO_NUMBER(TO_CHAR(START_DTM, 'HH24MISS')) - 10000
            ELSE TO_NUMBER(TO_CHAR(START_DTM, 'HH24MISS'))
          END
        ,6,0)
       ,'YYYYMMDDHH24MISS') AS SAFE_START_DTM
      /* Same for the end timestamp; Subtract from start timestamp and add to end timestamp so start time is never after end time */
      ,TO_DATE(TO_CHAR(END_DTM,'YYYYMMDD') || 
        LPAD(
          CASE
            WHEN EXTRACT(MONTH FROM END_DTM) = 3
              AND TRUNC(END_DTM) = NEXT_DAY(NEXT_DAY(TRUNC(END_DTM, 'MM') - 1, 'Sunday'), 'Sunday')  --DST starts on 2nd Sunday
              AND TO_NUMBER(TO_CHAR(END_DTM, 'HH24MISS')) BETWEEN 20000 AND 25959
            THEN TO_NUMBER(TO_CHAR(END_DTM, 'HH24MISS')) + 10000
            ELSE TO_NUMBER(TO_CHAR(END_DTM, 'HH24MISS'))
          END
        ,6,0)
       ,'YYYYMMDDHH24MISS') AS SAFE_END_DTM
      ,START_RSN_NM
      ,END_RSN_NM
    FROM APPDWH.AFT_GLS_PRTNR_TMCARD@SBP411

    WHERE CNTRY_CD = 'US'
      AND BUS_DT >= '10-APR-17'  --one week prior to launch of Pulse
      AND (BUS_DT BETWEEN '21-MAY-18' AND '20-JUL-18') or (BUS_DT BETWEEN '22-MAY-17' AND '20-JUL-17')
    ) gls
    
  INNER JOIN APPDWH.ADT_STORE@SBP411 s
    ON gls.STORE_NUM = s.STORE_NUM

  /* Get official time zone descriptions to make sure everything from ADT_STORE is valid */
  LEFT JOIN (SELECT DISTINCT TZNAME FROM V$TIMEZONE_NAMES) tzr
    ON REGEXP_SUBSTR(s.TIME_ZONE_DESCR,'[^ ]+', 1, 2) = tzr.TZNAME
) tp
  ON prtnr.SAP_PRTNR_ID = tp.PRTNR_NUM
    AND sr.SURVEY_RESP_DT_ORIG > tp.SHIFT_START_DTM_PT
    AND sr.SURVEY_RESP_DT_ORIG <= tp.SHIFT_END_DTM_PT + 7  --shift has to have been in the last 7 days (tried using BUS_DT but doens't seem to be any faster)

--WHERE TRUNC(sr.SURVEY_RESP_DT_ORIG) = '09-JUN-17'  --for testing
WHERE sr.QUESTION_ID IN ('Q1')
) a

INNER JOIN APPPDW.ADV_CAL c
  ON TRUNC(a.SHIFT_END_DTM_LCL) = c.CAL_DT
  
LEFT JOIN APPPDW.ADV_STORE s
  ON a.STORE_NUM_WORKED = s.STORE_NUM
    AND s.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)

WHERE a.SHIFT_RANK = 1  --most recent shift prior to survey response date/time
  AND c.DAY_ABBR_NM NOT IN ('SA','SU')
  
GROUP BY 
  a.QUESTION_ID
  ,a.JOB_ID
  ,a.DAY_PART
  ,c.FSCL_YR_NUM
  ,c.FSCL_WK_IN_YR_NUM
  ,c.DAY_ABBR_NM