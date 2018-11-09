

--------------------------------------------------
-- ED DATA FOR R WORKSHOP DAY 2 
--29881 rows 
--------------------------------------------------

select FacilityShortName
	, ed.PatientID
	, VisitID
	--, ad.PatientId as AD_PatientID
	--, ad.AccountNumber
	, StartDate
	, StartDateFiscalYear
	, Age
	, TriageAcuityCode
	, CharlsonIndexTotalWeight
	, ad.AdmissionNursingUnitCode
	, StarttoDispositionInclCDU as ED_LOS_minutes 
	, ad.LOSDays as AD_LOS_days 

-- select distinct FacilityShortName
from [EDMart].[dbo].[vwEDVisitIdentifiedRegional]			ed 
	left join [ADTCMart].[ADTC].[AdmissionDischargeView]	ad 
	on ed.PatientID = ad.PatientId
	and ed.VisitID = ad.AccountNumber

where 1=1 
	and FacilityShortName = 'lgh'   
	and StartDate >= '2015-01-01' 
	and AdmittedFlag = 1 
	and ad.PatientId is not null 


order by FacilityShortName
	, StartDate
	, Age
	, TriageAcuityCode