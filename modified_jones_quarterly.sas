/*
	Earnings management models - using quarterly data
	Author: Joost Impink, September 2017
	Models estimated:
	- Modified Jones model, as Jones model, but using chSales - chREC to compute fitted values.
		- variable names DA_mJones ABSDA_mJones 

	tac:		Total accruals, computed as net profit after tax before extraordinary items 
				less cash flows from operations	
	1/TAt-1:	Inverse of beginning of year total assets
	chSales:	Change in net sales revenue
	chREC: 		Change in net receivables
	PPE:		Gross property, plant, and equipment
	ROA:		Return on assets. 
	Variables used Compustat Funda
	AT:		Total assets
	NI:		Net income
	IB: 	Income Before Extraordinary Items
	IBC: 	Income Before Extraordinary Items (Cash Flow) (used if IB is missing)
	OANCF: 	Operating Activities - Net Cash Flow
	PPEGT:	Property, Plant and Equipment - Total (Gross)
	RECT: 	Receivables - Total
	SALE:	Sales

	IBY: 	

*/


/* Include %array and %do_over */
filename m1 url 'https://gist.githubusercontent.com/JoostImpink/c22197c93ecd27bbf7ef/raw/2e2a54825c9dbfdfd66cfc94b9abe05e9d1f1a8e/array.sas';
%include m1;
filename m1b url 'https://gist.githubusercontent.com/JoostImpink/c22197c93ecd27bbf7ef/raw/2e2a54825c9dbfdfd66cfc94b9abe05e9d1f1a8e/do_over.sas';
%include m1b;

/* Winsorize macro */
filename m2 url 'https://gist.githubusercontent.com/JoostImpink/497d4852c49d26f164f5/raw/11efba42a13f24f67b5f037e884f4960560a2166/winsorize.sas';
%include m2;

/* Get Funda variables: note ibcy and oancfy are cumulative */
%let fundqVars = atq niq ibq ibcy oancfy ppegtq rectq saleq ;

/* Funda data */
data getf_1 (keep = firmYear gvkey fyear datadate sich);
set comp.funda;
/* Period */
if 2005 <= fyear <= 2016;
/* Generic filter */
if indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C' ;
/* Firm-year identifier */
firmYear = gvkey || fyear ;
run;

/* 	Keep first record in case of multiple records; */
proc sort data =getf_1 nodupkey; by gvkey descending fyear;run;

/* if sich is missing, use the one of last year (note sorted descending by fyear) */
data getf_1 (drop = sich_prev);
set getf_1;
retain sich_prev;
by gvkey;
if first.gvkey then sich_prev = .;
if missing(sich) then sich = sich_prev;
sich_prev = sich;
run;

/* fundQ */
data getf_2 (keep = firmYear firmQuarter fqtr &fundqVars);
set comp.fundq;
if 2005 <= fyearq <= 2016;
if indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C' ;
firmYear = gvkey || fyearq;
firmQuarter = gvkey || fyearq || fqtr;
/* Keep if sale > 0, at > 0 */
if saleq > 0 and atq > 0;
run;

/* join */
proc sql;
  create table getf_3 as select a.*, b.* from getf_1 a, getf_2 b where a.firmYear = b.firmYear;
quit;

/* force unique records */
proc sort data=getf_3 nodupkey; by firmQuarter;run;

/* for iby, oancfy: create ibq and oancfq by subtracting previous quarter */
proc sql;
  create table b_fundq as 
  select a.*, a.ibcy - b.ibcy as ibcq, a.oancfy - b.oancfy as oancfq
  from getf_3 a left join getf_3 b
  /* make sure b is the previous quarter of the same firm */
  on a.firmYear = b.firmYear and a.fqtr - 1 = b.fqtr;
quit;

/* set ibcq and oancfq to iby and oancfy if fqtr is 1 */
data b_fundq;
set b_fundq;
if fqtr eq 1 then do;
  ibcq = ibcy;
  oancfq = oancfy;
end;
/* Use Income Before Extraordinary Items (Cash Flow) if ibq is missing */
if ibq =. then ibq=ibcq;
run;

/* Lagged values for: at sale rect ib */
%let lagVars = atq saleq rectq ibq;

/* Self join to get lagged values at_l, sale_l, rect_l, same quarter previous year */
proc sql;
 create table c_lagged as select a.*, %do_over(values=&lagVars, between=comma, phrase=b.? as ?_l)
 from b_fundq a, b_fundq b
 where a.gvkey = b.gvkey and a.fyear-1 = b.fyear and a.fqtr = b.fqtr;
quit;

/* Construct additional variables */
data c_lagged;
set c_lagged;
/* 2-digit SIC  */
SIC2 = int(sich/100);
/* variables */
tac       = (ibq - oancfq)/atq_l;  
inv_at_l  = 1 / atq_l;
rev       = saleq / atq_l;
drev      = (saleq - saleq_l) / atq_l;
drevadj   = (saleq - saleq_l)/atq_l - (rectq - rectq_l)/atq_l;
ppe       = ppegtq / atq_l;
netinc    = niq / atq;
roa 	  = ibq / atq;
roa_l 	  = ibq_l/ atq_l; /* net income before extraordinary items */
/* these variables may not be missing (cmiss counts missing variables)*/
if cmiss  (of sich tac inv_at_l drevadj ppe roa_l) eq 0;
run;

/* Winsorize  */
%let winsVars = tac inv_at_l drev drevadj ppe roa_l netinc ; 
%winsor(dsetin=c_lagged, dsetout=c_lagged_wins, /*byvar=, */ vars=&winsVars, type=winsor, pctl=1 99);

/* Regression by industry-year 
 edf + #params (4) will equal the number of obs (no need for proc univariate to count) */
proc sort data=c_lagged_wins; by fyear fqtr sic2;run;
proc reg data=c_lagged_wins noprint edf outest=d_parms;
model tac = inv_at_l drev ppe; /* Jones Model */
model tac = inv_at_l drevadj ppe roa_l; /* Kothari with ROA in model */     
by fyear fqtr sic2;
run;

/* Append discretionary accrual measures */
/* Jones model */
proc sql;
 create table e_model1 as select a.*, b.DA_Jones, abs (b.DA_Jones) as ABSDA_Jones	
	from 
		c_lagged_wins a left join 
 			( select a.firmQuarter, a.tac - ( b.intercept + %do_over(values=inv_at_l drev ppe, between=%str(+), phrase=a.? * b.?) ) as DA_Jones
			 from c_lagged_wins a left join d_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 1 */
			 and b._MODEL_ eq "MODEL1"
			 /* at a minimum 10 obs (6 degrees of freedom) */
			 and b._EDF_ > 6    
	     ) b
		on a.firmQuarter = b.firmQuarter ;
quit;
/* Modified Jones model: drev is used in first model, but drevadj is used to compute fitted value */
proc sql;
 create table e_model2 as select a.*, b.DA_mJones, abs (b.DA_mJones) as ABSDA_mJones	
	from 
		e_model1 a left join 
 			( select a.firmQuarter, a.tac - ( b.intercept + a.drevadj * b.drev + %do_over(values=inv_at_l ppe, between=%str(+), phrase=a.? * b.?) ) as DA_mJones
			 from e_model1 a left join d_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 1 */
			 and b._MODEL_ eq "MODEL1"
			 /* at a minimum 10 obs (6 degrees of freedom) */
			 and b._EDF_ > 6    
	     ) b
		on a.firmQuarter = b.firmQuarter ;
quit;


/* Winsorize discretionary accrual variables  */
%let winsVars = DA_Jones DA_mJones ABSDA_Jones ABSDA_mJones  ; 
%winsor(dsetin=e_model2, dsetout=e_wins, /*byvar=, */ vars=&winsVars, type=winsor, pctl=1 99);

/* Means, medians for key variables */
proc means data=e_wins n mean median ;
var tac inv_at_l drevadj ppe roa_l &winsVars ;
run; 


