/*
	Earnings management models
	Author: Joost Impink, March 2016
		- Oct 2018: Added Ball and Shivakumar 2006, Dechow and Dichev 2002 measures
	Models estimated:
	- Jones model, 	tac = a0 + a1 1/TAt-1 + a2chSales + a3PPE + error.
		- variable names DA_Jones ABSDA_Jones 
	- Modified Jones model, as Jones model, but using chSales - chREC to compute fitted values.
		- variable names DA_mJones ABSDA_mJones 
	- Kothari 2005, controlling for ROA, tac = a0 + a1 1/TAt-1 + a2(chSales - chREC) + a3PPE + a4ROA + error.
		- variable names DA_Kothari ABSDA_Kothari  
	- Kothari 2005, performance matched, Jones model, difference in discretionary accruals between firm and closest firm in terms of roa
		- variable names DA_pmKothari ABSDA_pmKothari  
	- Ball and Shivakumar 2006, with operating cash flow, dummy for negative ocf and interaction
		- variable names DA_BS, ABSDA_BS
	- Dechow and Dichev, modified by McNichols, with past present and next period ocf 
		- variable names DA_DD, ABSDA_DD
	- Kothari, Mizik, Rowchardury 2015, add net income to modified Jones model, estimate by industry with firm and year dummies
		- variable names DA_Kothari2015 ABSDA_Kothari2015
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
*/

/* Assign directory for output */
%let projectDir = F:/temp/da/;

libname da "&projectDir";

/* Include %array and %do_over */
filename m1 url 'https://gist.githubusercontent.com/JoostImpink/c22197c93ecd27bbf7ef/raw/2e2a54825c9dbfdfd66cfc94b9abe05e9d1f1a8e/array.sas';
%include m1;
filename m2 url 'https://gist.githubusercontent.com/JoostImpink/c22197c93ecd27bbf7ef/raw/2e2a54825c9dbfdfd66cfc94b9abe05e9d1f1a8e/do_over.sas';
%include m2;

/* Winsorize macro */
filename m3 url 'https://gist.githubusercontent.com/JoostImpink/497d4852c49d26f164f5/raw/11efba42a13f24f67b5f037e884f4960560a2166/winsorize.sas';
%include m3;

/* Get Funda variables */
%let fundaVars = at ni ib ibc oancf ppegt rect sale ;


data da.a_funda (keep = key gvkey fyear datadate sich &fundaVars);
set comp.funda;
/* Period */
if 2005 <= fyear <= 2016;
/* Generic filter */
if indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C' ;
/* Firm-year identifier */
key = gvkey || fyear;
/* Keep if sale > 0, at > 0 */
if sale > 0 and at > 0;
/* Use Income Before Extraordinary Items (Cash Flow) if ib is missing */
if ib =. then ib=ibc;
run;

/* Lagged values for: at sale rect ib */
%let lagVars = at sale rect ib oancf;

/* Self join to get lagged values at_l, etc */
proc sql;
 create table da.a_funda2 as select a.*, %do_over(values=&lagVars, between=comma, phrase=b.? as ?_l)
 from da.a_funda a, da.a_funda b
 where a.gvkey = b.gvkey and a.fyear-1 = b.fyear;
quit;

/* next period's oancf => oancf_n */
proc sql;
 create table da.a_funda3 as select a.*, b.oancf as oancf_n 
 from da.a_funda2 a left join da.a_funda2 b
 on a.gvkey = b.gvkey and a.fyear+1 = b.fyear;
quit;


/* Construct additional variables */
data da.a_funda3;
set da.a_funda3;
/* 2-digit SIC  */
SIC2 = int(sich/100);
/* variables */
tac       = (ib - oancf)/at_l;  /* alternative: tac        = (ib-oancf+xidoc)/at_l */
inv_at_l  = 1 / at_l;
rev       = sale / at_l;
drev      = (sale - sale_l) / at_l;
drevadj   = (sale - sale_l)/at_l - (rect - rect_l)/at_l;
ppe       = ppegt / at_l;
netinc    = ni / at;
roa 	  = ib / at;
roa_l 	  = ib_l/ at_l; /* net income before extraordinary items */
cfo 	  = oancf / at_l;
dcfo	  = (oancf < 0); /* dcfo is 1 if oancf is negative */
if (missing(cfo) eq 1) then dcfo = .; /* set dcfo to missin gif cfo is missing */
/* these variables may not be missing (cmiss counts missing variables)*/
if cmiss  (of tac inv_at_l drevadj ppe roa_l) eq 0;
run;

/* Winsorize  */
%let winsVars = tac inv_at_l drev drevadj ppe roa_l netinc cfo; 
%winsor(dsetin=da.a_funda3, dsetout=da.a_funda_wins, /*byvar=, */ vars=&winsVars, type=winsor, pctl=1 99);

data da.a_funda_wins;
set da.a_funda_wins;
/* interaction of cfo and dcfo */
dcfo_cfo  = cfo * dcfo; 
run;

/* get prev and next cfo */
proc sql;
	create table da.a_funda_wins2 as select a.*, b.cfo as cfo_l
	from da.a_funda_wins a left join da.a_funda_wins b 
	on a.gvkey = b.gvkey and a.fyear - 1 = b.fyear;
quit;
proc sql;
	create table da.a_funda_wins3 as select a.*, b.cfo as cfo_n
	from da.a_funda_wins2 a left join da.a_funda_wins2 b 
	on a.gvkey = b.gvkey and a.fyear + 1 = b.fyear;
quit;

/* Regression by industry-year 
 edf + #params (4) will equal the number of obs (no need for proc univariate to count) */
proc sort data=da.a_funda_wins3; by fyear sic2;run;
proc reg data=da.a_funda_wins3 noprint edf outest=da.c_parms;
model tac = inv_at_l drev ppe /noint; /* Jones Model */
model tac = inv_at_l drevadj ppe roa_l ; /* Kothari with ROA in model - with intercept*/     
model tac = inv_at_l drev ppe cfo dcfo dcfo_cfo /noint; /* Ball and Shivakumar 2006 */
model tac = inv_at_l drev ppe cfo_l cfo cfo_n /noint; /* Dechow and Dichev, modified by McNichols */
by fyear sic2;
run;

/* Append discretionary accrual measures */

/* Jones model */
proc sql;
 create table da.d_model1 as select a.*, b.DA_Jones, abs (b.DA_Jones) as ABSDA_Jones	
	from 
		da.a_funda_wins3 a left join 
 			( select a.key, a.tac - ( %do_over(values=inv_at_l drev ppe, between=%str(+), phrase=a.? * b.?) ) as DA_Jones
			 from da.a_funda_wins3 a left join da.c_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 1 */
			 and b._MODEL_ eq "MODEL1"
			 /* at a minimum 10 obs (6 degrees of freedom) */
			 and b._EDF_ > 6    
	     ) b
		on a.key = b.key ;
quit;

/* Modified Jones model: drev is used in first model, but drevadj is used to compute fitted value */
proc sql;
 create table da.d_model2 as select a.*, b.DA_mJones, abs (b.DA_mJones) as ABSDA_mJones	
	from 
		da.d_model1 a left join 
 			( select a.key, a.tac - ( a.drevadj * b.drev + %do_over(values=inv_at_l ppe, between=%str(+), phrase=a.? * b.?) ) as DA_mJones
			 from da.d_model1 a left join da.c_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 1 */
			 and b._MODEL_ eq "MODEL1"
			 /* at a minimum 10 obs (6 degrees of freedom) */
			 and b._EDF_ > 6    
	     ) b
		on a.key = b.key ;
quit;

/* Kothari model (with ROA in regression) */
proc sql;
 create table da.d_model3 as select a.*, b.DA_Kothari, abs (b.DA_Kothari) as ABSDA_Kothari	
	from 
		da.d_model2 a left join 
 			( select a.key, a.tac - ( b.intercept + %do_over(values=inv_at_l drevadj ppe roa_l, between=%str(+), phrase=a.? * b.?) ) as DA_Kothari
			 from da.d_model2 a left join da.c_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 2 */
			 and b._MODEL_ eq "MODEL2"
			 /* at a minimum 10 obs (5 degrees of freedom) */
			 and b._EDF_ > 5    
	     ) b
		on a.key = b.key ;
quit;

/*	Kothari performance matching: get DA_Jones accruals for closest in ROA */
proc sql;
	create table da.d_model4 as
		select a.*,
		/* gvkey of matched firm */
		b.gvkey as gvkey_m, 
		/* difference in ROA */
		abs(a.roa - b.roa) as Difference, 
		/* difference in DA_Jones (and absolute difference) */
		a.DA_Jones - b.DA_Jones as DA_pmKothari, abs (calculated DA_pmKothari) as ABSDA_pmKothari
		from da.d_model3 a left join  da.d_model3  b
		on a.fyear = b.fyear and a.sic2 = b.sic2 /* same 2-digit SIC industry-year */		
		and a.key ne b.key /* not the same firm */
		group by a.gvkey, a.fyear
		having Difference = min(Difference); /* keep best match for size difference */		
quit;

/* drop possible multiple matches (with the same difference) in previous step */
proc sort data=da.d_model4 nodupkey; by key;run;


/* Ball Shivakumar 2006 */
proc sql;
 create table da.d_model5 as select a.*, b.DA_BS, abs (b.DA_BS) as ABSDA_BS	
	from 
		da.d_model4 a left join 
 			( select a.key, a.tac - (  %do_over(values=inv_at_l drev ppe cfo dcfo dcfo_cfo, between=%str(+), phrase=a.? * b.?) ) as DA_BS
			 from da.d_model4 a left join da.c_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 2 */
			 and b._MODEL_ eq "MODEL3"
			 /* at a minimum 10 obs (5 degrees of freedom) */
			 and b._EDF_ > 5    
	     ) b
		on a.key = b.key ;
quit;

proc sql;
 create table da.d_model6 as select a.*, b.DA_DD, abs (b.DA_DD) as ABSDA_DD	
	from 
		da.d_model5 a left join 
 			( select a.key, a.tac - (  %do_over(values=inv_at_l drev ppe cfo_l cfo cfo_n, between=%str(+), phrase=a.? * b.?) ) as DA_DD
			 from da.d_model5 a left join da.c_parms b
			 on a.sic2 = b.sic2 and a.fyear = b.fyear
			 /* Model 2 */
			 and b._MODEL_ eq "MODEL4"
			 /* at a minimum 10 obs (5 degrees of freedom) */
			 and b._EDF_ > 5    
	     ) b
		on a.key = b.key ;
quit;



/* Kothari 2015, regression by industry with year and firm fixed effects */
proc sort data=da.a_funda_wins3; by sic2;run;

proc glm data=da.a_funda_wins3;
  class fyear gvkey; 
  /* Regression output (for further inspection, not needed for residuals) */
  ods output	ParameterEstimates  = work.params 
	        	FitStatistics 		= work.fit
            	NObs 				= work.obs;
  model tac = inv_at_l drevadj ppe netinc fyear gvkey /solution  ;
  output out=da.d_model7 (keep=gvkey fyear DA_Kothari2015) residual=DA_Kothari2015 ;  
  by sic2;
run;
quit;

/* Append Kothari 2015 measure */
proc sql;
	create table da.d_model8 as select a.*, b.DA_Kothari2015, abs(b.DA_Kothari2015) as ABSDA_Kothari2015
	from da.d_model6 a left join da.d_model7 b on a.gvkey = b.gvkey and a.fyear = b.fyear;
quit;

/* Winsorize discretionary accrual variables  */
%let winsVars = DA_Jones DA_mJones DA_Kothari DA_pmKothari ABSDA_Jones  ABSDA_mJones  ABSDA_Kothari ABSDA_pmKothari DA_Kothari2015 ABSDA_Kothari2015 DA_BS ABSDA_BS DA_DD ABSDA_DD; 
%winsor(dsetin=da.d_model8, dsetout=da.e_wins, /*byvar=, */ vars=&winsVars, type=winsor, pctl=1 99);

/* Means, medians for key variables */
proc means data=da.e_wins n mean median ;
var tac inv_at_l drevadj ppe roa_l &winsVars ;
run; 

