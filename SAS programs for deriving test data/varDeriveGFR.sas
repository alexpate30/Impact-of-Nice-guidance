*********************************************************************;
* STUDY: GPRD                                                       *;
* PROGRAM:                                                          *;
    %LET PGM = create_gfr.sas  Alex: GFR.sas;
* DIRECTORY:                                                        *;
* STATISTICIAN/PROGRAMMER: ___ S. Eaton                             *;
*                                                                   *;
*   first creation person dataset with extraction of relevant data from Rx medical etc files  *;
*********************************************************************;
options ps=60 ls=70;
FOOTNOTE1 "PROGRAM: &PGM, (&SYSDATE, &SYSTIME)"    ;
run;


****************************************************************************************************;
*/ file with programs called for this study */;

%include "/mnt/ja01-home01/mbrxsap3/phd_risk/code/runprg_tjeerd/libb AP.sas";

%include "&mmacroprg.riskfactor1.sas";
%include "&mmacroprg.extract_data1.sas";  
%include "&mmacroprg.import1.sas";  

/* It is important to note that eGFR scores are either recorded directly by the GP, or can be calculated from creatinine levels */
/* I will calculate eGFR_GP, and eGFR_CR, I will compare these to see if they are systematically different, and also will choose */
/* which one I believe to be more reliable if a patient has both */



/* First do the GFR scores that are directly input, for some reason I am using between 0 and 150 as sensible values */
/* Need to change this after speaking to Sabine or Paolo */
****************************************************************************************************;
*/ macro to estimate gfr scores, as entered directly by the GP */;
%macro gfr_gp(iindata, datapressure);
proc contents data=&datapressure;
run;

data step1_gfr_gp;
  set &datapressure;
  if enttype ^=466 then delete;
  gfr_gp1=data2;
  *drop data3-data6;
run;
proc univariate data=step1_gfr_gp;
  title 'original blood pressure data';
  var gfr_gp1;
run;
proc sort data=step1_gfr_gp;
  by patid eventdate gfr_gp1;
run;

*/ removal of extreme values */;
data step1_gfr_gp;
  set step1_gfr_gp;
  if gfr_gp1 > 0 and gfr_gp1<=250 then output;
run;
proc univariate data=step1_gfr_gp;
  title 'blood pressure data after removal extremes';
  var gfr_gp1;
run;


*/ lowest gfr_gp at given date */;
data step1_gfr_gp;
  set step1_gfr_gp;
  by patid eventdate gfr_gp1;
  if first.eventdate then output;
  keep patid eventdate gfr_gp1 data3;
run;

/* Bring in cohort of people to merge with */
data allpers;
	set &iindata;
	proc sort; by patid;
run;

/* Do the merge, and create egfr deriv variable to say how it was dervied */
data p2.egfr_gp_scores (keep = patid pracid gfr_gp1 egfr_deriv eventdate rename = (gfr_gp1 = egfr));
	merge step1_gfr_gp (in=ina) allpers (in=inb);
	by patid;
	egfr_deriv = 'gp';
	if ina and inb then output;
run;

*/ creation of systolic in timeperiod */;
*%risk_creat2 (&iindata, index_date_A, step1_gfr_gp, gfr_gp, gfr_gp1, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2, A);

%mend;



/* Next do the GFR scores that are calculated from the creatinine levels */
/* Need to find out what valid values are for creatinine */
/* Also an ethnicity is required in order to calculate the GFR score */
/* If this is missing, then GFR will be missing also (check this with Tjeerd?? Seems better than guessing the ethnicity) */


/* NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB */
/* ******************************************************************************************************* */
/*   Note that this requires the ethnicity and age variables for cohort A/B to already have been derived   */
/* ******************************************************************************************************* */
/* NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB */



/* Note that you need age and ethnicity in order to derive gfr values from creatinine */
/* I use the varAge_A and varEthnicity_A files to do this */
/* This works for deriving gfr's for cohort B as all patients in cohort B are in cohort A */
/* If we wanted to derive egfrs for other patients, would need to change code */


****************************************************************************************************;
*/ macro to estimate gfr scores through creatinine levels in the blood */;
%macro gfr_cr(iindata, datapressure, cohort);
proc contents data=&datapressure;
run;

data step1_gfr_cr;
  set &datapressure;
  if enttype ^=165 then delete;
  /* Only output entries where unit = umol/L, as decided by results of test_creatinine_A */
  if data3 ^= 142 then delete;
  /* Delete if entry = 0 */
  if data2 = 0 or data2 = . then delete;
  if data2 < 0 or data2 > 4000 then delete;
  creatinine=data2;
  *drop data3-data6;
run;
proc univariate data=step1_gfr_cr;
  title 'original blood pressure data';
  var creatinine;
run;
proc sort data=step1_gfr_cr;
  by patid eventdate creatinine;
run;

*/ removal of extreme values */;
data step1_gfr_cr;
  set step1_gfr_cr;
run;
proc univariate data=step1_gfr_cr;
  title 'blood pressure data after removal extremes';
  var creatinine;
run;


*/ highest creatinine at given date (indicates worst) */;
data step1_gfr_cr;
  set step1_gfr_cr;
  by patid eventdate creatinine;
  if last.eventdate then output;
  keep patid eventdate creatinine;
run;

/* Now need to mix with ethnicity */
/* First get the ethnicites for all patients in cohort A */
data ethnicity;
	set p2.varEthnicity_&cohort;
	proc sort; by patid;
run;

/* Merge wtih all the creatinine values */
/* Only output creatinine for patients in cohort A (in the ethnicity file) */
data step2_gfr_cr;
	merge step1_gfr_cr (in=ina) ethnicity (in=inb);
	by patid;
	if ina and inb then output;
run;

/* Delete entries where ethnicity is missing */
data step3_gfr_cr;
	set step2_gfr_cr;
	if ethnicity = 'unclas' then delete;
	if ethnicity = 'missing' then delete;
run;
/* Check to see range of ethnicities is correct */
proc freq data=step3_gfr_cr; table Ethnicity;run;


/* Now merge with the age variable */
data age;
	set p2.varAge_&cohort;
	proc sort; by patid;
run;

data step4_gfr_cr;
	merge step3_gfr_cr (in=ina) age (in=inb);
	by patid;
	if ina and inb then output;
run;

/* Finally merge with p1.allpersA/B to get the gender */
data allpers (keep = patid gender);
	set &iindata;
	proc sort; by patid;
run;

data step5_gfr_cr;
	merge step4_gfr_cr (in=ina) allpers (in=inb);
	by patid;
	if ina and inb then output;
run;

/* Make conversion from umol/L to mg/dL */
data step6_gfr_cr;
	set step5_gfr_cr;
	creatinine = creatinine/88.42;
run;
/* Check it all seems legit */
proc means data=step6_gfr_cr n nmiss mean std median range min max;var creatinine;run;

/* Calculate gfr from creatinine */
data step7_gfr_cr;
	set step6_gfr_cr;
	if ethnicity = 'black' then do;
		if gender = 2 then do;
			if creatinine <= 0.7 then egfr = 166*(creatinine/0.7)**(-0.329)*0.993**age;
			if creatinine > 0.7 then egfr = 166*(creatinine/0.7)**(-1.209)*0.993**age;
		end;
		if gender = 1 then do;
			if creatinine <= 0.9 then egfr = 163*(creatinine/0.9)**(-0.411)*0.993**age;
			if creatinine > 0.9 then egfr = 163*(creatinine/0.9)**(-1.209)*0.993**age;
		end;
	end;
	if ethnicity ^= 'black' then do;
		if gender = 2 then do;
			if creatinine <= 0.7 then egfr = 144*(creatinine/0.7)**(-0.329)*0.993**age;
			if creatinine > 0.7 then egfr = 144*(creatinine/0.7)**(-1.209)*0.993**age;
		end;
		if gender = 1 then do;
			if creatinine <= 0.9 then egfr = 141*(creatinine/0.9)**(-0.411)*0.993**age;
			if creatinine > 0.9 then egfr = 141*(creatinine/0.9)**(-1.209)*0.993**age;
		end;
	end;
run;

/* Create egfr_deriv variable to say how egfr was derived, also exclude all over 250 (same as egfr_gp file) */
data p2.egfr_cr_scores (keep = patid pracid egfr egfr_deriv eventdate);
	set step7_gfr_cr;
	egfr_deriv= 'cr';
	if egfr < 250 then output;
run;

*/ creation of systolic in timeperiod */;
*%risk_creat2 (&iindata, index_date_A, step1_gfr_gp, gfr_gp, gfr_gp1, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2, A);

%mend;


****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;

*/ input codes with blood pressure */;
%macro sel1_gfr_gp;
*/ ADDITIONAL CLINICAL data */;
 if enttype in (466) then output;  */ systolic blood pressure*/;  
%mend;

*/ extract gfr scores */;
%testadd_extract1 (tttest_gfr_gp, rctdata.sel1test_, sel1_gfr_gp);

*/ timemeas is time-period for measuring value - selected on idex date or in 5 years before -after*/;
%gfr_gp (p1.allpersA, tttest_gfr_gp);


proc delete data=work._all_;
/*Delete all the tables created in this program*/
run;


*/ Extract all the gfr scores calculated through creatinine */ */;
%macro sel1_gfr_cr;
*/ ADDITIONAL CLINICAL data */;
 if enttype in (165) then output;  */ creatinine */;  
%mend;

*/ extract creatinine measurements */;
%testadd_extract1 (tttest_gfr_cr, rctdata.sel1test_, sel1_gfr_cr);

%gfr_cr (p1.allpersA, tttest_gfr_cr, A);

/* Note that you need age and ethnicity in order to derive gfr values from creatinine */
/* I use the varAge_A and varEthnicity_A files to do this */
/* This works for deriving gfr's for cohort B as all patients in cohort B are in cohort A */
/* If we wanted to derive egfrs for other patients, would need to change code */

/* Concatenate the egfr scores into one */
data egfr_scores;
	set p2.egfr_cr_scores p2.egfr_gp_scores;
	proc sort; by patid;
run;


/* Test data */
/*
ODS PDF FILE="/mnt/ja01-home01/mbrxsap3/phd_risk/output/p2_derive_variables/aa_test_egfr_scores.pdf";
proc print data=p2.egfr_cr_scores(obs=10);run;
proc print data=p2.egfr_gp_scores(obs=10);run;
proc means data=egfr_scores; var egfr; by egfr_deriv;run;
proc univariate data=egfr_scores; var egfr; histogram egfr; by egfr_deriv;run;
ods pdf close;
*/
/* Note they are not really comparable as at lots of labs/hospitals they dont record anything over 60 or 90 */


/* The above code extracts ALL gfr values (from either gfr or creatinine measurements) */
/* Want to combine, and then output ones prior to the index date for each cohort */ 

%macro format_data_for_CKD_calc(cohort, timemeas1=, timemeas2=);

/* First read in person file */
data allpers;
	set p1.allpers&cohort;
	time_valid = index_date_&cohort - dtvalid;
	proc sort; by patid;
run;

/* Combine the egfrs dervied from creatinine and dervied directly */
data egfr_scores2;
	merge egfr_scores (in=ina) allpers (in=inb);
	by patid;
	if ina and inb then output;
run;

/* Restrict only to entries that are prior to the index date, in the previous three years */

/* WHY AM I ONLY LOOKING IN PREV THREE YEARS ????? */

data egfr_scores3 (keep = patid pracid eventdate egfr egfr_deriv index_date_&cohort);
	set egfr_scores2;
	if index_date_&cohort - &timemeas2 < eventdate < index_date_&cohort + &timemeas1 then output;
run;

/* Sort by eventdate, so most recent events first */
proc sort data=egfr_scores3; by patid descending eventdate;run;

data p2.egfr_scores_&cohort;
	set egfr_scores3;
	retain num;
	by patid descending eventdate;
	if first.patid then num + 1;
	output;
run;

proc print data=p2.egfr_scores_&cohort(obs=100);

%mend format_data_for_CKD_calc;

%format_data_for_CKD_calc(A,timemeas1=0,timemeas2=1827);
%format_data_for_CKD_calc(B,timemeas1=0,timemeas2=1827);


proc export data=p2.egfr_scores_A dbms = csv outfile="/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/egfr_scores_A.csv" replace;run;
proc export data=p2.egfr_scores_A dbms = tab outfile="/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/egfr_scores_A.txt" replace;run;

proc export data=p2.egfr_scores_B dbms = csv outfile="/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/egfr_scores_B.csv" replace;run;
proc export data=p2.egfr_scores_B dbms = tab outfile="/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/egfr_scores_B.txt" replace;run;
