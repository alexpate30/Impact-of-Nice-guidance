*********************************************************************;
* STUDY: GPRD                                                       *;
* PROGRAM:                                                          *;
    %LET PGM = create_systolic.sas  Susan: BP.sas;
* DIRECTORY:                                                        *;
* STATISTICIAN/PROGRAMMER: ___ S. Eaton                             *;
*                                                                   *;
*   first creation person dataset with extraction of relevant data from Rx medical etc files  *;
*********************************************************************;
options ps=60 ls=70;
FOOTNOTE1 "PROGRAM: &PGM, (&SYSDATE, &SYSTIME)"    ;
run;

/* IMPORTANT: AP */
/* IMPORTANT: AP */
/* IMPORTANT: AP */
/*

I have made added new code to this program that will work specifically for my project. The code will no longer be generalisable to projects
where there are multiple index dates in one person file.

*/
/* IMPORTANT: AP */
/* IMPORTANT: AP */
/* IMPORTANT: AP */


****************************************************************************************************;
*/ file with programs called for this study */;

%include "/mnt/ja01-home01/mbrxsap3/phd_risk/code/runprg_tjeerd/libb AP.sas";

%include "&mmacroprg.riskfactor1.sas";
%include "&mmacroprg.extract_data1.sas";  
%include "&mmacroprg.import1.sas";  


****************************************************************************************************;
*/ macro to estimate systolic blood pressure */;
%macro systolic_variation (iindata, datapressure, cohort, timemeas1=, timemeas2=);
proc contents data=&datapressure;
run;

data step1_bp;
  set &datapressure;
  if enttype ^=1 then delete;
  diastolic=data1;
  systolic=data2;
  drop data3-data6;
run;
proc univariate data=step1_bp;
  title 'original blood pressure data';
  var systolic;
run;
proc sort data=step1_bp;
  by patid eventdate systolic;
run;

*/ removal of extreme values */;
data step1_bp;
  set step1_bp;
  if systolic >= 70 and systolic<=210 then output;
run;
proc univariate data=step1_bp;
  title 'blood pressure data after removal extremes';
  var systolic;
run;


*/ lowest systolic pressure at given date */;
data step1_bp;
  set step1_bp;
  by patid eventdate systolic;
  if first.eventdate then output;
  keep patid eventdate systolic;
run;


/* BITS THAT I HAVE ADDED */
/* BITS THAT I HAVE ADDED */
/* BITS THAT I HAVE ADDED */

/* The name of the variable is systolic */
/* We want to output only ones in the last five years */
/* If one or less, SBP variability is set to missing */

/* Merge SBP values with the cohort of interest */
data all_pats;
	set &iindata;
	proc sort; by patid;
run;

/* Only output ones within timemeas2 days before the index date */
data all_pats_bp;
	merge all_pats (in=ina) step1_bp (in=inb);
	by patid;
	if ina and inb and index_date_&cohort - &timemeas2 < eventdate <= index_date_&cohort then output;
run;
proc sort data=all_pats_bp; by patid eventdate;run;

/* Create a count for number of measurements, and sum of measurements */
data all_pats_bp2;
	set all_pats_bp;
	by patid;
	if first.patid then do;
		meas_count = 1;
		*systolic_sum = systolic;
	end;
	else do; 
		meas_count + 1;
		*systolic_sum + systolic;
	end;
run;

proc sort data=all_pats_bp2;by patid;run;

proc means data=all_pats_bp2 noprint;var systolic;by patid;output out=means_out std=std;run;

data means_out2 (keep = patid std rename=(std=systolic_std));
	set means_out;
run;


data p2.varSystolic_Variability_&cohort (keep = patid pracid systolic_std);
	merge all_pats (in=ina) means_out2 (in=inb);
	by patid;
	if ina and ~inb then systolic_std = . ;
	if ina then output;
run;

proc sort data=p2.varSystolic_Variability_&cohort;by pracid patid;run;

%mend;


****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;

*/ input codes with blood pressure */;
%macro sel1_bp;
*/ ADDITIONAL CLINICAL data */;
 if enttype in (1) then output;  */ systolic blood pressure*/;  
%mend;

*/ systolic blood pressure */;
%testadd_extract1 (ttaddclin_systolic, rctdata.sel1addclin_, sel1_bp);



*/ timemeas is time-period for measuring value - selected on idex date or in 5 years before -after*/;
%systolic_variation (p1.allpersA, ttaddclin_systolic, A, timemeas1=0, timemeas2=1827);
*%systolic (q.pat_indexhesons, ttaddclin_systolic, timemeas1=0, timemeas2=1827);


proc print data=p2.varSystolic_Variability_A(obs=100);run;
proc means data=p2.varSystolic_Variability_A n nmiss min max mean sum;var systolic_std;run;

