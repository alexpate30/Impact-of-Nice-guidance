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


****************************************************************************************************;
*/ file with programs called for this study */;

%include "/mnt/ja01-home01/mbrxsap3/phd_risk/code/runprg_tjeerd/libb AP.sas";

%include "&mmacroprg.riskfactor1.sas";
%include "&mmacroprg.extract_data1.sas";  
%include "&mmacroprg.import1.sas";  


****************************************************************************************************;
*/ macro to estimate systolic blood pressure */;
%macro systolic (iindata, datapressure, timemeas1=, timemeas2=);
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



*/ creation of systolic in timeperiod */;
%risk_creat2 (&iindata, index_date_A, step1_bp, systolic_blood_pressure_index, systolic, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);

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
%systolic (p1.allpersA, ttaddclin_systolic, timemeas1=0, timemeas2=1827);
*%systolic (q.pat_indexhesons, ttaddclin_systolic, timemeas1=0, timemeas2=1827);


/*
proc univariate data=q.pat_indexhesons;
  title 'CHECK index q';
  var systolic_blood_pressure_index;
  where index='Q';
run;
proc univariate data=q.pat_indexhesons;
  title 'CHECK index random';
  var systolic_blood_pressure_index;
  where index='random';
run;
*/

