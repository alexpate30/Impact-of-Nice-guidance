*********************************************************************;
* STUDY: GPRD                                                       *;
* PROGRAM:                                                          *;
    %LET PGM = create_bmi3.sas;
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

*/ file with libname statements for this study */;
%include "/mnt/ja01-home01/mbrxsap3/phd_risk/code/runprg_tjeerd/libb AP.sas";

%include "&mmacroprg.\riskfactor1.sas";
%include "&mmacroprg.\extract_data1.sas";  
%include "&mmacroprg.\import1.sas";  


****************************************************************************************************;

*/ macro to estimate bmi */;
%macro bmi (iindata, databmi_addclin, timemeas1=, timemeas2=);

/* Here three datasets are created, one outputs bmi values, one outputs weight values and one height values */
data bmi (keep=patid eventdate bmi) height (keep=patid eventdate height) weight (keep=patid eventdate weight); 
  set &databmi_addclin;

  bmi=data3;
  if bmi < 10 or bmi > 70 then bmi=.;

  if enttype=13 then weight=data1;
  if weight < 30 then weight=.;
  if enttype=14 then height=data1;
  if height < 0.5 or height > 2.5 then height=.;

  if bmi NE . then output bmi;
  if height NE . then output height;
  if weight NE . then output weight;
run;

proc univariate data=bmi;
   var bmi;
run;
proc univariate data=weight;
   var weight;
run;
proc univariate data=height;
   var height;
run;

proc sort data=height;by patid;run;
proc sort data=weight;by patid;run;
proc sort data=bmi;by patid;run;

/* This ensures we only have height dates for adults, as we are going to take an average */
*/ adults only */;
data iindata;
	set &iindata;
run;
proc sort data=iindata; by patid;run;

data ypat;
  set iindata(keep=patid yob);
  by patid;
  if first.patid;
run;


data height;
  merge height (in=ina) ypat (in=inb keep=patid yob);
  by patid;
  if ina and inb and year(eventdate)-yob >= 18 then output;
run;
data weight;
  merge weight (in=ina) ypat (in=inb keep=patid yob);
  by patid;
  if ina and inb and year(eventdate)-yob >= 18 then output;
run;
proc sort data=weight;
  by patid eventdate;
run;

/* This calculates the average height of the person across all values recorded */
*/ average of height for person */;
data height1;
  set height;
  by patid;
  if first.patid then do;
     numhgt=0;
     numrec=0;
  end;
  numrec+1;
  numhgt+height;
  if last.patid then do;
     heightpers=numhgt/numrec;
     output;
  end;
  keep patid heightpers;
run;
proc univariate data=height1;
  title 'height per person';
  var heightpers;
run;


*/ average of weight per date */;
data weight1;
  set weight;
  by patid eventdate;
  if first.eventdate then do;
     numwgt=0;
     numrec=0;
  end;
  numrec+1;
  numwgt+weight;
  if last.eventdate then do;
     weight=numwgt/numrec;
     output;
  end;
  keep patid weight eventdate;
run;
proc univariate data=weight1;
  title 'weight per date';
  var weight;
run;


/* This merges the height and weight files and calculates an estimated bmi */
*/ merge with height */;
data bmi1;
  merge weight1 (in=ina) height1 (in=inb);
  by patid;
  if ina and inb then do;
     bmi_estim=weight/(heightpers*heightpers);
     output;
  end;
  keep patid eventdate bmi_estim;
run;
proc univariate data=bmi1;
  title 'bmi estimated';
  var bmi_estim;
run;

/* This reads in the bmi file (bmi directly input into medical record) and bmi1 file (bmi calculated from height and weight) */
data bmifinal;
  merge bmi (in=ina) bmi1 (in=inb);
  by patid eventdate;
  if ina or inb then do;
    if inb and not ina then bmi=bmi_estim;
    if bmi < 10 or bmi > 70 then bmi=.;
    bmi0=bmi;
    if bmi0 NE . then output;
  end; 
  keep patid bmi0 eventdate;
run;
proc univariate data=bmifinal;
  title 'final bmi';
  var bmi0;
run;

/* Risk_creat2 takes a table of all the measurements for a cohort and calculates the risk factor within a timeframe of the index_date */
*/ creation of bmi in original data */;

/* 
iindata is the cohort of patients
Dateref:
- dateref is put into the 'window1' variable in risk_creat1
- This window variable is used to sort the input dataset of people, then to calculate ddiff=abs(eventdate-&window1);
- Then the data is sorted by diff, and the first value is outputted
proc sort;
  by patid ddiff;
run;
data rxmed&kk;
  set rxmed&kk;
  by patid ddiff;
  if first.patid then output;
  drop ddiff;
run;

I think window1 is meant to be the index date, and we keep the closest value to this index date (as the abs function is used). 
Therefore dateref should be the index_date? should change to index_date_&cohort? Is this a problem that not all data is historical?
Could end up using a bmi that is from after the index date

bmifinal is the output from all previous stuff, is the dataset containing bmi values
bmi_index is what we are calling the variable?
bmi0 is what the column with bmi values in is called
The line "and eventdate <=dateref+&timemeas1 and eventdate >=dateref-&timemeas2" precedes 'then output' in a datastep. 

*/

%risk_creat2 (&iindata, index_date_A, bmifinal, bmi_index, bmi0, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);

%mend;


****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;

*/ input raw smoking data from addclin details and med files  */;

%macro sel1_bmi;
 if enttype in (13) then output;  */ weight */;  
 if enttype in (14) then output;  */ height*/;  
%mend;

%testadd_extract1 (ttaddclin_bmi, rctdata.sel1addclin_, sel1_bmi);

%bmi (p1.allpersA, ttaddclin_bmi, timemeas1=0, timemeas2=1827);


/*
%macro ttr;
%do xx=3 %to 11;
*/
*/ timemeas is time-period for measuring value */;
/*%bmi (casecon.casecon&xx, ttaddclin_bmi, timemeas1=0, timemeas2=1827);
%bmi (casecon.caseconb&xx, ttaddclin_bmi, timemeas1=0, timemeas2=1827);
%end;
%mend;
%ttr;
*/
