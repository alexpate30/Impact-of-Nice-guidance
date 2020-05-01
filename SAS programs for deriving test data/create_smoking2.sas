*********************************************************************;
* STUDY: GPRD                                                       *;
* PROGRAM:                                                          *;
    %LET PGM = create_smoking2.sas  Susan: smoking.sas;
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

*/ macro to estimate smoking */;
%macro smoking (iindata, datasmok_addclin, datasmok_med, timemeas1=, timemeas2=);


**** Setting up smoking variable *****;
  * where 0= nonsmoker *;
  *       1= exsmoker  *;
  *       2= smoker    *;

*/ cleaning of smoking data */;
data numcig1 (keep=patid eventdate ttnumcigs_perday cigarettes_perday cigars_perday tobacco_perday) smoking1 (keep=patid smoking eventdate) stopsmok1 (keep=patid startsmoke stopsmoke eventdate);
  set &datasmok_addclin;

  format data1 1.0;
  if data1=0 then smoking=.;
  if data1=1 then smoking=2;
  if data1=2 then smoking=0;
  if data1=3 then smoking=1;

  /* TJEERD OLD CODE */
  /*
  startsmoke=data5;
  stopsmoke=data6;
  if startsmoke in(0,2,3) then startsmoke=.;
	else if startsmoke > 3 then do;
          %modify_dates (startsmoke);
      end;
  if stopsmoke in(0,2,3) then stopsmoke=.;
	else if stopsmoke > 3 then do;
          %modify_dates (stopsmoke);
      end;
  */

  /* MY NEW CODE */
  format startsmoke stopsmoke ddmmyys10.;
	startsmoke = input(put(data5,Z8.),yymmdd8.);
	stopsmoke = input(put(data6,Z8.),yymmdd8.);

  cigarettes_perday=data2; cigars_perday=data3;  tobacco_perday=data4; 
  if cigarettes_perday > 120 or cigarettes_perday <= 0 then cigarettes_perday = .;
  if cigars_perday > 120 or cigars_perday <= 0 then cigars_perday = .;
  if tobacco_perday > 120 or tobacco_perday <= 0 then tobacco_perday = .;

  if cigarettes_perday NE . then ttnumcigs_perday=cigarettes_perday;
  else if cigars_perday NE . then ttnumcigs_perday=cigars_perday;
  else if tobacco_perday NE . then ttnumcigs_perday=tobacco_perday;

  if ttnumcigs_perday > 0 and smoking NE 2 then smoking=2;  */ if no record of smoking */;

  if ttnumcigs_perday NE . then output numcig1;
  if smoking NE . then output smoking1;
  if startsmoke NE . or stopsmoke NE . then output stopsmok1;
run;
proc freq data=smoking1;
  title 'original smoking data ';
  tables smoking;
run;
proc univariate data=numcig1;
   var ttnumcigs_perday cigarettes_perday cigars_perday tobacco_perday;
run;
proc univariate data=stopsmok1;
   var startsmoke stopsmoke;
run;


*/ merge with code list to obtain variables status indicating current-ex smoking based on read code */;
proc sort data=smoking_codes;
  by medcode;
run;
data smoking_codes;
  set smoking_codes;
  by medcode;
  if first.medcode;
run;

data medical_status;
  set &datasmok_med;
run;
proc sort data=medical_status;
  by medcode;
run;

**** Setting up smoking variable *****;
  * where 0= nonsmoker *;
  *       1= exsmoker  *;
  *       2= smoker    *;
data smoking2;
  merge medical_status (in=ina) smoking_codes (in=inb);
  by medcode;
  if ina and inb then do;
    if status="No" then smoking=0;
    if status="Ex" then smoking=1;
    if status="Yes" then smoking=2;
    if smoking NE . then output;
  end;
  keep patid eventdate smoking;
run;


*/ combination of additional detail and medical file for smoking status */;
data smoking0;
  set smoking1 smoking2;
  if eventdate NE .;
run;
proc sort data=smoking0;
  by patid eventdate descending smoking;
run;
proc freq data=smoking0;
  title 'raw medical read codes';
  tables smoking;
run;


**   Keeping highest value on a given eventdate **;
data smoking0;
  set smoking0;
  by patid eventdate descending smoking;
  if first.eventdate then output;
run;

/* EDIT CODE: so that if a patient has smoked in the past, a non-smoker record is changed to ex-smoker */
/* First create a variable smokepast that will = 0 if a patient has only ever not smoked prior to and including date,
   will = 1 if patient has only ever been an ex-smoker prior to and including the date, and will = 2 if patient
   has ever smoked in the past */
data smoking_edit;
	set smoking0;
	retain retain_smoking;
	by patid eventdate;
	if first.patid then retain_smoking = smoking;
	elso do;
		if smoking = 1 and retain_smoking < 1 then do;
			retain_smoking = smoking;
		end;
		if smoking = 2 and retain_smoking < 2 then do;
			retain_smoking = smoking;
		end;
	end;
	output;
run;

/* Print to check its working properly */
proc print data=smoking_edit(obs=300);run;

/* If patient has smoked in past but has record as non smoker, change to ex-smoker */
data smoking_edit2 (drop=smoking_new);
	set smoking_edit;
	if smoking = 0 and retain_smoking > 0 then smoking = 1;
run;

/* Print to check its working properly */
proc print data=smoking_edit2(obs=300);run;

**** smoking variable gets reduced to smokingnoex ***;
  * where 0=nonsmoker *;
  *       2 = smoker *;
  *  based on whether the most recent smoking observation is more or less than 5 years from the index date *;
data smoking0 (drop = retain_smoking);
  set smoking_edit2;
  by patid eventdate descending smoking;
  retain tdate;
  if first.patid then tdate=.;
  
  if tdate NE . then diff=eventdate-tdate;
  smokingnoex = smoking;
  if smoking=1 then do;
    if diff <=1460  then smokingnoex=2;  */ less than 4 years ago - past smoker set to smoker */;
    if diff > 1460 then smokingnoex=0;  */ more than 4 years ago, past smoker set to non-smoker */;
  end;

  tdate=eventdate;

  ttsmoking=smoking;
  ttsmokingnoex=smokingnoex;
  drop smoking smokingnoex tdate diff;
run;
proc freq data=smoking0;
  title 'raw medical read codes';
  tables ttsmokingnoex;
run;


*/ creation of smoking in original data */;
%risk_creat2 (&iindata, index_date_A, smoking0, Smoking_index, ttsmoking, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);
%risk_creat2 (&iindata, index_date_A, smoking0, Smokingnoex_index, ttsmokingnoex, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);
%risk_creat2 (&iindata, index_date_A, numcig1, Numcigs_perday_index, ttnumcigs_perday, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);
*/

proc datasets library=work nolist;
  delete smoking0 smoking1 smoking2 numcig1 stopsmok1 medical_status;
%mend;


****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;

*/ input smoking codes with indicators status -current-ex smoker classification */;
%let smoke_code_loc=/mnt/ja01-home01/mbrxsap3/phd_risk/codelists/csv_v2/;  */ location of final code sets selected by researcher */;
%importcsv (smoking_codes, &smoke_code_loc.smoking_medcode, ddatarow=2);

proc print data=smoking_codes;run;

*/ input raw smoking data from addclin details and med files  */;
%macro sel1_smok;
 if enttype in (4) then output;  */ smoking*/;  
%mend;
%testadd_extract1 (ttaddclin_smoking, rctdata.sel1addclin_, sel1_smok);

%rxmed_extract1 (ttmedical_smoking, rctdata.sel1med_, medcode, smoking_codes);


*/ timemeas is time-period for measuring value */;
%smoking (p1.allpersA, ttaddclin_smoking, ttmedical_smoking, timemeas1=0, timemeas2=1827);
  
/*
proc freq data=statin.persstatin_hesons;
  title 'final in pat_index';
  tables smoking_index smokingnoex_index;
run;
*/
*/ only records at baseline */;

