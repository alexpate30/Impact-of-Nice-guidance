*********************************************************************;
* STUDY: GPRD                                                       *;
* PROGRAM:                                                          *;
    %LET PGM = create_cholest2.sas;
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

proc format;
  value OPR
	0	= "Data Not Entered"
	1	= "<"
	2	= "<="
	3	= "="
	4	= ">"
	5	= ">="
	6	= "~";

  /*  Our documentation note this as SUM */
  /*
  value UNITS
	0 = "No Data Entered"
	1 =	"%"
	2 = "(bdsk u)"
	3 =	"(bsa)"
	4 =	"(cfu)"
	5 =	"(knk u)"
	6 =	"(mclg u)"
	7 =	"(od)"
	8 =	"(pa)"
	9 =	"(ph)"
	10 = "(ppb)"
	11 = "(ppm)"
	12 = "(pu)"
	13 = "(td u)"
	14 = "/(hpf)"
	15 = "/day"
	16 = "/hour"
	17 = "/L"
	18 = "/m3"
	19 = "=2/min"
	20 = "/minute"
	21 = "/mL"
	22 = "/month"
	23 = "/second"
	24 = "/week"
	25 = "/year"
	26 = "1"
	27 = "1(tot)"
	28 = "1/mL"
	29 = "10*-2(%)"
	30 = "10*-3(%)"
	31 = "10*12/L"
	32 = "10*3(rbc)"
	33 = "10*3/L"
	34 = "10*3/mL"
	35	10*6/L
	36	10*6/mL
	37	10*9/L
	38	10*9/mL
	39	degC
	40	cm(H2O)
	41	d
	42	dB
	43	dBA
	44	deg
	45	feet
	46	fg	
	47	fL
	48	fmol
	49	g
	50	g(creat)
	51	g(hgb)
	52	g(tot_nit)
	53	g(tot_prot)
	54	g(wet_tis)
	55	g/d
	56	g/dL
	57	g/L	
	58	h
	59	iu
	60	iu/d
	61	iu/L
	62	iu/mL
	63	kat
	64	kat/kg
	65	kat/L
	66	Kgs
	67	/kg(body wt)
	68	kg/L
	69	kpal
	70	ku/L
	71	L/min
	72	lm
	73	lm/m2
	74	m
	75	m/s
	76	m/s2
	77	m2
	78	meq
	79	meq/L
	80	mg
	81	mg/d
	82	mg/dL
	83	mg/L
	84	mg/m3
	85	mg/min
	86	mg/mmol
	87	min
	88	mu/mL
	89	mL
	90	mL/min
	91	mL/s
	92	mm
	93	mm(Hg)
	94	mm/h
	95	mmol/d
	96	mmol/L
	97	mmol/mol
	98	mol/kg
	99	mol/L
	100	mol/s
	101	month
	102	mosmol/kg
	103	mosmol/L
	104	ng
	105	ng/L
	106	ng/mL	
	107	nkat
	108	nm
	109	nmol/d
	110	nmol/L
	111	nmol/s
	112	ns
	113	Pa
	114	pg
	115	pg/L
	116	pg/mL
	117	pkat
	118	pm
	119	pmol
	120	pmol/L
	121	ps
	122	rad
	123	rem
	124	s
	125	stone
	126	u
	127	u/L
	128	ueq
	129	ug
	130	ug/d
	131	ug/dL
	132	ug/g
	133	ug/L
	134	ug/min
	135	uiu
	136	ukat
	137	um
	138	umol
	139	umol/d
	140	umol/g(creat)
	141	umol/h/g(Hb)
	142	umol/L
	143	umol/min
	144	umol/mL/h
	145	umol/mmol
	146	us
	147	week
	148	year
	149	MicroU/L
	150	cms
	151	ratio
	152	second
	153	10*9
	154	L	
	155	10*6
	156	mmol
	157	10*3
	158	mPas
	159	cP
	160	mu/L
	161	1/1
	162	/100WBC
	164	/uL
	165	iu/dL
	166	kg
	167	kiu/L
	168	kPa
	169	mg/mmol(creat)
	170	miu/L
	171	mmol/mmol(creat)
	172	mPa.s
	173	nmol/L/h
	174	nmol/L/hr
	175	u/dL
	176	u/g
	177	u/g(Hb)
	178	u/mL
	179	ug/mL
	180	umol/mol
	181	umol/mol(Hb)
	182	uu/mL
	183	10*-2
	184	10*-3
	185	L/L
	186	kua/L
	187	mmol/mmol
	188	nmol/g
	189	nmol/h/mLRBC
	190	nmol/mL/min
	191	nmol/mmol
	192	pmol/h/mg(Hb)
	193	uu/spec
	194	g/mol
	195	titre
	196	mg/12hrs
	197	SD
	198	/cu.mm
	199	GPL/ml
	200	MPL/ml
	201	ugFEU/ml
	202	mOsm/kg
	203	GPL U/ml
	204	GPM U/ml;

  value TQU
    0	Data Not Entered
1	<
2	<=
3	=
4	>
5	>=
6	~
7	Very High
8	High
9	Normal
10	Low
11	Very Low
12	Abnormal
13	Potential Abnormal
14	Outside ref range
15	Nil
16	Trace
17	+
18	++
19	+++
20	Not examined
21	Positive
22	Negative
23	Absent
24	Present
25	Normal
26	Abnormal
27	Potential Abnormal
28	
29	Negative
30	Positive
31	Positive
32	Negative
33	Unknown
34	Not examined
35	Above high reference limit
36	Below low reference limit
37	Outside reference range
38	Potentially abnormal
39	Normal
40	Low
41	High
42	Significantly Low
43	Significantly High
44	Abnormal
45	Very Abnormal
46	Not Applicable;

	value POP
	0	Data Not Entered
1	Age based
2	Age and race based
3	Age and sex based
4	Gestational age based
5	Generic normal range
6	Other (e.g. specific disease based)
7	Race based
8	Race and sex based
9	Sex based

	*/

****************************************************************************************************;
*/ macro to estimate cholesterol ldl hdl */;
%macro cholest (iindata, datalab, timemeas1=, timemeas2=);

%macro convert_tc;
  converted_value=.;
  if qualifier not in(8,41) and units in(37,49,80,82,83) 
     then converted_value=value * 10 * (1/386.598);
    else if (qualifier in(8,41) and value > 25) and units in(37,49,80,82,83)
     then converted_value = value *10 * (1/386.598);
  oldvalue = value;
  oldunits = units;
  if converted_value NE . then do;
    convertflag=1;
    value = converted_value;
    units = 96;
  end;
  if oldvalue < 25 and convertflag=1 then value=oldvalue;

  if 1.877 <= value <=13.609;
  if units in(1,151) then delete;
  if units NE 96 then units=96;
%mend;
%macro convert_hdl;
  converted_value=.;
  if units in(80,83) 
     then converted_value=value * 0.0259;
    else if value > 10 and units =82
     then converted_value = value * 0.0259;
  oldvalue = value;
  oldunits = units;
  if converted_value NE . then do;
    convertflag=1;
    value = converted_value;
    units = 96;
  end;

  if 0.391 <= value <= 4.68;
  if units in(1,138,151) then delete;
%mend;
/* Appears unit = 96, mmol/L is the preferred unit of measurement */

%macro convert (selectlabb, outlabb, title=);
data &outlabb (keep=patid eventdate medcode operator value units qualifier rangefrom rangeto basis);
  set &datalab;
  &selectlabb   and data2 > 0 and eventdate NE . then output;
  rename data2=value;
  rename data1=operator data3=units data4=qualifier data5=rangefrom data6=rangeto data7=basis;
run;
proc univariate data=&outlabb;
  title "&title";
  var value rangefrom rangeto;
run;
proc freq data=&outlabb;
  tables operator units qualifier basis 
         qualifier*units / list missing;
run;

*/ conversion */;
data &outlabb;
  set &outlabb;

  %convert_&outlabb;

  value&outlabb=value;
run; 

proc freq data=&outlabb;
  tables convertflag
         units
         qualifier * units / missing list;
run;

proc univariate data=&outlabb;
  var value&outlabb;
run;

*/ lowest value at given date */;
proc sort data=&outlabb;
  by patid eventdate value&outlabb;
run;
data &outlabb;
  set &outlabb;
  by patid eventdate value&outlabb;
  if first.eventdate then output;
  keep patid eventdate value&outlabb;
run;
%mend;

/* Basically HDL, Cholesterol, and HDL/Cholesterol are all done seperately, LDL is ignored complete;y */


*/ step 1a - creation of file hdl with converted values HDL */;
%convert(if enttype=175, hdl, title=HDL);

*/ creation of lab in timeperiod */;
%risk_creat2 (&iindata, index_date_A, hdl, HDL_A, valuehdl, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);



*/ step 1b - creation of file tc with converted values cholesterol */;
%convert(if enttype=163, tc, title=total cholesterol);

*/ creation of lab in timeperiod */;
%risk_creat2 (&iindata, index_date_A, tc, Cholesterol_A, valuetc, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);



*/ creation ratio of hdl and cholesterol */;
data ratiodata;
  merge hdl (in=ina) tc (in=inb);
  by patid eventdate;
  if ina and inb then do;
    ratioval=valuetc/valuehdl;
    output;
  end;
run;
proc univariate;
  title 'ratio choelsterol to hdl';
  var ratioval;
run;

*/ creation of lab in timeperiod */;
%risk_creat2 (&iindata, index_date_A, ratiodata, Cholesterol_HDL_Ratio_A, ratioval, and eventdate <=index_date_A+&timemeas1 and eventdate >=index_date_A-&timemeas2);


proc datasets library=work nolist;
  delete hdl tc ratiodata;
run;

%mend;


****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;
****************************************************************************************************;

%macro tyt;

libname ty "\\Mhramktd10734\f\Projects\qrisk\data";
data ttest_cholesterol;
  set ty.test_cholesterol (obs=10000);
run;


data tpat_index;
  set q.pat_index (obs=1000);
run;
%mend;



%macro sel1_cholest;
*/ TEST data */;
 tchol=.;  thdl=.;  tldl=.;  tldlhdl=.;
 if enttype in (163) or put(medcode, code16_medcode.)="code16" then do;  tchol=1;  output;  end;  */ cholesterol*/;  
 if enttype in (206,177) or put(medcode, code17_medcode.)="code17" then do;  tldl=1;  output;  end;    */ ldl*/; 
 if enttype in (175) or put(medcode, code18_medcode.)="code18" then do;  thdl=1;  output;  end;  */ hdl*/; 
 if enttype in (338) or put(medcode, code19_medcode.)="code19" then do;  tldlhdl=1;  output;  end;  */ ldl hdl ratio*/;  
%mend;

*/ cholesterol-ldl-hdl */;
%testadd_extract1 (ttest_cholesterol, rctdata.sel1test_, sel1_cholest);


proc freq;
  title 'input raw lab data';
  table enttype;
run;
 


*/ timemeas is time-period for measuring value - selected on index or 5 years before*/;
%cholest (p1.allpersA, ttest_cholesterol, timemeas1=0, timemeas2=1827);

proc contents data=p2.varCholesterol_hdl_ratio_A;run;
proc print data=p2.varCholesterol_hdl_ratio_A(obs=10);run;


/* Test to see if any cholesterol values in the additional clinical data */
%testadd_extract1 (taddclin_cholesterol, rctdata.sel1addclin_, sel1_cholest);
