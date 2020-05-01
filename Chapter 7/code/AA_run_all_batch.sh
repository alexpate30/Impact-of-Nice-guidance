LOAD R

SET WORKING DIRECTORY TO WHERE CODE IS STORED
nohup Rscript 'p1.1 baseline_table.R' > 'p1.1 baseline_table.out'
nohup Rscript 'p1.2 run_imputation_h1.R' > 'p1.2 run_imputation_h1.out'
nohup Rscript 'p1.3 run_imputation_h2.R' > 'p1.3 run_imputation_h2.out'
nohup Rscript 'p1.4 load_imputed_datasets.R' > 'p1.4 load_imputed_datasets.out'
nohup Rscript 'p2.1 qrisk3_calc_functions.R' > 'p2.1 qrisk3_calc_functions.out'
nohup Rscript 'p2.2 calculate_EHR_derived_qrisk3_scores.R' > 'p2.2 calculate_EHR_derived_qrisk3_scores.out'
nohup Rscript 'p2.3 run_analysis_incidence_over_time.R' > 'p2.3 run_analysis_incidence_over_time.out'
nohup Rscript 'p2.4 run_analysis_risk_by_year.R' > 'p2.4 run_analysis_risk_by_year.out'