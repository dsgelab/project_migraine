
# execute this script in order to generate the study population 

echo "start triptane users filtering"
time0=$(date +%s)
awk -F ',' 'NR == 1 || (($6 == "PURCH") && (index($9, "N02CC") != 0)) {print $0}' /data/processed_data/detailed_longitudinal/R10/detailed_longitudinal_2023-09-11.csv > /data/projects/project_mferro/project_migraine/data/migraine_diagnosis.csv
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo "start migraine diagnosis filtering"
time0=$(date +%s)
awk -F ',' 'NR == 1 || ((index($9, "G43") != 0) || (index($10, "G43") != 0)) {print $0}' /data/processed_data/detailed_longitudinal/R10/detailed_longitudinal_2023-09-11.csv > /data/projects/project_mferro/project_migraine/data/triptan_users.csv
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo "start study population extraction"
time0=$(date +%s)
/opt/R-4.1.1/bin/Rscript scripts/define_study_population.R > logs/log_study_population.txt
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo "start case-control definition"
time0=$(date +%s)
/opt/R-4.1.1/bin/Rscript scripts/define_case_controls.R > logs/log_case_definition.txt
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo 'study cohort have been created'
