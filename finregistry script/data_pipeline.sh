
# define file paths

detailed_longitudinal=/data/processed_data/detailed_longitudinal/R10/detailed_longitudinal_DF10_2022-11-11.csv
endpointer=/data/processed_data/endpointer/R10/longitudinal_endpoints_no_omits_DF10_2022_09_29.csv

cluster_migraine_events_file=/data/projects/project_mferro/project_migraine/data/endpoint_migraine.csv
triptan_users_file=/data/projects/project_mferro/project_migraine/data/triptan_users.csv
nsaid_users_file=/data/projects/project_mferro/project_migraine/data/nsaid_users.csv

# execute this script in order to generate the study population 

echo "start triptane users filtering"
time0=$(date +%s)
awk -F ',' 'NR == 1 || (($2 == "PURCH") && (index($6, "N02CC") != 0)) {print $0}' $detailed_longitudinal > $triptan_users_file
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo "start cluster headache endpoint filtering"
time0=$(date +%s)
awk -F ',' 'NR == 1 || ($6 == "G6_CLUSTHEADACHE_WIDE") {print $0}' $endpointer > $cluster_migraine_events_file
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo "start NSAID users filtering"
time0=$(date +%s)
awk -F ',' 'NR == 1 || (($2 == "PURCH") && (index($6, "M01A") != 0)) {print $0}' $detailed_longitudinal > $nsaid_users_file
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

time0=$(date +%s)
/opt/R-4.1.1/bin/Rscript scripts/make_endpoint_file.R > logs/log_make_endpoint.txt
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo 'endpoint file have been created'