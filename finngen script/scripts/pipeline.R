# execute this script in order to generate the study population 

echo "start triptane users filtering"
time0=$(date +%s)
zcat /finngen/library-red/finngen_R12/phenotype_1.0/data/finngen_R12_detailed_longitudinal_1.0.txt.gz  | awk -F '\t' 'NR == 1 || (($2 == "PURCH") && (index($5, "N02CC") != 0)) {print $0}' > /home/ivm/project_migraine/data/triptan_users.csv
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"

echo "start Cluster Headache endpoint filtering"
time0=$(date +%s)
zcat /finngen/library-red/finngen_R12/phenotype_1.0/data/finngen_R12_endpoint_1.0.txt.gz  | awk -F '\t' 'NR == 1 || {print $1 $2  $3  $4  $15766  $15767  $15768  $15769  $15770  $15771  $15772  $15773}' > /home/ivm/project_migraine/data/endpoint_migraine.csv
time1=$(date +%s)
execution_time=$((time1 - time0))
echo "Execution time: $execution_time seconds"


echo "start NSAID users filtering"
time0=$(date +%s)
zcat /finngen/library-red/finngen_R12/phenotype_1.0/data/finngen_R12_detailed_longitudinal_1.0.txt.gz  | awk -F '\t' 'NR == 1 || (($2 == "PURCH") && (index($5, "M01A") != 0)) {print $0}' > /home/ivm/project_migraine/data/triptan_users.csv
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