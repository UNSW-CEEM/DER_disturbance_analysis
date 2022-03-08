# Execute validation scripts to compare two branches
# Run from DER_disturbance_analysis base directory e.g.:
# % ./validation/full_validation.sh branch_to_validate master

# exit if no args provided
if [ $# -eq 0 ]
then
    echo "No arguments provided - test branch required, ref branch optional"
    exit 1
fi
# set target name
TEST_BRANCH="$1"
if [ -z "$2" ]
then
    echo "No ref branch provided, defaulting to master"
    REF_BRANCH="master"
else
    REF_BRANCH="$2"
fi

echo "Validating branch $TEST_BRANCH against $REF_BRANCH"

REF_PREFIX="ref"
TEST_PREFIX="test"

# Build databases and run analysis for reference branch
git checkout $REF_BRANCH
if [ $? -eq 0 ]
then
    Rscript validation/build_validation_databases.R $REF_PREFIX
    Rscript validation/run_validation_analysis.R $REF_PREFIX
else
    echo "Checkout failed"
    exit 1
fi

# Build databases and run analysis for test branch
git checkout $TEST_BRANCH
if [ $? -eq 0 ]
then
    Rscript validation/build_validation_databases.R $TEST_PREFIX
    Rscript validation/run_validation_analysis.R $TEST_PREFIX
else
    echo "Checkout failed"
    exit 1
fi

# Compare results
Rscript validation/validate_results.R $REF_PREFIX $TEST_PREFIX
