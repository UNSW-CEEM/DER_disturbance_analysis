# Tool result validation process

In order to ensure that results are not being modified by changes to the tool - or that expected changes ARE being made - we need to validate/benchmark the results.

Validation datasets will be made available on cloudstor once they are finalised.

## Running validation

1. Run analysis on sample data with reference version of tool
    1. Checkout reference version of tool (currently latest master branch)
    2. Build database for the events in the `DER_disturbance_analysis/validation/data` directory using `build_validation_databases.R`
    3. Run tool using metadata from the event directory as config json
    4. Batch save results in the same directory as the data, entering `ref` as the file name
    5. (optional) - Instead of steps 3 and 4, use `run_validation_analysis.R` to generate the files.
2. Run analysis on sample data with version of tool to be tested
    1. Checkout target branch
    2. Build database for the events in the `DER_disturbance_analysis/validation/data` directory using `build_validation_databases.R`
    3. Run tool using metadata from the event directory as config json. __IMPORTANT__ - make sure any changes in configuration in the target branch (new fields/values etc.) are reflected in the test meta data.
    4. Batch save results in the same directory as the data, entering `test` as the file name
    5. (optional) - Instead of steps 3 and 4, use `run_validation_analysis.R` to generate the files.
3. Compare reference and test results using `validate_results.R`
    1. Identify if results match
    2. Check any discrepencies against expected impact of test version of tool

### Automated validation

Run `full_validation.sh` in your terminal program of choice from the tool base directory.
Pass positional arguments of test and reference branch e.g.
```bash
./full_validation.sh branch_to_validate master
```

## Common issues / FAQ

-  `Error: Result must have length 36, not 0` or `Error in read.table(...) first five rows are empty: giving up` when running validation
    - This is likely caused by new or modified tool settings. Ensure that any new settings are reflected in the `test_meta_data.json` file for each event. Once this validation is complete ensure that `ref_meta_data.json` files are updated and uploaded to cloudstor for future use.


## Choosing validation datasets

A representative set of validation data is required. In order to capture this we are currently uising the data from 2021-05-25 from QLD, 2020-01-31 from SA, and one event using Tesla data (2021-03-12)

For each event the following process will need to be followed:

1. Build database from raw data for event
2. Run normal tool analysis with appropriate settings for that event. Ensure that frequency data is included if necessary, and all category filters other than "raw" are checked
3. Batch save the results under a memorable name
4. Identify appropriate sample circuits based on the results in the circuit summary. Currently the following columns are being used to identify unique circuits:
    * `response_category`, `reconnection_compliance_status`, `ufls_status`, `compliance_status`, `Standard_Version`
5. Using the site ID of the sample results filter the raw data to only use included circuits (including site_details, circuit_details and raw data files). 
6. Save this filtered raw data as a new set of files under `DER_disturbance_analysis/validation/data` in a sub-directory with a meaningful name representing the event.
7. Copy any required supporting files - the metadata file from the sample selection should be included, as should any necessary network frequency data.
