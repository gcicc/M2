# test stub for developing ETL scripts

source(paste0(repo_clone,"/Shared-Content/preamble.R"))

for (study in studies) {
  input_dir <- path(input_base_dir,study)
  if (study=="Milan") {
    demog_df <- demog_milan_etl(debug=output_base_dir)
    histology_df <- histology_milan_etl(debug=output_base_dir)
    symptoms_df <- symptoms_milan_etl(debug=output_base_dir)
    vce_df <- vce_milan_etl(debug=output_base_dir)
  } else if (study=="Phase0") {
    demog_df <- demog_phase0_etl(debug=output_base_dir)
    histology_df <- histology_phase0_etl(debug=output_base_dir)
    symptoms_df <- symptoms_phase0_etl(debug=output_base_dir)
    vce_df <- vce_phase0_etl(debug=output_base_dir)
  } else if (study=="TAK-062-2001") {
    demog_df <- demog_062_2001_etl(debug=output_base_dir)
    histology_df <- histology_062_2001_etl(debug=output_base_dir)
    symptoms_df <- symptoms_062_2001_etl(debug=output_base_dir)
    vce_df <- vce_062_2001_etl(debug=output_base_dir)
  } else if (study=="Mock-Study") {
    demog_df <- demog_mock_etl(debug=output_base_dir)
    histology_df <- histology_mock_etl(debug=output_base_dir)
    symptoms_df <- symptoms_mock_etl(debug=output_base_dir)
    vce_df <- vce_mock_etl(debug=output_base_dir)
  } else {
    print("Logic error: unsupported study")
  }
}
