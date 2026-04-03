-- OPM Dashboard: Geographic Monthly Summary
-- Powers: Geography tab (state choropleth map, concentration over time)
-- US-only; excludes rows with no state
-- Re-run monthly after new data is uploaded to opm_data.employment

CREATE OR REPLACE TABLE `mapping-patents.opm_data.geo_monthly` AS
SELECT
  CAST(file_year AS INT64)          AS file_year,
  CAST(file_month AS INT64)         AS file_month,
  duty_station_state_abbreviation,
  agency,
  stem_occupation_type,
  COUNT(*)                          AS headcount
FROM `mapping-patents.opm_data.employment`
WHERE file_type = 'employment'
  AND duty_station_country_code = 'US'
  AND duty_station_state_abbreviation IS NOT NULL
  AND duty_station_state_abbreviation != ''
GROUP BY 1, 2, 3, 4, 5;
