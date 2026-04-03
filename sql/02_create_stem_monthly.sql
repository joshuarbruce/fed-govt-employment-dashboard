-- OPM Dashboard: STEM/Scientists Monthly Summary
-- Powers: Scientists & STEM tab (who, which agencies, where, trends)
-- Only includes rows where stem_occupation_type is populated
-- Re-run monthly after new data is uploaded to opm_data.employment

CREATE OR REPLACE TABLE `mapping-patents.opm_data.stem_monthly` AS
SELECT
  CAST(file_year AS INT64)          AS file_year,
  CAST(file_month AS INT64)         AS file_month,
  agency,
  agency_subelement,
  stem_occupation_type,
  duty_station_state_abbreviation,
  COUNT(*)                          AS headcount
FROM `mapping-patents.opm_data.employment`
WHERE file_type = 'employment'
  AND stem_occupation_type IS NOT NULL
  AND stem_occupation_type NOT IN ('', 'ALL OTHER OCCUPATIONS', 'UNSPECIFIED')
GROUP BY 1, 2, 3, 4, 5, 6;
