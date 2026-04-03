-- OPM Dashboard: Agency Monthly Summary
-- Powers: Overview tab, By Agency tab (headcount trends, avg pay)
-- Re-run monthly after new data is uploaded to opm_data.employment

CREATE OR REPLACE TABLE `mapping-patents.opm_data.agency_monthly` AS
SELECT
  CAST(file_year AS INT64)                                    AS file_year,
  CAST(file_month AS INT64)                                   AS file_month,
  agency,
  agency_subelement,
  occupational_group,
  work_schedule,
  COUNT(*)                                                    AS headcount,
  AVG(SAFE_CAST(annualized_adjusted_basic_pay AS FLOAT64))   AS avg_pay
FROM `mapping-patents.opm_data.employment`
WHERE file_type = 'employment'
GROUP BY 1, 2, 3, 4, 5, 6;
