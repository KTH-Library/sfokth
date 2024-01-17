#' Get external collaboration indicators per publication
#'
#' @param con connection to Bibmet
#' @param uts list of UT numbers
#' @param orgids list of Unified_org_id:s for organisations in SFO
#' @import dplyr
#' @returns tibble
#' @export
collaboration_ext <- function(con, uts, orgids) {

  con |>
    tbl("BestResAddr") |>
    filter(UT %in% !!uts) |>
    collect() |>
    group_by(UT) |>
    summarise(affs_sweden = n_distinct(Name_eng[toupper(Country_name) == "SWEDEN"]),
              aff_inter = any(toupper(Country_name) != "SWEDEN"),
              affs_sfo = n_distinct(Unified_org_id[Unified_org_id %in% !!orgids]),
              aff_industry = any(Org_type_code == "CO"),
              aff_other = any(Org_type_code %in% c("IN", "HL", "OP", "OT", "UH", "OT")),
              swe_collab = affs_sweden > 1,
              sfo_collab = affs_sfo > 1) |>
    mutate(across(c("aff_inter", "aff_industry", "aff_other", "swe_collab", "sfo_collab"), \(x) recodeNA(x, FALSE)))
}

#' Get internal KTH collaboration indicators per publication
#'
#' @param con connection to Bibstat
#' @param uts list of UTs
#' @import dplyr
#' @returns tibble
#' @export
collaboration_kth <- function(con, uts) {

  diva <- con |> tbl("Diva_rev")
  affs <- con |> tbl("Diva_Affiliations")

  diva |>
    filter(ISI %in% !!uts) |>
    inner_join(affs, by = "PID") |>
    collect() |>
    rename(UT = ISI) |>
    group_by(UT) |>
    summarise(n_schools = n_distinct(Diva_school_id_new),
              n_deps = n_distinct(Diva_dep_id_new),
              sch_collab = n_schools > 1,
              dep_collab = n_deps > 1)|>
    mutate(across(c("sch_collab", "dep_collab"), \(x) recodeNA(x, FALSE)))
}

#' Performance indicators, national collaborations
#'
#' @param data collaboration status and indicators per publication and site
#' @import dplyr
#' @export
collaboration_perf_natl <- function(data) {
  data |>
    group_by(site) |>
    summarise(p = n(),
              natl = sum(swe_collab, na.rm = TRUE),
              natl_share = mean(swe_collab, na.rm = TRUE),
              jcf = mean(Jcf[include_jcf & swe_collab], na.rm = TRUE),
              top20 = mean(Top20[include_jcf & swe_collab], na.rm = TRUE),
              cf = mean(Cf_scxwo[include_cf & swe_collab], na.rm = TRUE),
              top10 = mean(Top10_scxwo[include_cf & swe_collab], na.rm = TRUE),
              sra = sum(swe_collab & sfo_collab, na.rm = TRUE),
              sra_share = mean(sfo_collab[swe_collab], na.rm = TRUE),
              sra_jcf = mean(Jcf[include_jcf & swe_collab & sfo_collab], na.rm = TRUE),
              sra_cf = mean(Cf_scxwo[include_cf & swe_collab & sfo_collab], na.rm = TRUE),
              dep = sum(swe_collab & dep_collab, na.rm = TRUE),
              dep_share = mean(dep_collab[swe_collab], na.rm = TRUE),
              dep_jcf = mean(Jcf[include_jcf & swe_collab & dep_collab], na.rm = TRUE),
              dep_cf = mean(Cf_scxwo[include_cf & swe_collab& dep_collab], na.rm = TRUE),
              .groups = "drop")
}

#' Performance indicators, international collaborations
#'
#' @param data collaboration status and indicators per publication and site
#' @import dplyr
#' @export
collaboration_perf_inter <- function(data) {
  data |>
    group_by(site) |>
    summarise(p = n(),
              inter = sum(aff_inter, na.rm = TRUE),
              inter_share = mean(aff_inter, na.rm = TRUE),
              jcf = mean(Jcf[include_jcf & aff_inter], na.rm = TRUE),
              top20 = mean(Top20[include_jcf & aff_inter], na.rm = TRUE),
              cf = mean(Cf_scxwo[include_cf & aff_inter], na.rm = TRUE),
              top10 = mean(Top10_scxwo[include_cf & aff_inter], na.rm = TRUE),
              sra = sum(aff_inter & sfo_collab, na.rm = TRUE),
              sra_share = mean(sfo_collab[aff_inter], na.rm = TRUE),
              sra_jcf = mean(Jcf[include_jcf & aff_inter & sfo_collab], na.rm = TRUE),
              sra_cf = mean(Cf_scxwo[include_cf & aff_inter & sfo_collab], na.rm = TRUE),
              dep = sum(aff_inter & dep_collab, na.rm = TRUE),
              dep_share = mean(dep_collab[aff_inter], na.rm = TRUE),
              dep_jcf = mean(Jcf[include_jcf & aff_inter & dep_collab], na.rm = TRUE),
              dep_cf = mean(Cf_scxwo[include_cf & aff_inter& dep_collab], na.rm = TRUE),
              .groups = "drop")
}

#' Performance indicators, other collaborations
#'
#' @param data collaboration status and indicators per publication and site
#' @import dplyr
#' @export
collaboration_perf_other <- function(data) {
  data |>
    group_by(site) |>
    summarise(p = n(),
              industry = sum(aff_industry, na.rm = TRUE),
              inter_share = mean(aff_industry, na.rm = TRUE),
              jcf = mean(Jcf[include_jcf & aff_industry], na.rm = TRUE),
              top20 = mean(Top20[include_jcf & aff_industry], na.rm = TRUE),
              cf = mean(Cf_scxwo[include_cf & aff_industry], na.rm = TRUE),
              top10 = mean(Top10_scxwo[include_cf & aff_industry], na.rm = TRUE),
              other = sum(aff_other, na.rm = TRUE),
              other_share = mean(aff_other, na.rm = TRUE),
              .groups = "drop")
}

#' Get fundings per publication
#'
#' @param con connection to Bibmet
#' @param uts list of UT numbers
#' @import dplyr
#' @returns tibble
#' @export
fundings_ut <- function(con, uts) {

  doc <- con |>
    tbl("Document") |>
    filter(UT %in% !!uts)

  fundings <- con |>
    tbl("Funding_grant")

  doc |>
    inner_join(fundings, by = "Doc_id") |>
    filter(is.na(Grant_agency_pref)) |>
    select(UT, No_grants, Grant_agency) |>
    distinct() |>
    collect()
}
