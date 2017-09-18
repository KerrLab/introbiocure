#' Merge Section Data with BIO 200 into Master Sheet
#'
#' @param master URL of the master spreadsheet
#' @param d Data frame containing section data (see \code{\link{get_section_data_180}})
#'
#' @return A data frame containing the updated master data
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_section_data_200(<section url>)
#' master_add_section_data_200(<master sheet url>, d)
#' }
master_add_section_data_200 <- function(master, d) {
    assertthat::assert_that(
        assertthat::is.string(master),
        is.data.frame(d),
        nrow(d) > 0
    )

    d <- d %>%
        dplyr::select(StrainID, Base.Mutations, AA.Mutations, SequenceProblemIdentified)

    dMaster <- get_master_data(master = master)

    overlap <- dplyr::semi_join(d, dMaster, by = c("StrainID"))

    if (nrow(overlap) < nrow(d)) {
        # There are StrainIDs specified in d that aren't in master.
        strain_diff <- setdiff(d$StrainID, dMaster$StrainID)
        stop(
            paste(
                "Data contains strain ID(s) that aren't in master",
                paste0(strain_diff, collapse = ", "),
                sep = ": "
            )
        )
    }

    # See if there are any matching strains that already have mutation info
    overlap_master <- dplyr::semi_join(dMaster, d, by = c("StrainID"))
    if (any(!is.na(overlap_master$Base.Mutations)) || any(!is.na(overlap_master$AA.Mutations))) {
        xstrains <- sort(unique(rbind(overlap_master[!is.na(overlap_master$Base.Mutations), "StrainID"], overlap_master[!is.na(overlap_master$AA.Mutations), "StrainID"])$StrainID))
        stop(
            paste(
                "Data contains strain(s) that already have mutation info",
                paste0(xstrains, collapse = ", "),
                sep = ": "
            )
        )
    }

    dUpdated <- dplyr::inner_join(d, dMaster, by = "StrainID") %>%
        dplyr::select(
            Year = Year,
            Quarter = Quarter,
            Section = Section,
            Group = Group,
            StrainID = StrainID,
            Pro.or.Des = Pro.or.Des,
            Drug.at.Isolation = Drug.at.Isolation,
            Fitness = Fitness,
            Drug1 = Drug1,
            Drug1.MIC = Drug1.MIC,
            Drug2 = Drug2,
            Drug2.MIC = Drug2.MIC,
            Base.Mutations = Base.Mutations.x,
            AA.Mutations = AA.Mutations.x,
            ProblemIdentified = ProblemIdentified,
            SequenceProblemIdentified = SequenceProblemIdentified.x
        )
    dNonUpdated <- dplyr::anti_join(dMaster, d, by = "StrainID")

    dplyr::bind_rows(dUpdated, dNonUpdated) %>%
        dplyr::arrange(Year, Quarter, Section, Group, StrainID)

}
