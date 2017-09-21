#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom googlesheets gs_auth
#' @export
googlesheets::gs_auth


# Column types for 180 sections
col_types_180 <- readr::cols(
    Year = readr::col_integer(),
    Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
    Section = readr::col_character(),
    Group = readr::col_integer(),
    StrainID = readr::col_character(),
    Pro.or.Des = readr::col_factor(levels = c("Progenitor", "Descendant")),
    Drug.at.Isolation = readr::col_character(), # could make a factor
    Fitness = readr::col_number(),
    Drug1 = readr::col_character(), # could make a factor?
    Drug1.MIC = readr::col_number(),
    Drug2 = readr::col_character(), # could make a factor?
    Drug2.MIC = readr::col_number(),
    ProblemIdentified = readr::col_character() # Could be converted to a logical
)

# Column types for 200 sections
col_types_200 <- readr::cols(
    #Year = readr::col_integer(),
    #Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
    #Section = readr::col_character(),
    Group = readr::col_integer(),
    StrainID = readr::col_character(),
    Pro.or.Des = readr::col_factor(levels = c("Progenitor", "Descendant")),
    Drug.at.Isolation = readr::col_character(), # could make a factor
    Fitness = readr::col_number(),
    RIF.MIC = readr::col_number(),
    Base.Mutations = readr::col_character(),
    AA.Mutations = readr::col_character(),
    SequenceProblemIdentified = readr::col_character() # could be converted to a logical
)


# Column types for master sheet
col_types_master <- readr::cols(
    Year = readr::col_integer(),
    Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
    Section = readr::col_character(),
    Group = readr::col_integer(),
    StrainID = readr::col_character(),
    Pro.or.Des = readr::col_factor(levels = c("Progenitor", "Descendant")),
    Drug.at.Isolation = readr::col_character(), # could make a factor
    Fitness = readr::col_number(),
    Drug1 = readr::col_character(), # could make a factor?
    Drug1.MIC = readr::col_number(),
    Drug2 = readr::col_character(), # could make a factor?
    Drug2.MIC = readr::col_number(),
    Base.Mutations = readr::col_character(),
    AA.Mutations = readr::col_character(),
    ProblemIdentified = readr::col_character(),
    SequenceProblemIdentified = readr::col_character()
)

# Extract a row number from a Well ID (e.g. "C7" -> 3)
well_row <- function(well) {
    well_letters <- toupper(substr(well, 1, 1))
    strtoi(sapply(well_letters, charToRaw), 16L) - 64
}

# Extract a column number from a Well ID (e.g. "C7" -> 7)
well_column <- function(well) {
    as.integer(substr(well, 2, 4))
}

# Extract section, group, and drug information from a plate name
extract_plate_info <- function(plate) {
    pattern <- "S([A-Za-z]{1,2})-G([0-9]{1,2})-([A-Za-z]{3})"
    col_names <- c("Plate", "Section", "Group", "Drug")

    matches_pattern <- stringi::stri_detect_regex(
        str = plate,
        pattern = pattern
    )

    for (ix in which(matches_pattern == FALSE)) {
        warning(
            paste(plate[ix], "is an invalid plate name. Skipping."),
            call. = FALSE
        )
    }

    m <- stringi::stri_match_first_regex(
        str = plate,
        pattern = pattern
    )
    colnames(m) <- col_names
    na.omit(tibble::as_tibble(m))
}
