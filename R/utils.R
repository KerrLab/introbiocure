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

