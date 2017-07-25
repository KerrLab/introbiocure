#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom googlesheets gs_auth
#' @export
googlesheets::gs_auth

col_types_180 <- readr::cols(
    Year = readr::col_integer(),
    Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
    Section = readr::col_character(),
    Group = readr::col_integer(),
    Anc.or.Des = readr::col_factor(levels = c("A", "D")),
    Drug.at.Isolation = readr::col_character(), # TODO: make a factor
    Fitness = readr::col_number(),
    Drug = readr::col_character(), # TODO: make a factor?
    MIC = readr::col_number()
)

col_types_200 <- readr::cols(
    Year = readr::col_integer(),
    Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
    Section = readr::col_character(),
    Group = readr::col_integer(),
    Anc.or.Des = readr::col_factor(levels = c("A", "D")),
    Cluster = readr::col_factor(levels = c("I", "II")),
    Direction = readr::col_factor(levels = c("F", "R")),
    Sequence = readr::col_character(),
    Base.Mutations = readr::col_character(),
    AA.Mutations = readr::col_character()
)

# Column types for both courses
col_types_both <- readr::cols(
    Year = readr::col_integer(),
    Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
    Section = readr::col_character(),
    Group = readr::col_integer(),
    Anc.or.Des = readr::col_factor(levels = c("A", "D"))
)
