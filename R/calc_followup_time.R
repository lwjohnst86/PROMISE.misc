#' Mean and SD followup time for those who attended the 6 year visit.
#'
#' @param data Project data
#'
#' @export
calc_followup_time <- function(data = project_data) {
    data %>%
        dplyr::arrange(SID, VN) %>%
        dplyr::group_by(SID) %>%
        dplyr::slice(n()) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(MeanFollowup = aide::ave_sd(YearsFromBaseline))
}
