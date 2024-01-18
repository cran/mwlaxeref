standardize_state_names <- function(x) {
  return(tolower(gsub(" ", "_", x)))
}

# match_state_column <- function(data) {
#   mwlaxeref_states <- data.frame(
#     mwlaxeref.state.name = standardize_state_names(state.name),
#     mwlaxeref.state.code = tolower(state.abb)
#   )
#   state_col_name <- grep("^(state)(s?)", colnames(data), value = TRUE)
#   out <-
#     data |>
#     dplyr::mutate(mwlaxeref.state = standardize_state_names(
#       !!rlang::sym(state_col_name)
#     ))
#   states_in_data <- unique(data$mwlaxeref.state)
#   if (any(states_in_data %in% mwlaxeref$state.name)) {
#     out <-
#       out |>
#       dplyr::left_join(
#         mwlaxeref_states,
#         by = c("mwlaxeref.state" = "mwlaxeref.state.name")
#       )
#   } else if (any(states_in_data %in% mwlaxeref$state.code)) {
#     out <- dplyr::mutate(out, mwlaxeref.state.code)
#   }
#   return(out)
# }
