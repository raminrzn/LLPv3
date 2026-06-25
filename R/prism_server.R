# ---------------------------------------------------------------------------
# ModelsCloud / PRISM server gateway.
#
# The ModelsCloud (pexa) platform does not call model_run() directly: it calls
# <package>::gateway(func = "model_run", model_input = ...) as the single entry
# point, which dispatches to the package's model functions. This mirrors the
# canonical RESP bridge contract (resplab/bridgePrism, resplab/fev1Prism) and
# is what makes the deployed model resolvable by the platform's executor.
# ---------------------------------------------------------------------------

#' ModelsCloud / PRISM gateway (server entry point)
#'
#' Generic dispatcher invoked by the ModelsCloud platform. It reads `func` from
#' the incoming request, strips the platform-only fields (`func`, `api_key`,
#' `session_id`), and calls the named package function with the remaining
#' arguments — e.g. `gateway(func = "model_run", model_input = ...)` runs
#' [model_run()]. If `func` is omitted it defaults to `"model_run"`. The result
#' is serialised to JSON, matching the established gateway contract.
#'
#' @param ... Request fields supplied by the platform. Recognised control
#'   fields are `func` (the function to run), `api_key`, and `session_id`; all
#'   other fields (typically `model_input`) are passed to the selected function.
#' @return A JSON string produced by [jsonlite::toJSON()] containing the
#'   selected function's result.
#' @seealso [model_run()], [get_default_input()], [get_sample_input()]
#' @export
gateway <- function(...) {
  arguments <- list(...)
  func <- arguments$func
  if (is.null(func)) func <- "model_run"

  arguments$func <- NULL
  arguments$api_key <- NULL
  arguments$session_id <- NULL

  out <- if (length(arguments) == 0) do.call(func, list()) else do.call(func, arguments)
  jsonlite::toJSON(out, dataframe = "rows", na = "null", digits = NA)
}

#' Run the model via the PRISM-style entry point
#'
#' Thin alias for [model_run()], kept for compatibility with platform/clients
#' that dispatch to `prism_model_run`.
#'
#' @param model_input See [model_run()].
#' @return See [model_run()].
#' @seealso [model_run()]
#' @export
prism_model_run <- function(model_input = NULL) {
  model_run(model_input)
}

# Lightweight availability check some clients call through the gateway. Not
# exported: the gateway resolves it within the package namespace via do.call().
connect_to_model <- function(api_key = "") {
  list(error_code = 0, session_id = "", version = "", description = "ModelsCloud enabled")
}
