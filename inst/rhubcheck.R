if (!require(rhub, quietly = TRUE)) {
        install.packages("rhub")
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
        stop("Incorrect number of args, needs 2: platform (string), token (string)")
}

platform <- args[[1L]]
token <- args[[2L]]
if (!is.element(platform, rhub::platforms()[[1L]])) {
        stop(paste(platform, "not in rhub::platforms()[[1L]]"))
}
rhub::validate_email(email = substr(utils::maintainer(pkg = "scoringTools"), regexec("<", utils::maintainer(pkg = "scoringTools"))[[1]][1] + 1, nchar(utils::maintainer(pkg = "scoringTools")) - 1),
                     token = token)
cr <- rhub::check(platform = platform, show_status = TRUE)
statuses <- cr[[".__enclos_env__"]][["private"]][["status_"]]

res <- do.call(rbind, lapply(statuses, function(thisStatus) {
        data.frame(
                plaform  = thisStatus[["platform"]][["name"]],
                errors   = length(thisStatus[["result"]][["errors"]]),
                warnings = length(thisStatus[["result"]][["warnings"]]),
                notes    = length(thisStatus[["result"]][["notes"]]),
                stringsAsFactors = FALSE
        )
}))
print(res)

if (any(colSums(res[2L:3L]) > 0)) {
        stop("Some checks with errors, warnings or notes.")
}

if (any(colSums(res[4L:4L]) > 0)) {
        warning("Some checks with notes.")
}
