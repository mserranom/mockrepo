
soep_make_input <- function(df, dbname, filename, path) {
    if (!file.exists(path)) {
        # handle non-existing directory error.
        stop("The specified directory does not exist.")
    } else {
        # file header for input from Stata
        header <- paste0("qui {\ninput ", paste(colnames(df), collapse = " "))
        # open file stream to write the header
        file <- file(file.path(path, filename), "w", encoding = "UTF-8")
        # write header line
        writeLines(header, file)
        # close stream to enable append
        close(file)

        # print the whole data.frame table asis appending to file
        data.table::fwrite(
            df,
            file      = file.path(path, filename),
            append    = TRUE,
            quote     = TRUE,
            sep       = " ",
            na        = ".",
            row.names = FALSE,
            col.names = FALSE
        )

        # re-open stream
        file <- file(file.path(path, filename), "a", encoding = "UTF-8")

        footer <- glue::glue(
            "end\n\n",
            'save \"${mydata}/{{dbname}}\", replace\n\n',
            'noi di \"Data successfully loaded in the server.\"\n\n',
            "} // qui\n\n", "***", "\n\n",
            .open = "{{", .close = "}}"
        )

        # write closing line for quietly
        writeLines(footer, file)

        close(file)
        unlink(file)
    }
}

soep_deliver_job <- function(path, sender, project = NULL, jobname = NULL) {

    # SOEPRemote credentials
    # TODO move in a key ring and retrieve from there
    username <- "XXXXXXXXX"
    password <- "XXXXXXXXX"

    # header to validate code input
    header <- glue::glue(
        "** user = {username}\n",
        "** password = {password}\n",
        "** package = STATA\n",
        "** project = GSOEP\n\n"
    )
    # read file with code content for SOEPRemote as plain text
    task <- readr::read_file(path)

    time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

    # create subject label, check if there is a name defined for the project
    if (is.null(project)) {
        subject <- glue::glue("SOEPRemote Job {time}")
    } else {
        subject <- glue::glue(
            "{project}: SOEPRemote Job {time}"
        )
    }

    # check whether there is a name for the task to add
    if (!is.null(jobname)) {
        subject <- glue::glue("{subject} ({jobname})")
    }

    # create envelope object
    job <- emayili::envelope()
    # populate object with headers and body
    job <- job %>%
        emayili::from(emayili::as.address(sender, validate = TRUE)) %>%
        emayili::to("xxxxxxxxx") %>%
        emayili::subject(subject) %>%
        emayili::text(paste0(header, task))

    # create SMTP object for using the gmail server, we use an app password
    smtp <- emayili::server(
        host = "smtp.hello.com",
        port = 587,
        username = "xxxxxxxxxx",
        password = "xxxxxxxx",
        reuse = FALSE
    )

    # send email, leave verbose for debugging
    smtp(job, verbose = TRUE)
}
