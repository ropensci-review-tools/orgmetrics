at_base_id <- "appegDLtrNVgkWROB"

airtable_update_schema <- function (base_id = at_base_id) {

    metrics_metadata <- load_model_json_data ()$metrics

    schema <- airtabler::air_get_schema (at_base_id)

    table_num <- which (schema$tables$name == "CHAOSS Metrics")
    table_id <- schema$tables$id [table_num]
    fields <- schema$tables$fields [[table_num]]

    # ------ Update airtable field names -----
    # airtable names can (at least initially) be either metadata "name""" or
    # metadata "airtable_name".
    index <- cbind (
        match (fields$name, metrics_metadata$name),
        match (fields$name, metrics_metadata$airtable_name)
    )
    index <- apply (index, 1, function (i) {
        ifelse (all (is.na (i)), NA_integer_, i [which (!is.na (i))] [1])
    })
    index <- cbind (seq_along (index), index)
    index <- index [which (!is.na (index [, 2])), ]
    # [, 1] -> fields; [, 2] -> mettrics_metadata

    field_names_orig <- field_names_updated <- fields$name
    field_names_updated [index [, 1]] <- metrics_metadata$airtable_name [index [, 2]]

    if (any (duplicated (field_names_updated))) {
        dup <- field_names_updated [which (duplicated (field_names_updated))]
        cli::cli_abort ("Duplicated name of original field in airtable data: '{dup}'")
    }

    index <- which (field_names_updated != field_names_orig)
    updates <- lapply (index, function (i) {
        f_i <- match (field_names_orig [i], fields$name)
        field_id <- fields$id [f_i]
        airtabler::air_update_field (
            base = at_base_id,
            table_id = table_id,
            field_id = field_id,
            name = field_names_updated [i]
        )
    })

    # ------ Update airtable field descriptions -----
}
