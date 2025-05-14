at_base_id <- "appegDLtrNVgkWROB"

#' Update the schema data of the airtable "CHAOSS Metrics" table.
#'
#' The only schema data currently able to be updated by 'airtabler' are the
#' name and description of each field. Note also that this updates all fields
#' listed in the 'repometrics' JSON schema, even though not all of these may
#' actually be present in the data.
#' @noRd
airtable_update_schema <- function (at_base_id = at_base_id) {

    metrics_metadata <- load_model_json_data ()$metrics

    schema <- airtabler::air_get_schema (at_base_id)

    table_num <- which (schema$tables$name == "CHAOSS Metrics")
    table_id <- schema$tables$id [table_num]
    fields <- schema$tables$fields [[table_num]]

    # ------ Update airtable field names -----
    updates_names <- airtable_update_schema_names (
        at_base_id,
        table_id,
        fields,
        metrics_metadata
    )

    # ------ Update airtable field descriptions -----
    updates_desc <- airtable_update_schema_desc (
        at_base_id,
        table_id,
        fields,
        metrics_metadata
    )

    list (names = updates_names, desc = updates_desc)
}

airtable_update_schema_names <- function (at_base_id,
                                          table_id,
                                          fields,
                                          metrics_metadata) {

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
    field_names_updated [index [, 1]] <-
        metrics_metadata$airtable_name [index [, 2]]

    if (any (duplicated (field_names_updated))) {
        dup <- field_names_updated [which (duplicated (field_names_updated))]
        cli::cli_abort (
            "Duplicated name of original field in airtable data: '{dup}'"
        )
    }

    index <- which (field_names_updated != field_names_orig)
    lapply (index, function (i) {
        f_i <- match (field_names_orig [i], fields$name)
        field_id <- fields$id [f_i]
        airtabler::air_update_field (
            base = at_base_id,
            table_id = table_id,
            field_id = field_id,
            name = field_names_updated [i]
        )
    })
}

#' Update airtable schema "descriptions" for each field.
#'
#' This must be run AFTER 'airtable_update_schema_names' as it presumes that
#' all names equal the 'airtable_name' fields of 'metrics_metadata'.
#' @noRd
airtable_update_schema_desc <- function (at_base_id,
                                         table_id,
                                         fields,
                                         metrics_metadata) {

    # Reduce metadata to ones with names in metadata only:
    index <- which (metrics_metadata$airtable_name %in% fields$name)
    metrics_metadata <- metrics_metadata [index, ]
    # paste URLs on to descriptions:
    metrics_metadata$description <-
        paste (metrics_metadata$description, metrics_metadata$url)
    metrics_metadata$description <-
        gsub ("\\s+$", "", metrics_metadata$description)

    if ("description" %in% names (fields)) {
        field_desc_orig <- field_desc_updated <- fields$description
    } else {
        field_desc_orig <- field_desc_updated <- rep ("", nrow (fields))
    }
    index_to_fields <- match (metrics_metadata$airtable_name, fields$name)
    field_desc_updated [index_to_fields] <- metrics_metadata$description

    # Rm NAs for equality comparison
    field_desc_orig [which (is.na (field_desc_orig))] <- ""
    field_desc_updated [which (is.na (field_desc_updated))] <- ""
    index <- which (field_desc_updated != field_desc_orig)
    fields_update <- fields [index, ]
    fields_update$description <- field_desc_updated [index]

    lapply (seq_len (nrow (fields_update)), function (i) {
        airtabler::air_update_field (
            base = at_base_id,
            table_id = table_id,
            field_id = fields_update$id [i],
            description = fields_update$description [i]
        )
    })
}
