
#' Check a given file for having the right prediction format
check_prediction_file_format <- function( file_name ) {
    f = file_name
    
    if ( !file.exists(f) ) {
        cat("File", f, "does not exist.\n")
        return(invisible(NULL))
    }
    
    cat("\n\nChecking file:", basename(f), "\n")
    issues = FALSE
    
    # Check filename format
    if (!str_ends(basename(f), "_student_predictions.csv")) {
        cat("  ❌ Filename format incorrect. Should end with _student_predictions.csv\n")
        issues <- TRUE
    }
    
    df <- tryCatch(read_csv(f, show_col_types = FALSE, name_repair = "minimal"), 
                   error = function(e) NULL)
    
    if (is.null(df)) {
        cat("  ❌ Could not read file as CSV.\n")
        issues <- TRUE
        return(invisible(NULL))
    }
    
    cat( "  ✅ File read successfully with", nrow(df), "rows and", ncol(df), "columns.\n" )
    
    if (any(names(df) == "")) {
        cat("  🟠️  CSV file contains unnamed columns.\n")
        df <- df[, names(df) != ""]
        
        issues <- TRUE
    }
    
    required_cols <- c("student_id", "predicted_bully_level", 
                       "predicted_bully_high")
    missing <- setdiff(required_cols, colnames(df))
    if (length(missing) > 0) {
        cat("  ❌ Missing columns:", paste(missing, collapse = ", "), "\n")
        issues = TRUE
    }
    
    if ( !( "predicted_bully_risk" %in% colnames(df) ) ) {
        cat("  🟠 'predicted_bully_risk' column not included--but it is worth including if you have it.\n")
        issues = TRUE
    }
    
    extra <- setdiff(colnames(df), c( required_cols, "predicted_bully_risk") )
    if (length(extra) > 0) {
        cat("  ❌ ", length(extra), " extra columns found\n", sep="")
        issues = TRUE
    }
    
    
    if ( "student_id" %in% colnames(df) ) {
        if ( sum(duplicated(df$student_id)) > 0 ) {
            cat("  ❌ 'student_id' column contains duplicate values.\n")
            issues = TRUE
        }
    }
    
    if ( "predicted_bully_level" %in% colnames(df) ) {
        if (any(is.na(df$predicted_bully_level))) {
            ss = sum(is.na(df$predicted_bully_level))
            cat("  🟠 'predicted_bully_level' contains", ss, "NA values.\n")
        }
        if (!is.numeric(df$predicted_bully_level)) {
            cat("  ❌ 'predicted_bully_level' is not numeric.\n")
            issues = TRUE
        }
        tots = sum( !is.na( df$predicted_bully_level ) )
        if ( tots == 0 ) {
            cat("  ❌ 'predicted_bully_level' contains all NA values.\n")
            issues = TRUE
        } else {
            cat("  ✅ 'predicted_bully_level' contains", tots, "non-NA values.\n")
        }
        
    }
    
    if ( "predicted_bully_high" %in% colnames(df) ) {
        if (!is.numeric(df$predicted_bully_high)) {
            cat("  ❌ 'predicted_bully_high' is not numeric.\n")
            issues = TRUE
        }
        
        if (!all(df$predicted_bully_high %in% c(0, 1, NA))) {
            cat("  ❌ 'predicted_bully_high' should only contain 0 or 1 or NA values.\n")
            issues = TRUE
        }
        
        tots = sum( !is.na( df$predicted_bully_high ) )
        if ( tots == 0 ) {
            cat("  ❌ 'predicted_bully_high' contains all NA values.\n")
            issues = TRUE
        } else {
            cat("  ✅ 'predicted_bully_high' contains", tots, "non-NA values.\n")
        }
    }
    
    if ( "predicted_bully_risk" %in% colnames(df) ) {
        
        if ( any( is.na( df$predicted_bully_risk ) ) ) {
             ss = sum(is.na(df$predicted_bully_risk))
            cat("  🟠 'predicted_bully_risk' contains", ss, "NA values.\n")
        } else if ( !is.numeric(df$predicted_bully_risk) || 
                    any(df$predicted_bully_risk < 0 | df$predicted_bully_risk > 1)) {
            cat("  ❌ 'predicted_bully_risk' should be numeric and between 0 and 1.\n")
            issues = TRUE
        }
        
        tots = sum( !is.na( df$predicted_bully_risk ) )
        if ( tots == 0 ) {
            cat("  ❌ 'predicted_bully_risk' contains all NA values.\n")
            issues = TRUE
        } else {
            cat("  ✅ 'predicted_bully_risk' contains", tots, "non-NA values.\n")
        }
    }
    
    if ( !issues ) {
        cat("  ✅ Check complete and file in good shape\n\n")
    }
    
    invisible(NULL)
}

