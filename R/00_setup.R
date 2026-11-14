# Why this script is here
# I check where the external research data live before the analysis starts.
# The required directories and local configuration must already exist.

# AAS CTA reanalysis: locate and validate the external research-data directories.
# Patient-level data live outside the repository, under RESEARCH_DATA_ROOT.

# I read the configured root first so the analysis stops clearly if the data location is unavailable.
research_data_root <- Sys.getenv("RESEARCH_DATA_ROOT")

if (research_data_root == "") {
  stop(
    "RESEARCH_DATA_ROOT is not configured. ",
    "Set it in the project-specific .Renviron file."
  )
}

if (!dir.exists(research_data_root)) {
  stop("RESEARCH_DATA_ROOT does not point to an existing directory: ", research_data_root)
}

# I build the study paths and check that the raw and derived directories already exist.
aas_data_root <- file.path(research_data_root, "radiology", "cta-aortic-dissection")
aas_raw_dir <- file.path(aas_data_root, "raw")
aas_derived_dir <- file.path(aas_data_root, "derived")

required_directories <- c(aas_data_root, aas_raw_dir, aas_derived_dir)
missing_directories <- required_directories[!dir.exists(required_directories)]

if (length(missing_directories) > 0) {
  stop(
    "Required AAS data directory/directories not found:\n",
    paste(missing_directories, collapse = "\n")
  )
}
