# Reproduce the Table 2 subgroup breakdown from the retained analytic object.
# This reads derived data only. It does not rerun the clinical pipeline.
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript --vanilla scripts/validation/reproduce_table2_subgroups.R INPUT.rds EMPTY_OUTPUT_DIR", call. = FALSE)
}
source_path <- normalizePath(args[1], mustWork=TRUE)
out <- path.expand(args[2])
if (file.exists(out) && !dir.exists(out)) stop("Output path is not a directory.", call. = FALSE)
if (dir.exists(out) && length(list.files(out, all.files = TRUE, no.. = TRUE))) {
  stop("Use a new or empty output directory. Existing files are never overwritten.", call. = FALSE)
}
x <- readRDS(source_path)
keep <- c('add_rs','d_dimer_status','algorithm_class','aas_outcome','aas_positive')
stopifnot(all(keep %in% names(x)))
# Discard every other field immediately. No identifier or text is printed/exported.
x <- x[keep]
stopifnot(
  nrow(x) == 1193L,
  is.numeric(x$add_rs),
  all(is.na(x$add_rs) | x$add_rs %in% 0:3),
  all(x$d_dimer_status %in% c('Positive', 'Negative', 'Absent/not obtained')),
  all(x$algorithm_class %in% c('Concordant', 'Discordant', 'Unclassifiable')),
  all(x$aas_outcome %in% c('Positive', 'Negative', 'Unknown')),
  is.numeric(x$aas_positive),
  all(is.na(x$aas_positive) | x$aas_positive %in% 0:1)
)
score_group <- ifelse(is.na(x$add_rs),'Missing ADD-RS',ifelse(x$add_rs>=2,'ADD-RS 2-3','ADD-RS 0-1'))
stratum <- ifelse(score_group=='ADD-RS 2-3','ADD-RS 2-3',
 ifelse(x$algorithm_class=='Unclassifiable','Unclassifiable',paste(score_group,x$d_dimer_status,sep=' / ')))
levels <- c('ADD-RS 2-3','ADD-RS 0-1 / Positive','ADD-RS 0-1 / Absent/not obtained','ADD-RS 0-1 / Negative','Unclassifiable')
stopifnot(all(stratum %in% levels))
summarize <- function(i,label) {
 pos <- sum(x$aas_outcome[i]=='Positive')
 neg <- sum(x$aas_outcome[i]=='Negative')
 unk <- sum(x$aas_outcome[i]=='Unknown')
 ci <- if(pos+neg) binom.test(pos,pos+neg)$conf.int*100 else c(NA,NA)
 data.frame(group=label,total=sum(i),positive=pos,negative=neg,unknown=unk,
 known=pos+neg,yield_pct=if (pos+neg) 100*pos/(pos+neg) else NA_real_,exact95_lower_pct=ci[1],exact95_upper_pct=ci[2])
}
subgroups <- do.call(rbind,lapply(levels,function(g) summarize(stratum==g,g)))
classes <- do.call(rbind,lapply(c('Concordant','Discordant','Unclassifiable'),function(g) summarize(x$algorithm_class==g,g)))
score <- do.call(rbind,lapply(c('ADD-RS 2-3','ADD-RS 0-1','Missing ADD-RS'),function(g) summarize(score_group==g,g)))
expected_aas_positive <- ifelse(x$aas_outcome=='Positive',1L,ifelse(x$aas_outcome=='Negative',0L,NA_integer_))
mapping_agrees <- identical(is.na(x$aas_positive),is.na(expected_aas_positive)) &&
 all(x$aas_positive[!is.na(expected_aas_positive)] == expected_aas_positive[!is.na(expected_aas_positive)])
checks <- data.frame(check=c('aas_positive_matches_aas_outcome_including_NA','mutually_exclusive_exhaustive_strata','subgroup_positive_total','subgroup_negative_total','subgroup_unknown_total','concordant_is_high_score_or_low_positive','discordant_is_low_absent_or_negative','no_indeterminate_dimer','expected_panel_numerators','expected_panel_known_denominators','expected_panel_total_denominators'),
 passed=c(mapping_agrees,sum(subgroups$total)==1193L,sum(subgroups$positive)==79L,sum(subgroups$negative)==1111L,sum(subgroups$unknown)==3L,
 all((x$algorithm_class=='Concordant')==(score_group=='ADD-RS 2-3' | (score_group=='ADD-RS 0-1' & x$d_dimer_status=='Positive'))),
 all((x$algorithm_class=='Discordant')==(score_group=='ADD-RS 0-1' & x$d_dimer_status %in% c('Absent/not obtained','Negative'))),
 !any(x$d_dimer_status=='Indeterminate'),identical(subgroups$positive,c(32L,1L,42L,0L,4L)),identical(subgroups$known,c(230L,49L,824L,48L,39L)),identical(subgroups$total,c(230L,50L,825L,48L,40L))))
stopifnot(all(checks$passed))
# Write only after every check passes. Keep these outputs local until reviewed.
if (!dir.exists(out) && !dir.create(out, recursive = TRUE)) {
  stop("Could not create output directory.", call. = FALSE)
}
write.csv(subgroups,file.path(out,'subgroup_counts.csv'),row.names=FALSE)
write.csv(classes,file.path(out,'algorithm_counts.csv'),row.names=FALSE)
write.csv(score,file.path(out,'score_counts.csv'),row.names=FALSE)
write.csv(checks,file.path(out,'assertions.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
cat('All ',nrow(checks),' aggregate assertions passed.\n',sep='')
