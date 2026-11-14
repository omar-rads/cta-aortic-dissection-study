# Synthetic regression tests. No source data, clinical text, or writes.
args <- commandArgs(TRUE)
old <- parse("R/11_report_alt_dx_validate.R")
assigned <- vapply(old,function(e) if(is.call(e)&&identical(e[[1]],as.name("<-"))&&is.symbol(e[[2]]))as.character(e[[2]]) else "",character(1))
start <- which(assigned=="candidate_fields");end <- which(assigned=="agreement_metrics")-1L
stopifnot(length(start)==1L,length(end)==1L,start<end)
grid <- expand.grid(status=c("present","negated_absent","uncertain"),certainty=c("definite","probable_likely","possible_indeterminate"),acuity=c("acute","likely_acute","chronic","indeterminate"),plausibility=c("yes","no","uncertain"),evidence=c("valid","wrong_case","changed_whitespace","wrong_key","missing_indication"),stringsAsFactors=FALSE)
packet_ids <- sprintf("SYNTH-%04d",seq_len(nrow(grid)+2L))
config <- list(accepted_endpoint_acuity=c("acute","likely_acute","indeterminate"))
synthetic_report <- "SYNTHETIC TEXT: focal  example."
synthetic_indication <- "SYNTHETIC INDICATION: example symptom."
packet_by_id <- setNames(lapply(packet_ids,function(id)list(report_audit_id=id,reports=list(list(source_report_key="synthetic_key",report_text=synthetic_report)),clinical_indications=list(list(indication_text=synthetic_indication)))),packet_ids)
report_map <- data.frame(report_audit_id=packet_ids,source_report_key="synthetic_key",original_full_report=synthetic_report)
make_candidate <- function(i){
 g<-grid[i,]
 list(normalized_diagnosis="Synthetic example",diagnostic_category="other",cta_component="chest",finding_status=g$status,diagnostic_certainty=g$certainty,acuity=g$acuity,plausibly_explains_indication=g$plausibility,report_evidence_quote=switch(g$evidence,wrong_case="Focal  example.",changed_whitespace="focal example.","focal  example."),indication_evidence_quote=if(g$evidence=="missing_indication")"" else "example symptom.",source_report_keys=list(if(g$evidence=="wrong_key")"wrong_key" else "synthetic_key"))
}
pass1 <- setNames(lapply(seq_len(nrow(grid)),function(i)list(report_audit_id=packet_ids[i],candidates=list(make_candidate(i)))),packet_ids[seq_len(nrow(grid))])
pass1[[packet_ids[nrow(grid)+1L]]]<-list(report_audit_id=packet_ids[nrow(grid)+1L],candidates=list())
positive_i<-which(grid$status=="present"&grid$certainty=="definite"&grid$acuity=="acute"&grid$plausibility=="yes"&grid$evidence=="valid")
uncertain_i<-which(grid$status=="uncertain"&grid$certainty=="possible_indeterminate"&grid$acuity=="acute"&grid$plausibility=="uncertain"&grid$evidence=="valid")
pass1[[packet_ids[nrow(grid)+2L]]]<-list(report_audit_id=packet_ids[nrow(grid)+2L],candidates=list(make_candidate(positive_i),make_candidate(uncertain_i)))
pass2<-pass1
for(i in start:end)eval(old[[i]],envir=.GlobalEnv)
valid<-grid$evidence=="valid"|(grid$evidence=="missing_indication"&grid$plausibility=="no")
qualifies<-valid&grid$status=="present"&grid$certainty!="possible_indeterminate"&grid$plausibility=="yes"&grid$acuity!="chronic"
unresolved<-grid$plausibility!="no"&grid$acuity!="chronic"&(!valid|grid$status=="uncertain"|grid$certainty=="possible_indeterminate"|grid$plausibility=="uncertain")
expected<-c(ifelse(qualifies,"positive",ifelse(unresolved,"uncertain","negative")),"negative","positive")
stopifnot(identical(encounter_validation$pass1_endpoint,expected),identical(encounter_validation$pass2_endpoint,expected),all(encounter_validation$complete_agreement),identical(candidates$evidence_valid[candidates$pass==1L][seq_len(nrow(grid))],valid))
# The two trailing cases assert zero candidates -> negative and qualifying
# candidate -> positive even when another candidate remains unresolved.
cat("Synthetic endpoint/evidence regression tests passed:",length(expected),"encounters per pass; strict case/whitespace/key/indication checks; no source data.\n")
