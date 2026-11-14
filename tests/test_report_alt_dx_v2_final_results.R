# Final audit of saved candidate flags, encounter collapse and aggregate tables.
# This reads only completed v2 private objects and prints aggregate validation.
source("R/00_setup.R")
args<-commandArgs(TRUE);stopifnot(length(args)==1L)
d<-file.path(aas_derived_dir,"report_alt_dx",args[1],"validation")
cc<-readRDS(file.path(d,"private_candidate_validation.rds"))
ee<-readRDS(file.path(d,"private_encounter_validation.rds"))
out<-"results/report_alt_dx_v2"
counts<-read.csv(file.path(out,"final_endpoint_counts.csv"))
cats<-read.csv(file.path(out,"qualifying_diagnosis_categories.csv"))
mult<-read.csv(file.path(out,"qualifying_diagnosis_multiplicity.csv"))
quality<-read.csv(file.path(out,"evidence_quality.csv"))
stopifnot(nrow(ee)==1111L,!anyDuplicated(ee$report_audit_id),!anyNA(cc$evidence_valid),!anyNA(cc$endpoint_qualifying),!anyNA(cc$endpoint_uncertain_signal))
for(p in 1:2){
 c<-cc[cc$pass==p,]
 # Independently reconstruct eligibility and unresolved sets from saved fields.
 evidence<-c$report_key_valid & c$report_quote_in_supplied_text & c$report_quote_in_original_text & c$indication_quote_valid & nzchar(c$report_evidence_quote)
 eligible<-evidence & c$finding_status=="present" & c$diagnostic_certainty%in%c("definite","probable_likely") & c$plausibly_explains_indication=="yes" & c$acuity%in%c("acute","likely_acute","indeterminate")
 unresolved<-c$acuity!="chronic" & c$plausibly_explains_indication%in%c("yes","uncertain") & (!evidence|c$finding_status=="uncertain"|c$diagnostic_certainty=="possible_indeterminate"|c$plausibly_explains_indication=="uncertain")
 stopifnot(identical(evidence,c$evidence_valid),identical(eligible,c$endpoint_qualifying),identical(unresolved,c$endpoint_uncertain_signal))
 posids<-unique(c$report_audit_id[eligible]);uncids<-setdiff(unique(c$report_audit_id[unresolved]),posids)
 endpoint<-ifelse(ee$report_audit_id%in%posids,"positive",ifelse(ee$report_audit_id%in%uncids,"uncertain","negative"))
 stopifnot(identical(endpoint,ee[[paste0("pass",p,"_endpoint")]]))
 x<-counts[counts$pass==p,];z<-c(positive=length(posids),negative=1111L-length(posids)-length(uncids),unclassifiable=length(uncids))
 stopifnot(all(x$n==z[x$endpoint]),sum(x$n)==1111L,all(x$classifiable_denominator==1111L-length(uncids)),all(x$eligible_denominator==1111L))
 q<-c[eligible,];t<-table(q$report_audit_id)
 m<-mult[mult$pass==p,];stopifnot(m$n_encounters[m$n_qualifying==">1"]==sum(t>1),sum(m$n_encounters)==1111L)
 for(i in which(cats$pass==p)){
  z<-q[q$diagnostic_category==cats$category[i],]
  stopifnot(cats$qualifying_candidate_count[i]==nrow(z),cats$encounters_with_qualifying_category[i]==length(unique(z$report_audit_id)))
 }
 stopifnot(sum(cats$qualifying_candidate_count[cats$pass==p])==nrow(q),quality$n[quality$pass==p&quality$metric=="Qualifying candidates"]==nrow(q))
}
t<-read.csv(file.path(out,"endpoint_transition.csv"));stopifnot(sum(t$n)==1111L)
for(p in 1:2){
 marg<-aggregate(t$n,list(t[[paste0("pass",p)]]),sum);x<-counts[counts$pass==p,]
 stopifnot(all(marg$x==x$n[match(marg$Group.1,x$endpoint)]))
}
a<-read.csv(file.path(out,"agreement_statistics.csv"));stopifnot(a$n_agree[a$dimension=="endpoint"]==sum(t$n[t$pass1==t$pass2]))
hist<-read.csv(file.path(out,"historical_regex_transition.csv"));stopifnot(sum(hist$n)==1111L)
mh<-aggregate(hist$n,list(hist$v2_pass1),sum);stopifnot(all(mh$x==counts$n[counts$pass==1][match(mh$Group.1,counts$endpoint[counts$pass==1])]))
cat("Final aggregate reconciliation passed: both 1,111-encounter passes, exact evidence/endpoint flags, denominators, categories, multiplicity, transition margins and historical table.\n")
