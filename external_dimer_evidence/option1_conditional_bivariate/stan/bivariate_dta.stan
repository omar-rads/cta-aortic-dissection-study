data {
  int<lower=1> N;
  array[N] int<lower=0> tp;
  array[N] int<lower=0> fn;
  array[N] int<lower=0> tn;
  array[N] int<lower=0> fp;
  vector[2] mu_prior_mean;
  vector<lower=0>[2] mu_prior_sd;
  int<lower=1, upper=2> tau_prior_type;
  real<lower=0> tau_t_df;
  vector<lower=0>[2] tau_t_scale;
  vector<lower=0>[2] tau_gamma_shape;
  vector<lower=0>[2] tau_gamma_rate;
  real<lower=0> lkj_eta;
}

transformed data {
  array[N] int<lower=0> n_disease;
  array[N] int<lower=0> n_nondisease;
  for (i in 1:N) {
    n_disease[i] = tp[i] + fn[i];
    n_nondisease[i] = tn[i] + fp[i];
  }
}

parameters {
  vector[2] mu;
  vector<lower=0>[2] tau;
  cholesky_factor_corr[2] L_Omega;
  matrix[2, N] z;
}

transformed parameters {
  matrix[2, N] eta;
  matrix[2, N] study_probability;
  eta = rep_matrix(mu, N) + diag_pre_multiply(tau, L_Omega) * z;
  for (i in 1:N) {
    study_probability[1, i] = inv_logit(eta[1, i]);
    study_probability[2, i] = inv_logit(eta[2, i]);
  }
}

model {
  mu ~ normal(mu_prior_mean, mu_prior_sd);
  to_vector(z) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(lkj_eta);

  if (tau_prior_type == 1) {
    tau ~ student_t(tau_t_df, 0, tau_t_scale);
  } else {
    tau ~ gamma(tau_gamma_shape, tau_gamma_rate);
  }

  for (i in 1:N) {
    tp[i] ~ binomial_logit(n_disease[i], eta[1, i]);
    tn[i] ~ binomial_logit(n_nondisease[i], eta[2, i]);
  }
}

generated quantities {
  corr_matrix[2] Omega = multiply_lower_tri_self_transpose(L_Omega);
  real rho = Omega[1, 2];
  real pooled_sensitivity = inv_logit(mu[1]);
  real pooled_specificity = inv_logit(mu[2]);
  vector[2] eta_new = multi_normal_cholesky_rng(
    mu,
    diag_pre_multiply(tau, L_Omega)
  );
  real new_study_sensitivity = inv_logit(eta_new[1]);
  real new_study_specificity = inv_logit(eta_new[2]);
  array[N] int tp_rep;
  array[N] int tn_rep;
  vector[N] log_lik;
  real discrepancy_sensitivity_observed = 0;
  real discrepancy_sensitivity_replicated = 0;
  real discrepancy_specificity_observed = 0;
  real discrepancy_specificity_replicated = 0;

  for (i in 1:N) {
    real p_sens = study_probability[1, i];
    real p_spec = study_probability[2, i];
    real var_sens = fmax(n_disease[i] * p_sens * (1 - p_sens), 1e-9);
    real var_spec = fmax(n_nondisease[i] * p_spec * (1 - p_spec), 1e-9);
    tp_rep[i] = binomial_rng(n_disease[i], p_sens);
    tn_rep[i] = binomial_rng(n_nondisease[i], p_spec);
    log_lik[i] = binomial_lpmf(tp[i] | n_disease[i], p_sens)
      + binomial_lpmf(tn[i] | n_nondisease[i], p_spec);
    discrepancy_sensitivity_observed += square(tp[i] - n_disease[i] * p_sens) / var_sens;
    discrepancy_sensitivity_replicated += square(tp_rep[i] - n_disease[i] * p_sens) / var_sens;
    discrepancy_specificity_observed += square(tn[i] - n_nondisease[i] * p_spec) / var_spec;
    discrepancy_specificity_replicated += square(tn_rep[i] - n_nondisease[i] * p_spec) / var_spec;
  }
}
