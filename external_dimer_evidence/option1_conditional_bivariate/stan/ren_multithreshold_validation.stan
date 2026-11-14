data {
  int<lower=1> N;
  array[N] int<lower=0> tp_low_threshold;
  array[N] int<lower=0> tp_high_threshold;
  array[N] int<lower=0> fp_low_threshold;
  array[N] int<lower=0> fp_high_threshold;
  array[N] int<lower=0> n_disease;
  array[N] int<lower=0> n_nondisease;
  vector[2] log_threshold;
  vector[4] mean_prior_mean;
  vector<lower=0>[4] mean_prior_sd;
  vector<lower=0>[4] tau_gamma_shape;
  vector<lower=0>[4] tau_gamma_rate;
}

parameters {
  vector[4] population_mean;
  vector<lower=0>[4] tau;
  matrix[4, N] z;
}

transformed parameters {
  matrix[4, N] study_parameter;
  matrix[2, N] fpr;
  matrix[2, N] tpr;
  study_parameter = rep_matrix(population_mean, N) + diag_matrix(tau) * z;

  for (i in 1:N) {
    for (t in 1:2) {
      fpr[t, i] = inv_logit(
        (study_parameter[1, i] - log_threshold[t])
        / exp(study_parameter[3, i])
      );
      tpr[t, i] = inv_logit(
        (study_parameter[2, i] - log_threshold[t])
        / exp(study_parameter[4, i])
      );
    }
  }
}

model {
  population_mean ~ normal(mean_prior_mean, mean_prior_sd);
  tau ~ gamma(tau_gamma_shape, tau_gamma_rate);
  to_vector(z) ~ std_normal();

  for (i in 1:N) {
    fp_low_threshold[i] ~ binomial(n_nondisease[i], fpr[1, i]);
    fp_high_threshold[i] ~ binomial(
      fp_low_threshold[i],
      fpr[2, i] / fpr[1, i]
    );
    tp_low_threshold[i] ~ binomial(n_disease[i], tpr[1, i]);
    tp_high_threshold[i] ~ binomial(
      tp_low_threshold[i],
      tpr[2, i] / tpr[1, i]
    );
  }
}

generated quantities {
  vector[2] pooled_sensitivity;
  vector[2] pooled_specificity;
  for (t in 1:2) {
    real pooled_fpr = inv_logit(
      (population_mean[1] - log_threshold[t]) / exp(population_mean[3])
    );
    pooled_sensitivity[t] = inv_logit(
      (population_mean[2] - log_threshold[t]) / exp(population_mean[4])
    );
    pooled_specificity[t] = 1 - pooled_fpr;
  }
}
