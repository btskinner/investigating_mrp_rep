// --------------------------------------------------------------
//
// [ PROJ ] Postratify Helios survey responses
// [ FILE ] mrp.stan
//
// --------------------------------------------------------------

data {
  int<lower=1> N;     // # of cells
  int<lower=1> M;     // # of schools
  int<lower=1> L;     // # of 2nd-level covariates
  int<lower=1> J_sch; // # of school categories
  int<lower=1> J_gen; // # of gender categories
  int<lower=1> J_rac; // # of race/ethnicity categories
  int<lower=1> J_age; // # of age categories
  int<lower=1> J_gpa; // # of gpa categories
  int<lower=1> J_hrs; // # of hrs categories
  int views[N];       // # of cell members who saw the question
  int clicks[N];      // # of cell members who clicked box
  int sch[N];         // school categories
  int gen[N];         // gender categories
  int rac[N];         // race/ethnicity categories
  int age[N];         // age cat
  int gpa[N];         // gpa cat
  int hrs[N];         // hrs cat
  matrix[M,L] z;      // 2nd-level variables
}
parameters {
  // general intercept
  real beta_0;

  // using non-centered parameterization to help with convergence
  //
  // theta ~ N(mu, sigma) ==> mu + sigma * theta_std where theta_std ~ N(0,1)
  
  // standardized coefficients for reparametrized parameters
  vector[J_sch] sch_alpha_std;
  vector[J_gen] gen_alpha_std;
  vector[J_rac] rac_alpha_std;
  vector[J_age] age_alpha_std;
  vector[J_gpa] gpa_alpha_std;
  vector[J_hrs] hrs_alpha_std;
  vector[L] beta_std;

  // standard deviations for reparameterized parameters (restricted positive)
  real<lower=0> sch_alpha_sd;
  real<lower=0> gen_alpha_sd;
  real<lower=0> rac_alpha_sd;
  real<lower=0> age_alpha_sd;
  real<lower=0> gpa_alpha_sd;
  real<lower=0> hrs_alpha_sd;
  real<lower=0> beta_sd;
}
transformed parameters {
  // actual model parameters
  vector[J_sch] sch_alpha;
  vector[J_gen] gen_alpha;
  vector[J_rac] rac_alpha;
  vector[J_age] age_alpha;
  vector[J_gpa] gpa_alpha;
  vector[J_hrs] hrs_alpha;
  vector[L] beta;

  // linear combination
  vector[N] p_hat;

  // if: alpha ~ N(0, alpha_sd)
  // --> 0 + alpha_sd * alpha_std --> alpha_sd * alpha_std  
  sch_alpha = sch_alpha_sd * sch_alpha_std;
  gen_alpha = gen_alpha_sd * gen_alpha_std;
  rac_alpha = rac_alpha_sd * rac_alpha_std;
  age_alpha = age_alpha_sd * age_alpha_std;
  gpa_alpha = gpa_alpha_sd * gpa_alpha_std;
  hrs_alpha = hrs_alpha_sd * hrs_alpha_std;
  beta = beta_sd * beta_std;

  // linear combination (vectorized using indices)
  p_hat = beta_0
    + sch_alpha[sch]
    + gen_alpha[gen]
    + rac_alpha[rac]
    + age_alpha[age]
    + gpa_alpha[gpa]
    + hrs_alpha[hrs]
    + z[sch,] * beta;
}
model {
  // standardized coefficient priors: N(0,1)
  sch_alpha_std ~ std_normal();
  gen_alpha_std ~ std_normal();
  rac_alpha_std ~ std_normal();
  age_alpha_std ~ std_normal();
  gpa_alpha_std ~ std_normal();
  hrs_alpha_std ~ std_normal();
  beta_std ~ std_normal();
  beta_0 ~ std_normal();

  // standardized standard deviation priors: N+(0,1) due to restriction above
  sch_alpha_sd ~ std_normal();
  gen_alpha_sd ~ std_normal();
  rac_alpha_sd ~ std_normal();
  age_alpha_sd ~ std_normal();
  gpa_alpha_sd ~ std_normal();
  hrs_alpha_sd ~ std_normal();
  beta_sd ~ std_normal();

  // likelihood (binomial rather than bernoulli b/c we collapse [0/1]
  // observations to the cell level [clicks/views] as sufficient
  // statistic
  clicks ~ binomial_logit(views, p_hat);
}
