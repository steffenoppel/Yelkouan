/* multi-event model to model ring and bird survival
*
*
* states
* 1 = alive with colour ring
* 2 = alive without colour ring
* 3 = dead/emigrated
*
* event
* 1 = resighted or recaptured with colour ring
* 2 = recaptured or resighted with colour ring lost
* 3 = not seen and not captured
*/
data {
  int<lower=2> nocc;                       // capture events
  int<lower=0> nind;                       // number of individuals
  int<lower=0> nreleases;                  // number of releases with new or without plastic ring
  int<lower=1,upper=3> y[nreleases,nocc];  // observation histories of each ring/individual 
  int<lower=0> first[nreleases];           // release occasion
  int<lower=0> last[nreleases];            // last occ before an individual was released with a new plastic ring
  int<lower=0, upper=nind> individual[nreleases];   // id of individual belonging to each plastic ring
  int<lower=0> firstpind[nind];     // first releass per individual
  int<lower=1,upper=2> sex[nind];         // sex
  int<lower=0,upper=2> age[nind,nocc];    // age 1= first year, 2 = older, 0 = not available
  int<lower=0,upper=1> logger[nind,nocc];    // indicator of a logger
  int<lower=0, upper=1> IndFirst[nind,nocc]; // indicator of first capture per individual
  int<lower=1, upper=3> ringseries[nreleases]; // indicator of ring series (black, grey, white)
}
parameters {
  real b0[2,2,nocc-1];  //dimensions sex x age
  real b1;
  real b2;
  real yeareff[2,2,nocc];
  real<lower=0>sigma[2,2];
  real a0[2];
  real d0[2];
  real<lower=0, upper=1> m[3];
}
transformed parameters {
  real<lower=0, upper=1> s[nind,nocc-1];
  real<lower=0, upper=1> p[nind, nocc];
  real<lower=0, upper=1> pr[nind, nocc];
  real<lower=0, upper=1> ps[3,nreleases, nocc-1,3];
  real<lower=0, upper=1> po[3,nreleases, nocc,3];
  real<lower=0, upper=1> zeta[nreleases, nocc, 3];

// intercept of survival

  // linear predictors
  for(i in 1:nind){
    for(ts0 in 1:(firstpind[i]-1)) s[i,ts0] = 1;
    for(ts in firstpind[i]:(nocc-1)){
      s[i,ts] = inv_logit(b0[sex[i], age[i,ts], ts] + b1*logger[i,ts]+ b2*IndFirst[i, ts]); // 
    }
    for(t0 in 1:(firstpind[i]-1)){
    p[i,t0] = 1;
    pr[i,t0] = 1;  
    }
    for(t in firstpind[i]:nocc){
      p[i,t] =  inv_logit(a0[sex[i]]); 
      pr[i,t] =  inv_logit(d0[sex[i]]);  
    }
   }

  // transition and observation matrices
  for(ii in 1:nreleases){
    for(tts0 in 1:(first[ii]-1)){
     for(ki in 1:3){
       for(kj in 1:3){
        ps[ki,ii,tts0,kj] = 1;
        po[ki,ii,tts0,kj] = 1;
      }
      }
     }
    for(tts in first[ii]:(nocc-1)){
      ps[1,ii,tts,1] = (1-m[ringseries[ii]])*s[individual[ii],tts];
      ps[1,ii,tts,2] = m[ringseries[ii]]*s[individual[ii],tts];
      ps[1,ii,tts,3] = 1-s[individual[ii],tts];
 
      ps[2,ii,tts,1] = 0;
      ps[2,ii,tts,2] = s[individual[ii],tts];
      ps[2,ii,tts,3] = 1-s[individual[ii],tts];
  
      ps[3,ii,tts,1] = 0;
      ps[3,ii,tts,2] = 0;
      ps[3,ii,tts,3] = 1;
   }

    for(tt in first[ii]:nocc){
      po[1,ii,tt,1] = pr[individual[ii],tt];
      po[1,ii,tt,2] = 0;
      po[1,ii,tt,3] = 1-pr[individual[ii],tt];

      po[2,ii,tt,1] = 0;
      po[2,ii,tt,2] = p[individual[ii],tt];
      po[2,ii,tt,3] = 1-p[individual[ii],tt];
  
      po[3,ii,tt,1] = 0;
      po[3,ii,tt,2] = 0;
      po[3,ii,tt,3] = 1;
      } // tt
   }// ii

  for(iii in 1:nreleases){
    for(ttt0 in 1:(first[iii]-1)){
    zeta[iii, ttt0,1] = 0;
    zeta[iii, ttt0,2] = 0;
    zeta[iii, ttt0,3] = 0;
   }
    zeta[iii,first[iii],1]  = y[iii,first[iii]]==1;
    zeta[iii,first[iii],2]  = y[iii,first[iii]]==2;
    zeta[iii,first[iii],3]  = 0;
     for(ttt in (first[iii]+1):nocc) { 
      for(j in 1:3){  
      zeta[iii,ttt,j] = dot_product(zeta[iii, ttt-1,], ps[,iii,ttt-1,j])*po[j,iii,ttt,y[iii,ttt]];
      }//j
    }//ttt
   }//iii
 }
model {
  vector[nreleases] lik;
  // priors
  for(v in 1:(nocc-1)){
   b0[1,1,v] ~ normal(0,1.5);
   b0[1,2,v] ~ normal(0,1.5);
   b0[2,1,v] ~ normal(0,1.5);
   b0[2,2,v] ~ normal(0,1.5);
  }
   a0[1] ~ normal(0, 1.5);  // = Normal(0,1.5) für Intercept 
   a0[2] ~ normal(0, 1.5);
    
   d0[1] ~ normal(0, 1.5);  // = Normal(0,1.5) für Intercept 
   d0[2] ~ normal(0, 1.5);
        
   b1 ~ normal(0,3);
   b2 ~ normal(0,3);

   m[1] ~ beta(1,1);
   m[2] ~ beta(1,1);
   m[3] ~ beta(1,1);
   
  // likelihood 
  for(i in 1:nreleases){
   lik[i]= sum(zeta[i,last[i],]); // sum the likelihood over all states (should have one number only)
   1 ~ bernoulli(lik[i]);  // 
  }
}
