
options mprint source;
%macro sim_binom(N=,K= , rr_trt=,lambda_baseline=,op=,mar_ni=, type=,miss=,seq=)/minoperator;
/* N sample size                             *
 * repeat number for simulation K            *
 * group, treatment group-categorical with values   *
 * 1-test drug group  2-active comparator drug group *
 * rr_trt recurrent event rate ratio between test drug group and active comparator drug group *
 * lambda_baseline the mean recurrent event number assuming Poisson distribution              *  
 * mar_ni non-inferiority margin of rate ratio between test drug group and active comparator drug group *
 * op-overdispersion parameter in negative binomial model *
 * type-different types of NB model assumption violations *
 * miss-different types of missing patterns *
 * seq- separate different simulation types in the files generated within the macro * /
 /*generate baseline event number in the past time duration with T=1 year */
 /*assume Poisson distribution with lamda=lambda_baseline */
%do m=1 %to &K;

data baseline_&m._&seq.;
   m1=&lambda_baseline.;
    %do j= 1 %to &N/2;
      group=1;
      num_bs=rand('POISSON',m1);
	  output;
   %end;
 %do j= &N/2+1 %to &N;
      group=2;
      num_bs=rand('POISSON',m1);
	  output;
   %end;
keep num_bs; 
keep group;
 run;

data baseline_&m._&seq.1;
  set baseline_&m._&seq.;

/* missing completely at random pattern 101 10% dropout at both treatment groups*/  
  %if &miss. eq 101 %then %do;    
      group11=rand('BERNOULLI',0.9);
        if group11 eq 0 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 then do;
             group13=rand('BERNOULLI',0.5); 
             end;
             if group13 eq 0 then do;
               group14=rand('BERNOULLI',2/3);   
                 end;
   %end;
   /* missing completely at random pattern 102  10% dropout at test drug group 20% dropout at active control group*/  
   %else %if &miss. eq 102 %then %do;
    if group eq 2 then do;    
      group11=rand('BERNOULLI',0.8);
        end;
        if group11 eq 0 and group eq 2 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and group eq 2 then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and group eq 2 then do;
               group14=rand('BERNOULLI',4/7);
                 end;   
                  if group eq 1 then do;    
      group11=rand('BERNOULLI',0.9);
        end;
        if group11 eq 0 and group eq 1 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 and group eq 1 then do;
             group13=rand('BERNOULLI',0.5); 
             end;
             if group13 eq 0 and group eq 1 then do;
               group14=rand('BERNOULLI',2/3);
                 end;  
   %end;
      /* missing completely at random pattern 103  20% dropout at test drug group 10% dropout at active control group*/  
     %else %if &miss. eq 103 %then %do;
    if group eq 1 then do;    
      group11=rand('BERNOULLI',0.8);
        end;
        if group11 eq 0 and group eq 1 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and group eq 1 then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and group eq 1 then do;
               group14=rand('BERNOULLI',4/7);
                 end;   
                  if group eq 2 then do;    
      group11=rand('BERNOULLI',0.9);
        end;
        if group11 eq 0 and group eq 2 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 and group eq 2 then do;
             group13=rand('BERNOULLI',0.5); 
             end;
             if group13 eq 0 and group eq 2 then do;
               group14=rand('BERNOULLI',2/3);
                 end;  
   %end;
        /* missing completely at random pattern 104  30% dropout at both test drug group and active control group*/     
    %else %if &miss. eq 104 %then %do;  
      group11=rand('BERNOULLI',0.7);
        if group11 eq 0 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 then do;
             group13=rand('BERNOULLI',4/9); 
             end;
             if group13 eq 0 then do;
               group14=rand('BERNOULLI',0.5);  
                 end; 
   %end;
  /* missing  at random pattern 201 baseline dependent 20% dropout at both test drug group and active control group for exacerbations in the previous year>=3*/  
   %else %if &miss. eq 201 %then %do;
     if num_bs ge 3 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs ge 3  then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs ge 3  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs ge 3  then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
      %end;
	    /* missing  at random pattern 202 baseline dependent */
/* 20% dropout at test drug group for exacerbations in the previous year>=3*/  
	  /* 20% dropout at active control group for exacerbations in the previous year<3*/ 
           %else %if &miss. eq 202 %then %do;
     if num_bs ge 3 and group eq 1 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs ge 3 and group eq 1 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs ge 3 and group eq 1  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs ge 3 and group eq 1 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
                  if num_bs lt 3 and group eq 2 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs lt 3 and group eq 2 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs lt 3 and group eq 2  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs lt 3 and group eq 2 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
      %end; 
	    /* missing  at random pattern 203 baseline dependent */
/* 20% dropout at test drug group for exacerbations in the previous year<3*/  
	  /* 20% dropout at active control group for exacerbations in the previous year>=3*/ 
                %else %if &miss. eq 203 %then %do;
     if num_bs ge 3 and group eq 2 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs ge 3 and group eq 2 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs ge 3 and group eq 2  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs ge 3 and group eq 2 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
                  if num_bs lt 3 and group eq 1 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs lt 3 and group eq 1 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs lt 3 and group eq 1  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs lt 3 and group eq 1 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
      %end; 
/* lack of 20% efficacy due to non-compliance  10% in both treatment groups*/
 %else %if &miss. eq 401 %then %do;
   if group eq 1 then do;
     group11=rand('BERNOULLI',0.9);
   end;
   else if group eq 2 then do;
     group11=rand('BERNOULLI',0.9);
   end;
/* lack of 20% efficacy due to non-compliance  30% in both treatment groups*/
 %else %if &miss. eq 402 %then %do;
   if group eq 1 then do;
     group11=rand('BERNOULLI',0.7);
   end;
   else if group eq 2 then do;
     group11=rand('BERNOULLI',0.7);
   end;
    %end;

/* lack of 20% efficacy due to non-compliance  10% in test drug group 30% in active control group*/
 %else %if &miss. eq 403 %then %do;
   if group eq 1 then do;
     group11=rand('BERNOULLI',0.9);
   end;
   else if group eq 2 then do;
     group11=rand('BERNOULLI',0.7);
   end;
   %end;

/* lack of 20% efficacy due to non-compliance  30% in test drug group 10% in active control group*/
 %else %if &miss. eq 404 %then %do;
   if group eq 1 then do;
     group11=rand('BERNOULLI',0.7);
   end;
   else if group eq 2 then do;
     group11=rand('BERNOULLI',0.3);
   end;
   %end;
run;







 
 
data event1_&m._&seq.;
  set baseline_&m._&seq.;
   rr1=&rr_trt.;
   if group eq 1 then group_i=1;
   else if group eq 2 then group_i=0;
lrisk=log(365.25);
%if &type. eq negbio and &miss. eq %str()  %then %do;
 alpha=1/&op;
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
%end;

%if &type. eq negbio and &miss. in (101 102 103 104 201 202 203 )  %then %do;
 alpha=1/&op;
if group11 eq 1 then do;
 m_event=exp(-0.5+0.39*num_bs+group_i*log(rr1));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
end;
else if group11 eq 0 and group12 eq 1 then do;
 m_event=exp(-0.5+0.39*num_bs+group_i*log(rr1)+log(0.75));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
lrisk=log(365.25*0.75);
end;
else if group12 eq 0 and group13 eq 1 then do;
 m_event=exp(-0.5+0.39*num_bs+group_i*log(rr1)+log(0.5));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
lrisk=log(365.25*0.5);
end;
else if group13 eq 0 and group14 eq 1 then do;
 m_event=exp(-0.5+0.39*num_bs+group_i*log(rr1)+log(0.25));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
lrisk=log(365.25*0.25);
end;
else if group14 eq 0 then do;
num_event=.;
end;
%end;


%else %if &type. eq negbio and &miss. in (401 402 403 404)  %then %do;
 alpha=1/&op;
if group11 eq 1 then do;
 m_event=exp(-0.5+0.39*num_bs+group_i*log(rr1));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
end;
else if group11 eq 0 then do;
rr1=1.25*rr1;
 m_event=exp(-0.5+0.39*num_bs+group_i*log(rr1));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
lrisk=log(365.25*0.75);
end;
%end;














%else %if &type. eq pois %then %do;
num_event=rand('POISSON',m_event);
%end;
%else %if &type. eq rdiff0pois %then %do;
rr11=1.0*rr1;
rr12=1.0*rr1;
num_bs1=num_bs/2;
m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11)-LOG(2));
num_event1=rand('POISSON',m_event1);
num_bs2=num_event1;
m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12)-LOG(2));
num_event2=rand('POISSON',m_event2);
num_event=num_event2+num_event1;
%end;
%else %if &type. eq rdiff2pois %then %do;
rr11=1.2*rr1;
rr12=0.8*rr1;
num_bs1=num_bs/2;
m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11)-LOG(2));
num_event1=rand('POISSON',m_event1);
num_bs2=num_event1;
m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12)-LOG(2));
num_event2=rand('POISSON',m_event2);
num_event=num_event2+num_event1;
%end;
%else %if &type. eq rdiff1pois %then %do;
rr11=0.8*rr1;
rr12=1.2*rr1;
num_bs1=num_bs/2;
m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11)-LOG(2));
num_event1=rand('POISSON',m_event1);
num_bs2=num_event1;
m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12)-LOG(2));
num_event2=rand('POISSON',m_event2);
num_event=num_event2+num_event1;
%end;



%else %if &type. eq gammamix3 %then %do;
 %do j=1 %to &N;
 group2=rand('BERNOULLI',0.9);
 %end;
alpha=1/&op;
rr11=rr1*0.95;
rr12=rr1*1.45;
if group2 eq 1 then do;
 m_event1=exp(-0.5+0.39*num_bs+group_i*log(rr11));
 beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta1*rangam(seed,alpha);
num_event=rand('POISSON',gamma); 
end;
else if group2 eq 0 then do;
 m_event2=exp(-0.5+0.39*num_bs+group_i*log(rr12));
 beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta2*rangam(seed,alpha);
num_event=rand('POISSON',gamma); 
end;
%end;
%else %if &type. eq rdiff1 %then %do;
alpha=1/&op.;
rr11=0.8*rr1;
rr12=1.2*rr1;
num_bs1=num_bs/2;
 m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11)-LOG(2));
  beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12)-LOG(2));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;
%else %if &type. eq rdiff2 %then %do;
alpha=1/&op.;
rr11=1.2*rr1;
rr12=0.8*rr1;
num_bs1=num_bs/2;
 m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11)-LOG(2));
  beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12)-LOG(2));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;

%else %if &type. eq rdiff0 %then %do;
alpha=1/&op.;
rr11=1.0*rr1;
rr12=1.0*rr1;
num_bs1=num_bs/2;
m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11)-LOG(2));
beta1=m_event1*&op.;  
seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12)-LOG(2));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;

%else %if &type. eq rdiff00 %then %do;
alpha=1/&op.;
rr11=1.0*rr1;
rr12=1.0*rr1;
rr13=1.0*rr1;
num_bs1=num_bs/2;
 m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11));
  beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);





num_event=num_event2+num_event1;
%end;



%else %if &type. eq gammamix2 %then %do;
 %do j=1 %to &N;
 group2=rand('BERNOULLI',0.9);
 %end;
alpha=1/&op;
rr11=rr1*0.9;
rr12=rr1*1.9;
if group2 eq 1 then do;
 m_event1=exp(-0.5+0.39*num_bs+group_i*log(rr11));
 beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta1*rangam(seed,alpha);
num_event=rand('POISSON',gamma); 
end;
else if group2 eq 0 then do;
 m_event2=exp(-0.5+0.39*num_bs+group_i*log(rr12));
 beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta2*rangam(seed,alpha);
num_event=rand('POISSON',gamma); 
end;
%end;
%else %if &type. eq gammamix1 %then %do;
 %do j=1 %to &N;
 group2=rand('BERNOULLI',0.9);
 %end;
alpha=1/&op;
rr11=rr1*1.2;
rr12=rr1*0.8;
if group2 eq 1 then do;
 m_event1=exp(-0.5+0.39*num_bs+group_i*log(rr11));
 beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta1*rangam(seed,alpha);
num_event=rand('POISSON',gamma); 
end;
else if group2 eq 0 then do;
 m_event2=exp(-0.5+0.39*num_bs+group_i*log(rr12));
 beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta2*rangam(seed,alpha);
num_event=rand('POISSON',gamma); 
end;
%end;



%else %if &type. eq poismix1 %then %do;
 %do j=1 %to &N;
 group2=rand('BERNOULLI',0.5);
 %end;
rr11=rr1*1.2;
rr12=rr1*0.8;
 if group2 eq 1 then do;
 m_event1=exp(-0.5+0.39*num_bs+group_i*log(rr11));
num_event=rand('POISSON',m_event1);
end;
else if group2 eq 0 then do;
m_event2=exp(-0.5+0.39*num_bs+group_i*log(rr12));
num_event=rand('POISSON',m_event2);
end;
%end;
%else %if &type. eq poismix2 %then %do;
 %do j=1 %to &N;
 group2=rand('BERNOULLI',0.1);
 %end;
rr11=rr1*0.8;
rr12=rr1*2.8;
if group2 eq 1 then do;
 m_event1=exp(-0.5+0.39*num_bs+group_i*log(rr11));
num_event=rand('POISSON',m_event1);
end;
else if group2 eq 0 then do;
  m_event2=exp(-0.5+0.39*num_bs+group_i*log(rr12));
num_event=rand('POISSON',m_event2);
end;
%end;
%else %if &type. eq poismix3 %then %do;
 %do j=1 %to &N;
 group2=rand('BERNOULLI',0.9);
 %end;
rr12=rr1*0.55;
rr11=rr1*1.05;
if group2 eq 1 then do;
 m_event1=exp(-0.5+0.39*num_bs+group_i*log(rr11));

num_event=rand('POISSON',m_event1);
end;
else if group2 eq 0 then do;
  m_event2=exp(-0.5+0.39*num_bs+group_i*log(rr12));
num_event=rand('POISSON',m_event2);
end;

%end;


%if &type. eq poismix3 or &type. eq poismix1 or &type. eq poismix3 or &type. eq gammamix1 or &type. eq gammamix2 or &type. eq gammamix3 %then %do;
keep num_event lrisk num_bs group group2;
%end;
%else %do;
keep num_event lrisk num_bs group;
%end;
 run;  


proc sort data=event1_&m._&seq.;
by group;
run;


/*P-value for superiority test need to divide by 2 */
/*Estimate and 95% CI the same */
	 ods listing ;
ods output Estimates=est_group_&m._&seq.; 
PROC genmod DATA=event1_&m._&seq. noprint;
  CLASS group;
  MODEL num_event =group num_bs
  %if &type. eq rdiff0pois or &type. eq rdiff1pois or &type. eq rdiff2pois or &type. eq pois or & type. eq poismix1 or &type. eq poismix2 or &type. eq poismix3 %then %do;
  /DIST= Poisson link=log   offset=lrisk ;*TYPE3 WALD;
  %end;
  %else %do;
       /DIST= negbin link=log   offset=lrisk ;*TYPE3 WALD;
       %end;
  lsmeans group /  cl;     
 estimate 'test-active' group 1 -1  /exp;      
       run;
 %if &m. eq 1 %then %do;      
 data summary_&seq.;
   set est_group_&m._&seq.;      
where label eq 'Exp(test-active)';  
upper=LBetaUpperCL;
if upper lt 1 and upper ne . then ind_sup=1;
else if upper ge 1 then ind_sup=0;
marni=&mar_ni.;
if upper < marni and upper ne . then ind_ni=1;
else if upper ge marni then ind_ni=0;
order=1;
run;
%end;
%else  %if &m. ne 1 %then %do;      
 data summary_&seq.;
   set est_group_&m._&seq. summary_&seq.;      
where label eq 'Exp(test-active)';  
upper=LBetaUpperCL;
if upper lt 1 and upper ne . then ind_sup=1;
else if upper ge 1 then ind_sup=0;
marni=&mar_ni.;
if upper < marni and upper ne . then ind_ni=1;
else if upper ge marni then ind_ni=0;
order=1;
run;
%end;
%end;
proc sql noprint;
create table summary1_&seq. as 
  select sum(ind_ni)/sum(order) as pct_ni
	,sum(ind_sup)/sum(order) as pct_sup,label 
	from summary_&seq.
	where ind_ni ne . and ind_sup ne .
	group by label;
	quit;
%mend sim_binom;



%sim_binom(N=3000,K=3, rr_trt=1,lambda_baseline=1.31,op=0.677,mar_ni=1.15,type=negbio,miss=101,seq=1); 










/* missing completely at random pattern 101 10% dropout at both treatment groups*/  
  %if &miss. eq 101 %then %do;    
      group11=rand('BERNOULLI',0.9);
        if group11 eq 0 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 then do;
             group13=rand('BERNOULLI',0.5); 
             end;
             if group13 eq 0 then do;
               group14=rand('BERNOULLI',2/3);   
                 end;
   %end;
   /* missing completely at random pattern 102  10% dropout at test drug group 20% dropout at active control group*/  
   %else %if &miss. eq 102 %then %do;
    if group eq 2 then do;    
      group11=rand('BERNOULLI',0.8);
        end;
        if group11 eq 0 and group eq 2 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and group eq 2 then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and group eq 2 then do;
               group14=rand('BERNOULLI',4/7);
                 end;   
                  if group eq 1 then do;    
      group11=rand('BERNOULLI',0.9);
        end;
        if group11 eq 0 and group eq 1 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 and group eq 1 then do;
             group13=rand('BERNOULLI',0.5); 
             end;
             if group13 eq 0 and group eq 1 then do;
               group14=rand('BERNOULLI',2/3);
                 end;  
   %end;
      /* missing completely at random pattern 103  20% dropout at test drug group 10% dropout at active control group*/  
     %else %if &miss. eq 103 %then %do;
    if group eq 1 then do;    
      group11=rand('BERNOULLI',0.8);
        end;
        if group11 eq 0 and group eq 1 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and group eq 1 then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and group eq 1 then do;
               group14=rand('BERNOULLI',4/7);
                 end;   
                  if group eq 2 then do;    
      group11=rand('BERNOULLI',0.9);
        end;
        if group11 eq 0 and group eq 2 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 and group eq 2 then do;
             group13=rand('BERNOULLI',0.5); 
             end;
             if group13 eq 0 and group eq 2 then do;
               group14=rand('BERNOULLI',2/3);
                 end;  
   %end;
        /* missing completely at random pattern 104  30% dropout at both test drug group and active control group*/     
    %else %if &miss. eq 104 %then %do;  
      group11=rand('BERNOULLI',0.7);
        if group11 eq 0 then do;
          group12=rand('BERNOULLI',0.4); 
          end;
           if group12 eq 0 then do;
             group13=rand('BERNOULLI',4/9); 
             end;
             if group13 eq 0 then do;
               group14=rand('BERNOULLI',0.5);  
                 end; 
   %end;
  /* missing  at random pattern 201 baseline dependent 20% dropout at both test drug group and active control group for exacerbations in the previous year>=3*/  
   %else %if &miss. eq 201 %then %do;
     if num_bs ge 3 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs ge 3  then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs ge 3  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs ge 3  then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
      %end;
	    /* missing  at random pattern 202 baseline dependent */
/* 20% dropout at test drug group for exacerbations in the previous year>=3*/  
	  /* 20% dropout at active control group for exacerbations in the previous year<3*/ 
           %else %if &miss. eq 202 %then %do;
     if num_bs ge 3 and group eq 1 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs ge 3 and group eq 1 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs ge 3 and group eq 1  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs ge 3 and group eq 1 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
                  if num_bs lt 3 and group eq 2 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs lt 3 and group eq 2 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs lt 3 and group eq 2  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs lt 3 and group eq 2 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
      %end; 
	    /* missing  at random pattern 203 baseline dependent */
/* 20% dropout at test drug group for exacerbations in the previous year<3*/  
	  /* 20% dropout at active control group for exacerbations in the previous year>=3*/ 
                %else %if &miss. eq 203 %then %do;
     if num_bs ge 3 and group eq 2 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs ge 3 and group eq 2 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs ge 3 and group eq 2  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs ge 3 and group eq 2 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
                  if num_bs lt 3 and group eq 1 then do;     
      group11=rand('BERNOULLI',0.8);
      end;
        if group11 eq 0 and num_bs lt 3 and group eq 1 then do;
          group12=rand('BERNOULLI',0.35); 
          end;
           if group12 eq 0 and num_bs lt 3 and group eq 1  then do;
             group13=rand('BERNOULLI',6/13); 
             end;
             if group13 eq 0 and num_bs lt 3 and group eq 1 then do;
               group14=rand('BERNOULLI',4/7);
                 end;    
      %end; 


%macro sim_binom(N=,K= , rr_trt=,lambda_baseline=,op=,mar_ni=, type=,miss=,seq=)/minoperator;
/* N sample size                             *
 * repeat number for simulation K            *
 * group, treatment group-categorical with values   *
 * 1-test drug group  2-active comparator drug group *
 * rr_trt recurrent event rate ratio between test drug group and active comparator drug group *
 * lambda_baseline the mean recurrent event number assuming Poisson distribution              *  
 * mar_ni non-inferiority margin of rate ratio between test drug group and active comparator drug group *
 * op-overdispersion parameter in negative binomial model *
 * type-different types of NB model assumption violations *
 * miss-different types of missing patterns *
 * seq- separate different simulation types in the files generated within the macro * /
 /*generate baseline event number in the past time duration with T=1 year */
 /*assume Poisson distribution with lamda=lambda_baseline */
%do m=1 %to &K;

data baseline_&m._&seq.;
   m1=&lambda_baseline.;
    %do j= 1 %to &N/2;
      group=1;
      num_bs=rand('POISSON',m1);
	   if num_event eq 0 then do;
   t_final=t_drop;
   end;
   if num_event ge 1 then do;
%do s=1 %to num_event;
  p_&s.=rand('UNIFORM');
  t_&s.=-(log(1-p_&s.+p_&s.*exp(-&m.))*365.25)/&m.;
  output;
  %end; 
   end;
	  output;
   %end;
 %do j= &N/2+1 %to &N;
      group=2;
      num_bs=rand('POISSON',m1);
	  output;
   %end;
keep num_bs; 
keep group;
 run;
 %end;
 %mend;

%sim_binom(N=3000,K=10, rr_trt=1.2,lambda_baseline=1.31,op=0.677,mar_ni=1.15,type=,miss=,seq=1); 


%macro sim_miss(N=,lambda=,m=);
data sim;
%do j=1 %to &N.;
 num_event=rand('POISSON',&lambda.);
   t_drop=365.25*rand('UNIFORM');
 output;
 %end;
 keep num_event;
 keep t_drop;
 run;

data sim2;
  set sim;
 /*dropout time and time to multiple events */

 if num_event eq 0 then do;
   t_final=t_drop;
   end;
else if num_event ge 1 then do;
 

  output;
  %end;
keep 
  end;
run;
%mend sim_miss;

%sim_miss(N=3000,lambda=1.41,m=1.78);

 %do s=1 %to num_event.;
  p_&s.=rand('UNIFORM');
  t_&s.=-(log(1-p_&s.+p_&s.*exp(-&m.))*365.25)/&m.;
  %end; 


   %do s=1 %to num_&j.;
  p_&j.&s.=rand('UNIFORM');
  t_&j.&s.=-(log(1-p_&j.&s.+p_&j.&s.*exp(-&m.))*365.25)/&m.;
  %end; 
  
  
  
/*12122013*/

/*with a very weak dependence of exacerbation based on exacerbation history */
%macro sim_binom2(N=,K= , rr_trt=,lambda_baseline=,op=,mar_ni=, type=,miss=,seq=)/minoperator;
/* N sample size                             *
 * repeat number for simulation K            *
 * group, treatment group-categorical with values   *
 * 1-test drug group  2-active comparator drug group *
 * rr_trt recurrent event rate ratio between test drug group and active comparator drug group *
 * lambda_baseline the mean recurrent event number assuming Poisson distribution              *  
 * mar_ni non-inferiority margin of rate ratio between test drug group and active comparator drug group *
 * op-overdispersion parameter in negative binomial model *
 * type-different types of NB model assumption violations *
 * miss-different types of missing patterns *
 * seq- separate different simulation types in the files generated within the macro * /
 /*generate baseline event number in the past time duration with T=1 year */
 /*assume Poisson distribution with lamda=lambda_baseline */
%do m=1 %to &K;

data baseline_&m._&seq.;
   m1=&lambda_baseline./2;
    %do j= 1 %to &N/2;
      group=1;
      num_bs11=rand('POISSON',m1);
      num_bs12=rand('POISSON',m1);
	  output;
   %end;
 %do j= &N/2+1 %to &N;
      group=2;
      num_bs11=rand('POISSON',m1);
      num_bs12=rand('POISSON',m1);
	  output;
   %end;
keep num_bs11 num_bs12; 
keep group;
 run;
 
 
data event1_&m._&seq.;
  set baseline_&m._&seq.;
   rr1=&rr_trt.;
   num_bs1=num_bs11;
   if group eq 1 then group_i=1;
   else if group eq 2 then group_i=0;
 num_bs=num_bs11+num_bs12;
lrisk=log(365.25);
%if &type. eq negbio %then %do;
 alpha=1/&op;

  m_event=exp(-0.25+0.15*num_bs+group_i*log(rr1));
 beta=m_event*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma=beta*rangam(seed,alpha);
num_event=rand('POISSON',gamma);
%end;
%else %if &type. eq pois %then %do;
  m_event=exp(-0.25+0.15*num_bs+group_i*log(rr1));
num_event=rand('POISSON',m_event);
%end;
%else %if &type. eq rdiff0pois %then %do;
rr11=1.0*rr1;
rr12=1.0*rr1;
m_event1=exp(-0.25+0.15*num_bs11+group_i*log(rr11)-LOG(2));
num_event1=rand('POISSON',m_event1);
num_bs2=num_event1;
m_event2=exp(-0.25+0.15*num_bs2+group_i*log(rr12)-LOG(2));
num_event2=rand('POISSON',m_event2);
num_event=num_event2+num_event1;
%end;
%else %if &type. eq rdiff2pois %then %do;
rr11=1.2*rr1;
rr12=0.8*rr1;

m_event1=exp(-0.5+0.15*num_bs11+group_i*log(rr11)-LOG(2));
num_event1=rand('POISSON',m_event1);
num_bs2=num_event1;
m_event2=exp(-0.5+0.15*num_bs2+group_i*log(rr12)-LOG(2));
num_event2=rand('POISSON',m_event2);
num_event=num_event2+num_event1;
%end;
%else %if &type. eq rdiff1pois %then %do;
rr11=0.8*rr1;
rr12=1.2*rr1;
m_event1=exp(-0.5+0.15*num_bs11+group_i*log(rr11)-LOG(2));
num_event1=rand('POISSON',m_event1);
num_bs2=num_event1;
m_event2=exp(-0.5+0.15*num_bs2+group_i*log(rr12)-LOG(2));
num_event2=rand('POISSON',m_event2);
num_event=num_event2+num_event1;
%end;




%else %if &type. eq rdiff1 %then %do;
alpha=1/&op.;
rr11=0.8*rr1;
rr12=1.2*rr1;
 m_event1=exp(-0.5+0.15*num_bs1+group_i*log(rr11)-LOG(2));
  beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.15*num_bs2+group_i*log(rr12)-LOG(2));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;
%else %if &type. eq rdiff2 %then %do;
alpha=1/&op.;
rr11=1.2*rr1;
rr12=0.8*rr1;
 m_event1=exp(-0.5+0.15*num_bs1+group_i*log(rr11)-LOG(2));
  beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.15*num_bs2+group_i*log(rr12)-LOG(2));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;

%else %if &type. eq rdiff0 %then %do;
alpha=1/&op.;
rr11=1.0*rr1;
rr12=1.0*rr1;
m_event1=exp(-0.5+0.15*num_bs1+group_i*log(rr11)-LOG(2));
beta1=m_event1*&op.;  
seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.15*num_bs2+group_i*log(rr12)-LOG(2));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;

%else %if &type. eq rdiff00 %then %do;
alpha=1/&op.;
rr11=1.0*rr1;
rr12=1.0*rr1;
rr13=1.0*rr1;
num_bs1=num_bs/2;
 m_event1=exp(-0.5+0.39*num_bs1+group_i*log(rr11));
  beta1=m_event1*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma1=beta1*rangam(seed,alpha);
num_event1=rand('POISSON',gamma1);
num_bs2=num_event1;
 m_event2=exp(-0.5+0.39*num_bs2+group_i*log(rr12));
  beta2=m_event2*&op.;  
  seed=int(100000*RAND('UNIFORM'))+15678;
gamma2=beta2*rangam(seed,alpha);
num_event2=rand('POISSON',gamma2);
num_event=num_event2+num_event1;
%end;
 run;  


proc sort data=event1_&m._&seq.;
by group;
run;


/*P-value for superiority test need to divide by 2 */
/*Estimate and 95% CI the same */
ODS SELECT NONE;
ODS listing;
ods output Estimates=est_group_&m._&seq.; 
PROC genmod DATA=event1_&m._&seq. ;
  CLASS group;
  MODEL num_event =group num_bs
  %if &type. eq rdiff0pois or &type. eq rdiff1pois or &type. eq rdiff2pois or &type. eq pois or &type. eq poismix1 or &type. eq poismix2 or &type. eq poismix3 %then %do;
  /DIST= Poisson link=log   offset=lrisk ;*TYPE3 WALD;
  %end;
  %else %do;
       /DIST= negbin link=log   offset=lrisk ;*TYPE3 WALD;
       %end;
  lsmeans group /  cl;     
 estimate 'test-active' group 1 -1  /exp;      
       run;
 %if &m. eq 1 %then %do;      
 data summary_&seq.;
   set est_group_&m._&seq.;      
where label eq 'Exp(test-active)';  
upper=LBetaUpperCL;
if upper lt 1 and upper ne . then ind_sup=1;
else if upper ge 1 then ind_sup=0;
marni=&mar_ni.;
if upper < marni and upper ne . then ind_ni=1;
else if upper ge marni then ind_ni=0;
order=1;
run;
%end;
%else  %if &m. ne 1 %then %do;      
 data summary_&seq.;
   set est_group_&m._&seq. summary_&seq.;      
where label eq 'Exp(test-active)';  
upper=LBetaUpperCL;
if upper lt 1 and upper ne . then ind_sup=1;
else if upper ge 1 then ind_sup=0;
marni=&mar_ni.;
if upper < marni and upper ne . then ind_ni=1;
else if upper ge marni then ind_ni=0;
order=1;
run;
%end;
%end;
proc sql noprint;
create table summary1_&seq. as 
  select sum(ind_ni)/sum(order) as pct_ni
	,sum(ind_sup)/sum(order) as pct_sup,label 
	from summary_&seq.
	where ind_ni ne . and ind_sup ne .
	group by label;
	quit;
%mend sim_binom2;
%sim_binom2(N=3000,K=1000, rr_trt=1.175,lambda_baseline=1.31,op=0.677,mar_ni=1.15,type=pois,seq=2); 

%sim_binom2(N=3000,K=1000, rr_trt=1,lambda_baseline=1.31,op=0.677,mar_ni=1.15,type=pois,seq=1); 