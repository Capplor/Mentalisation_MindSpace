# Mixture Models Branch
This is the code to analyse the data of the experiment to test a mixture model as cognitive model for social categorisation.

- Scripts:
  - Main Models: Code to have run models and generate metrics.
  - Stan files are the models code, constrained are the hierarchical and general are the General priors ones.
    - Zero: Mixture model with independent components.
    - One: Mixture model where the dependency between the two beta distributions is the same for each cluster.
    - K: Mixture model where the dependency between the two beta distributions is cluster specific. 
- Data:
  - all_data_lists and w_pred.xlsx files are if you want to play around with the models without wanting to run main data.yesian way.


**Modelling choices**:

Beta distributions might not be the best choice of distribution for generalisability, but in this project we are not aiming for generalisability but to reproduce the participant response using their own beliefs.

Imagine an action as a Bernoulli trial (e.g., robbing a bank), and a serie of similar actions ( or semantically related, e.g., pick-pocketing, stealing a wallet, returning a wallet) as a binomial distribution which parameter determins the likelihood of doing the action or not.

The beta distribution is therefore the natural candidate to use as a prior for this binomial distribution and represents ones belief about the common concept amongst these similar behaviours (a dimension ~ here personality like - dishonesty/honsty ~). Psychologically is plausible as it is bounded, much like personality dimensions (always honest vs always dishonest) and can capture the nuance of reality, being able to be flat as well as ambivalent.
