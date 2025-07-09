# MindSpace: New Model of Social Categorisation in Mentalisation
This is the code to analyse the data of the experiment to test a mixture model as cognitive model for social categorisation.

This repo is made up of four branches:

#### Main Branch: 
- Scripts:
     - **Requirements**: Packages and Functions needed for the project.
     - **Main Data**: Code to have the cleaned dataset and variables to input. 
     - **Graph Simulator**: Code to generate the poster graph representing participants elicited beliefs.
     - **Demographics**: Code to run demographics.
- Data:
     - demo.xlsx demographics file ready to be analysed with demographics script,
     - study1.xlsx necessary to run Main Data. Experiment datafile.
     - shapes, theta, all_data_lists and final_Cluster xlsx files are for the Graph simulator, if you want to play around with the graphs without wanting to run main analyses

#### Full Dataset Branch: 
- Scripts:
     - **Main Models**: Code to have run models and generate metrics. 
     - Stan files are the models code, **constrained** are the **hierarchical** and *general* are the *General priors* ones.
- Data:
     - all_data_lists and w_pred.xlsx files are if you want to play around with the models without wanting to run main data.
  
#### Multinomial Regression Branch: 
- Scripts:
     - **Main Multinomial Regression**: Code to have run frequentist and Bayesian Analyses. 
- Data:
     - total_wide.xlsx files are if you want to play around with the models without wanting to run main data.

#### Alternative Dataset Branch: 
- TBA

### 📄 Poster: **Who am I playing with?** Exploring a New Model of Social Categorisation in Mentalisation
**Description**:  
#### *Poster Presented at CogSci2025*
This academic poster titled " **Who am I playing with?** Exploring a New Model of Social Categorisation in Mentalisation" presents a study on how people understand what kind of person the other is through trait-based categorisation. The research uses Minecraft players and players' "stereotypes" to elicit their beliefs and test probabilistic models (e.g., mixture models) to formalize social inference. Visuals include trait clusters, bar plots, and a QR code to references. 

[Download PDF Poster](https://github.com/user-attachments/files/21144747/CSS.Poster.pdf)
<img src="https://github.com/user-attachments/assets/ff9a2664-2161-463d-bcb2-b085993f95a7" 
     alt="Academic poster titled 'MIND SPACE – Trait-Based Mentalisation and Categorisation in Minecraft'. 
     The poster explores how people infer others’ beliefs and intentions through social categorisation, using 
     trait dimensions like extraversion and conscientiousness. It uses probabilistic mixture models to capture 
     how participants group behavior into fuzzy categories. The study uses Minecraft as an experimental environment 
     where participants rate avatars across trait sliders in multiple trials. Results show that hierarchical mixture 
     models outperform general models in predicting participant categorisation and the no correlations model seem to
     be the best fitting model. Key metrics include log-loss, RMSE, and KL divergence. 
     Visualizations include trait distributions, bar charts, model comparison tables, and cluster diagrams. 
     Discussion highlights alignment between participants’ beliefs and model predictions. A QR code links 
     to references." />






