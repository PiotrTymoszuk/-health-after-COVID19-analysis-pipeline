# A mother script executing the particular analyses

# tools -----

  c('./tools/sys_tools.R', 
    './tools/modeling_tools.R', 
    './tools/cov_project_tools.R', 
    './tools/cov_project_globals.R', 
    './tools/counting_tools.R', 
    './tools/clust_tools2.R') %>% 
  walk(source)

# executable scripts

  c('./analysis scripts/disease_perception_course.R', ## disease perception, % individuals with symptoms
    './analysis scripts/disease_perception_course_PASC.R', ## disease perception, % individuals with symptoms, the PASC subset
    './analysis scripts/symptom_course.R', ## modeling of individual symptom count trajectories
    './analysis scripts/symptom_frequencies.R', ## symptom frequencies in acute, long COVID and PASC
    './analysis scripts/symptom_coregulation.R', ## definition of symptom phenotypes
    './analysis scripts/participant_clustering.R', ## long COVID and PASC participant clusters
    './analysis scripts/participant_cluster_counts.R', ## symptom counts in the participant clusters
    './analysis scripts/participant_cluster_symptoms.R', ## symptoms in the participant clusters 
    './analysis scripts/participant_cluster_clinics.R') %>% ## clinical and demographic characteristic of the participant clusters
    walk(source)
  
# END -----