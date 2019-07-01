Input files and scripts necessary to produce Figure S11, the frequency distribution (GDS) among meadows of the Gullmarsford and Marstrand areas separated by distance classes. 

Files: 
- The script "asymetric_genetic_dist_function.R" creates the functions to calculate a full assymetric genetic distance matrix between two populations, using Rozenfeld or SAD as genetic distance metrics.
- The script "calculation_pairwise_geographic_distaces.R" calculates the pairwise geographic distances between sampling sites of Gullmarsfjord and Marstrand sites using marmap and fossil packages in R.
- The script "Plotting_Fig_S11.R" splits pairwise populations of Marstrand and Gullmarsfjord areas by geographical distance, and it uses the functions defined in "asymetric_genetic_dist_function.R"  to calculate the pairwise genetic distances between pairs of populations. Then, by summing up all the frequency distributions (GDS) of pairs of populations per area and geographical distance class, it plots the GDS separated by distance classes.

NOTE: The sampling site´s naming has been changed in respect to the manuscript in the following way: 
- For Gullmarsfjord: G1: G-AF, G2: G-AS, G3: G-BB, G4: G-BV, G5: G-RX, G6: G-SK, G7: G-NB, G8: G-SN, G9: G-T, G10: G-GB, G11: G-SG
- For Marstrand: M1: K-K, M2: K-NG, M3: K-SK, M4: K-OK, M5: K-BK, M6: K-LD, M7: K-RT, M8: K-NI, M9: K-ON, M10: K-KR, M11: K-SO