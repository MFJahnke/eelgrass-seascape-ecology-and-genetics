
Input files and scripts used to plot Figures S8 and S9, the real within-meadow GDS per meadow with the simulated population after simulating clonal growth, geitonogamuos self-fertilization and outcrossing for 10 generations.

Input files:
- Genetic distances within meadow, right input format for gstudio package:
		Mars_withclones.csv
		Gullm_withclones.csv
- Script "functions_selfing_outcr_randmat.R" contains the functions to simulate selfing, outcrossing and random mating on a population over several generations by applying some functions of the gstudio package. It also applies these functions to the Gullmarsfjord and Marstrand populations over 10 generations.
- Script "Plotting_Figures_S8_S9.R" splits the dfs created by the previous script and converts them to the correct input format for RClone package, which is used to calculate the Rozenfeld and SAD genetic distance metrics. It also plots the real GDS and the simulated ones.

NOTE: The sampling site´s naming has been changed in respect to the manuscript in the following way: 
- For Gullmarsfjord: G1: G-AF, G2: G-AS, G3: G-BB, G4: G-BV, G5: G-RX, G6: G-SK, G7: G-NB, G8: G-SN, G9: G-T, G10: G-GB, G11: G-SG
- For Marstrand: M1: K-K, M2: K-NG, M3: K-SK, M4: K-OK, M5: K-BK, M6: K-LD, M7: K-RT, M8: K-NI, M9: K-ON, M10: K-KR, M11: K-SO