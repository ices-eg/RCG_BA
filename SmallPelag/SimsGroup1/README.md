work of sim subgroup 1

Planned work
Tools: fishPiSim (https://github.com/ices-tools-dev/FishPi2/tree/master/WP3) and FishPi2WP3_BioSims (https://github.com/ices-eg/wk_WKBIOPTIM3/tree/master/FishPi2WP3_BioSims). The latter was developed under fishpi2 and has the same core functions as the fishPiSim tool, but has been defined under WKBIOPTIM3. 
Data: Logbook data - combined & RDB sample data
Plan of attack
Coverage at a regional level - logbook simulation
The idea would be to use the fishPiSim tool to look at this
Get familiar with the fishPiSim tool - there is a test script in the package
Identify strata. We should have a lot from last year
Analysis - Is it possible to uniquely identify the vessel doing the rare stuff - in e.g. time?
Identify number of samples to play with
Effort allocation between countries - keep what we have or allocate based on something. If the latter what? This depends a bit on the purpose of this study
Run simulations - equal between strata
Test different scenarios
Simple random - no stratification
Stratify by country
Include a time strata, so this will reflect the
Discuss shortcomings of the method
Identify relevant outputs to be developed - probably in the future
Apply biology to the population data
1st step would be to apply the sampled length distributions to the population and then run the simulations. 2nd step would be to run the same simulations with the ages instead. I see two approaches 1) simply picking samples within a stratum or 2) modeling - the first would be to randomly assign biologi to trips within an appropriated stratum in time, space and selectivity (metier (gear / mesh size)). I can’t really get my head around the latter at the moment, so for this WK I would  go for the first suggestion. If anyone has any pros and cons for the two approaches then these would be very much appreciated. Ideas on how to model would also be great
Apply lengths to the population data
Identify is there are any weird length distributions which could indicate that the sample is non-representative
Get a quick overview of the sampling - 1st year report from this case study
Identify relevant strata - where do we need to combine samples?
Analysis - total landings / total number of trips vs. number of sampled trips per species, stock, year, quarter, subdivision and metier
Analysis - AIC / PCA / other method for identifying what parameters influence the length distribution (maybe expressed as the mean length) the most year, country, space, time and metier. This will support the selected strata and possible combining e.g. would it be ok to combine length distribution across years
Standardization of the samples - same number of measured fish (length) and at the same level (trip) - Do we need the same number of fish in the samples?
Develop a script that randomly apply the length distribution to the logbook trips and export in the format needed
Run the simulation
Think about how to select
Discuss shortcomings of the method used
Identify relevant outputs to be developed - probably in the future
Applying ages to the population data
Get a quick overview of the age sampling (directly / length stratified) - 1st year report from this case study
Identify methods. Simple ALK’s or modelling? For now just I would go for simple ALK’s
Pros and cons 
Identify relevant strata
Analysis - number of age readings per strata
Develop a script that randomly apply the ages
Run the simulation - the same setup as for the lengths
Discuss shortcomings of the method used
Identify relevant outputs to be developed - probably in the future
Number of fish to be worked up
At the moment you can only play with number of trips (+ harbour days and number of trips within) in the fishPi2 tools, so this would require some thoughts.It will require that the tools are adapted to include this level of sampling. There are quite a lot of WKBIOPTIM tools looking at this, but most of them operates at the sample level and do not scale to the population.
Effectiveness of ALK vs sampling directly for age
Extremely nice topic, but how this should be tackled will require more thoughts.    
