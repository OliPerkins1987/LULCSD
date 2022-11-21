
#-------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------
# Assumptions
#-------------------------------------------------------------------
# Obviously this is a simplified model, but some particular simplifications worth remembering...
# We assume the only source of new trees is 'new planting' and 'restocking' which can only be obtained from nursery stocks
# We assume that CS schemes are exclusively tree planting
#-------------------------------------------------------------------
# UNITS
#-------------------------------------------------------------------
# Unit standardisation assumes data (and output variables) use the following scaling:
# land area: x 1000 hectares
# schemes availables: x1
# stocks: x 1,000,000 plants
# production: x 1 tonnes
#-------------------------------------------------------------------
uptake.of.cs.schemes <- function(	willingness, 
						schemes.available
						){
	# countryside stewardship schemes
	# willingness: probability of uptake (0 to 1): free parameter
	# schemes.available: number of schemes available: 
	uptake <- round(willingness * schemes.available)
	if(uptake<0)stop('uptake should not be negative')
	if(willingness<0)stop('willingness should not be negative')
	if(schemes.available<0)stop('schemes.available should not be negative')
return(uptake)}
#-------------------------------------------------------------------
area.of.land.in.cs.schemes <- function(	cs.uptake, 
							mean.area.per.agreement = 0.025, 
							maximum.land.area = 1e+04
							){
	# uptake: output of uptake.of.cs.schemes()
	# mean.area.per.agreement: 'area of land in cs agreements' divided by 'number of cs agreements', across 1991 to 2013, averages 25 hectares.
	# maximum.land: the upper limit of hectares available in england, estimated as 10 million hectares
	# simplest model is a linear relationship (y = kx) with an upper limit of maximum.land.area , but could use an asymptotic model if justified.
	area.in.cs.agreements <- cs.uptake * mean.area.per.agreement
	if(area.in.cs.agreements > maximum.land.area) area.in.cs.agreements <- maximum.land.area
	if(area.in.cs.agreements < 0) stop('should not be negative')
	if(mean.area.per.agreement < 0) stop('mean.area.per.agreement should not be negative')
	if(maximum.land.area < 0) stop('maximum.land.area should not be negative')
return(area.in.cs.agreements)}
#-------------------------------------------------------------------
nursery.stocks.partitioner <- function(	total.nursery.stocks, 
							fc.proportion, 
							fc.new.proportion,
							new.plants.per.area = 1.75
							){
	# function to partition the total nursery stocks into upper limits for each of the three demands:
	# fc.new.planting; fc.restocking; farm.woodland
	# (https://nhsforest.org/how-many-trees-can-be-planted-hectare)	
 	# splitting parameters: fc.proportion: 0 - 1 (remainder = farm.woodland), then split the fc component into fc.new.planting (remainder = fc.restocking)
	if(total.nursery.stocks < 0) stop('total.nursery.stocks should not be negative')
	if(fc.proportion < 0 | fc.proportion > 1) stop('fc.proportion must be between 0 and 1')
	if(fc.new.proportion < 0 | fc.new.proportion > 1) stop('fc.new.proportion must be between 0 and 1')
	fc.total <- total.nursery.stocks * fc.proportion
	fc.new.planting.nursery.stocks <- fc.total * fc.new.proportion
	fc.restocking.nursery.stocks <- fc.total * (1 - fc.new.proportion)
	farm.woodland.new.planting.and.restocking.nursery.stocks <- total.nursery.stocks * (1 - fc.proportion)
result <- data.frame(	fc.new.planting.area = fc.new.planting.nursery.stocks / new.plants.per.area,
				fc.restocking.area = fc.restocking.nursery.stocks / new.plants.per.area,
				farm.woodland.new.planting.and.restocking.area = farm.woodland.new.planting.and.restocking.nursery.stocks / new.plants.per.area)
return(result)}
#-------------------------------------------------------------------
change.in.forest.area <- function(	area.of.fc.new, 
						area.of.fc.restock, 
						forest.production
						){
	# model is based on change in area per iteration, not total area.
	if(area.of.fc.new < 0) stop('area.of.fc.new should not be negative')
	if(area.of.fc.restock < 0) stop('area.of.fc.restock should not be negative')
	change <- area.of.fc.new + area.of.fc.restock - forest.production
return(change)}
#-------------------------------------------------------------------
private.production.from.forests <- function(	total.forest, 
								green.tonne.forest.production, 
								area.per.thousand.green.tonne = 0.03
								){
	# maximum production cannot exceed total.forest
	# estimates of area.per.thousand.green.tonne suggest between 15 and 40 tonnes per hectare, see https://www.forest2market.com/blog/how-many-tons-of-wood-are-on-an-acre-of-land
	production.area <- green.tonne.forest.production * area.per.thousand.green.tonne
	if(production.area > total.forest) production.area <- total.forest
	if(production.area < 0) stop('production.area should not be negative')	
return(production.area)}
#-------------------------------------------------------------------
total.forest.area <- function(	forest.area.from.previous.iteration, 
						change.in.forest.area.previous.iteration
						){
	total <- forest.area.from.previous.iteration + change.in.forest.area.previous.iteration
	if(total < 0) stop('total forest area cannot be negative')
return(total)}
#-------------------------------------------------------------------
area.of.farm.woodland.felled <- function(	woodland.proportion.felled, 
							total.farm.wood
							){
	farm.woodland.felled <- woodland.proportion.felled * total.farm.wood
	if(farm.woodland.felled < 0) stop('farm.woodland.felled cannot be negative')
return(farm.woodland.felled)}
#-------------------------------------------------------------------
total.farm.woodland <- function(	farm.woodland.area.from.previous.iteration, 
						change.in.farm.woodland.area.previous.iteration
						){
	total <- farm.woodland.area.from.previous.iteration + change.in.farm.woodland.area.previous.iteration
	if(total < 0) total <- 0
return(total)}
#-------------------------------------------------------------------
farm.woodland.planting.and.restocking <- function(	intended.area.of.extra.farm.woodland.planting.and.restocking, # extra means more than commited already in CS scheme
									farm.woodland.new.planting.and.restocking.nursery.stocks,
									cs.uptake.area.of.land
									){
	# farm.woodland.new.planting.and.restocking.nursery.stocks merely provides an upper limit
	# area.of.land.in.cs.schemes merely provides a lower limit
	area.of.fw <- cs.uptake.area.of.land + intended.area.of.extra.farm.woodland.planting.and.restocking
	if(area.of.fw > farm.woodland.new.planting.and.restocking.nursery.stocks) area.of.fw <- farm.woodland.new.planting.and.restocking.nursery.stocks
	if(area.of.fw < 0) stop('area.of.fw cannot be negative')
	if(farm.woodland.new.planting.and.restocking.nursery.stocks < 0) stop('farm.woodland.new.planting.and.restocking.nursery.stocks cannot be negative')
return(area.of.fw)}
#-------------------------------------------------------------------
change.in.farm.woodland.area <- function(	farm.woodland.felled, 
							change.in.area.of.crops, 
							change.in.area.of.grassland,
							new.farm.woodland
							){
	change.in.fw <- new.farm.woodland - farm.woodland.felled - change.in.area.of.crops - change.in.area.of.grassland
return(change.in.fw)}
#-------------------------------------------------------------------

#-------------------------------------------------------------------	