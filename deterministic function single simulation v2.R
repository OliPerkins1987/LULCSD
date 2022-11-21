#----------------------------------------------------------------------------------------------
# single simulation using model v2
#----------------------------------------------------------------------------------------------
annual.iteration.v2 <- function(	
	willingness,
	schemes.available,
	mean.area.per.agreement,
	maximum.land.area,
	total.nursery.stocks,
	fc.proportion,
	fc.new.proportion,
	new.plants.per.area,
	forest.area.from.previous.iteration,
	change.in.forest.previous.iteration,
	farm.woodland.area.from.previous.iteration,
	change.in.farm.woodland.previous.iteration,
	green.tonne.forest.production,
	area.per.green.tonne,
	woodland.proportion.felled,
	intended.area.of.extra.farm.woodland.planting.and.restocking,
	change.in.area.of.crops,
	change.in.area.of.grassland
	){

	cs.uptake <- uptake.of.cs.schemes(	willingness, 
							schemes.available
							)

	cs.uptake.area.of.land <- area.of.land.in.cs.schemes(	cs.uptake, 
										mean.area.per.agreement, 
										maximum.land.area
										)

	NSP <- nursery.stocks.partitioner(	total.nursery.stocks, 
							fc.proportion, 
							fc.new.proportion,
							new.plants.per.area
							)
	area.of.fc.new <- NSP$fc.new.planting.area
	area.of.fc.restock <- NSP$fc.restocking.area
	farm.woodland.new.planting.and.restocking.nursery.stocks <- NSP$farm.woodland.new.planting.and.restocking.area

	total.forest <- total.forest.area(	forest.area.from.previous.iteration, 
							change.in.forest.previous.iteration
							)

	total.farm.wood <- total.farm.woodland(	farm.woodland.area.from.previous.iteration, 
								change.in.farm.woodland.previous.iteration
								)

	forest.production <- private.production.from.forests(	total.forest, 
										green.tonne.forest.production, 
										area.per.green.tonne
										)

	farm.woodland.felled <- area.of.farm.woodland.felled(	woodland.proportion.felled, 
										total.farm.wood
										)

	change.in.forest <- change.in.forest.area(	area.of.fc.new, 
									area.of.fc.restock, 
									forest.production
									)

	new.farm.woodland <- farm.woodland.planting.and.restocking(	intended.area.of.extra.farm.woodland.planting.and.restocking, 
											farm.woodland.new.planting.and.restocking.nursery.stocks,
											cs.uptake.area.of.land
											)

	change.in.farm.woodland <- change.in.farm.woodland.area(	farm.woodland.felled, 
											change.in.area.of.crops, 
											change.in.area.of.grassland,
											new.farm.woodland
											)

	change.in.area.of.trees <- change.in.forest + change.in.farm.woodland

result <- data.frame(	
	cs.uptake,
	cs.uptake.area.of.land,
	area.of.fc.new,
	area.of.fc.restock,
	farm.woodland.new.planting.and.restocking.nursery.stocks,
	total.forest,
	total.farm.wood,
	forest.production,
	farm.woodland.felled,
	change.in.forest,
	new.farm.woodland,
	change.in.farm.woodland,
	change.in.area.of.trees
	)
return(result)}	
#----------------------------------------------------------------------------------------------
single.simulation.v2 <- function(data, parameters){

	N <- length(data$year)

	# initialise an empty output with the correct column names
	x <- annual.iteration.v2(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
	output <- as.data.frame(matrix(,N,length(x))); names(output) <- names(x)
	iteration <- NULL

	# iterate through all years
	for(n in 1:N){

		if(n == 1){
			forest.area.from.previous.iteration = parameters$forest.area.initialisation
			change.in.forest.previous.iteration = 0
			farm.woodland.area.from.previous.iteration = parameters$forest.area.initialisation
			change.in.farm.woodland.previous.iteration = 0
			}
		if(n != 1){
			forest.area.from.previous.iteration = output$total.forest[n-1]
			change.in.forest.previous.iteration = output$change.in.forest[n-1]
			farm.woodland.area.from.previous.iteration = output$total.farm.wood[n-1]
			change.in.farm.woodland.previous.iteration = output$change.in.farm.woodland[n-1]
			}

		iteration <- annual.iteration.v2(	
			willingness =  parameters$willingness,
			schemes.available = parameters$schemes.available,
			mean.area.per.agreement = parameters$mean.area.per.agreement,
			maximum.land.area = parameters$maximum.land.area,
			total.nursery.stocks = data$Total.nursery.stock.sales..GB.million.plants.[n],
			fc.proportion = parameters$fc.proportion,
			fc.new.proportion = parameters$fc.new.proportion,
			new.plants.per.area = parameters$new.plants.per.area,
			forest.area.from.previous.iteration,
			change.in.forest.previous.iteration,
			farm.woodland.area.from.previous.iteration,
			change.in.farm.woodland.previous.iteration,
			green.tonne.forest.production = data$Private.production..thousand.green.tonnes.[n],
			area.per.green.tonne = parameters$area.per.green.tonne,
			woodland.proportion.felled = parameters$woodland.proportion.felled,
			intended.area.of.extra.farm.woodland.planting.and.restocking = parameters$intended.area.of.extra.farm.woodland.planting.and.restocking,
			change.in.area.of.crops = data$Crops.area..thousand.hectares.[n],
			change.in.area.of.grassland = data$Grassland.area.change..thousand.hectares.[n]
			)
		output[n,] <- iteration
		}
return(output)}
#----------------------------------------------------------------------------------------------
distance.v2 <- function(sim, raw){
  d1 <- sum((sim$area.of.fc.new - raw$Area.of.FC.new.planting..thousand.hectares.)^2,na.rm=T)
  d2 <- sum((sim$area.of.fc.restock - raw$Area.of.FC.restocking..thousand.hectares.)^2,na.rm=T)
  d3 <- sum((sim$total.forest - raw$Area.of.forest..thousand.hectares.)^2,na.rm=T)
  d4 <- sum((sim$total.farm.wood - raw$Area.of.farm.woodland..thousand.hectares.)^2,na.rm=T)
  d5 <- sum((sim$forest.production - raw$Private.production..thousand.green.tonnes.)^2,na.rm=T)
  res <- log(d1) + log(d2) + log(d3) + log(d4) + log(d5)
  return(res)}