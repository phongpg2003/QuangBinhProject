/***
* Name: FloddCiotat
* Author: franck sfiligoï taillandier
* Description: 
* Tags: Tag1, Tag2, TagN
***/
model FloodCiotat

//***********************************************************************************************************
//***************************  GLOBAL **********************************************************************
//***********************************************************************************************************
global {

//***************************  VARIABLES **********************************************************************

//file<geometry> osmfile <- osm_file("../includes/LaCiotat.osm.pbf",["waterway"::[], "highway"::[], "building"::[]]);
//file mnt_file <- grid_file("../includes/LCRed4_5m.asc");
	grid_file mnt_file <- grid_file("../includes/DEM.tif");
	
	//file my_data_flood_file <- csv_file("../includes/data_flood2.csv", ",");
	file roads_shape_file <- file("../includes/road.shp");
	file buildings_shape_file <- file("../includes/buildings.shp");
	
	file waterways_shape_file <- file("../includes/river.shp");
	
	shape_file population_shape_file <- shape_file("../includes/city_environment/population.shp");
	geometry shape <- envelope(mnt_file);
	graph road_network;
	map<road, float> current_weights;
	river river_origin;
	river river_ending;
	float initial_water_level;
	int increment;
	matrix data_flood;
	float diffusion_rate;
	list<building> obstacles;
	float obstacle_height <- 0.0;
	float water_speed_input;
	float ratio_received_water;
	float cell_area;

	float canal_depth <- 0 #m;
	
	float max_distance_to_river <- 300 #m;
	bool display_grid <- false parameter: true;
	bool end_simul<-false;
	int leaving_people <- 0;
	int dead_people_inside <- 0;
	int dead_people_outside <- 0;
	float fear_coeff<-0.05;
	
	list<cell> active_cells;
	float proba_info <- 0.2;
	list<cell> escape_cells;
	
	float strength_information_decrement <- 0.1;
	
	list<road> safe_roads ;
	
	
	float cumul_water_enter;
			
	float proba_know_rules <- 0.1;
	float river_broad <- 1 #m;
	float river_depth <- 1 #m;
	float coeff_enter_canal<-0.5;

	int nb_of_people <- 1000;
	
	string scenario <- "S1" among: ["S1","S2","S3","S4"];
	string type_explo <- "normal" among: ["normal", "stochasticity"];
	
	map<date, list<float>> data_map;

//***************************  PREDICAT et EMOTIONS  ********************************************************
	//beliefs*******************************************************
	predicate house_flooded <- new_predicate("house_flooded");
	predicate water_at_my_door <- new_predicate("water_at_my_door");
	predicate evacuation_is_possible <- new_predicate("evacuation_is_possible");
	//predicate want_to_inform <- new_predicate("want_to_inform");
	//predicate want_to_have_information <- new_predicate("want_to_have_information");
	predicate water_is_coming <- new_predicate("water_is_coming");
	predicate need_to_go_outside <- new_predicate("need_to_go_outside");
	predicate someone_to_help <- new_predicate("someone_to_help");
	predicate vulnerable_properties <- new_predicate("vulnerable_properties");
	predicate vulnerable_car <- new_predicate("vulnerable_car");
	predicate vulnerable_building <- new_predicate("vulnerable_building");
	predicate energy_is_on <- new_predicate("energy_is_on");
	predicate energy_is_dangerous <- new_predicate("energy_is_dangerous");
	predicate crisis_event <- new_predicate("crisis_event");

	//intention
	predicate drain_off <- new_predicate("drain_off");
	predicate evacuate <- new_predicate("evacuate");
	predicate inform <- new_predicate("inform");
	predicate outside <- new_predicate("outside");
	predicate upstairs <- new_predicate("upstairs");
	predicate inquire <- new_predicate("inquire");
	predicate protect_car <- new_predicate("protect_car");
	predicate protect_properties <- new_predicate("protect_properties");
	predicate turn_off <- new_predicate("turn_off");
	predicate protect <- new_predicate("protect");
	predicate weather_strip <- new_predicate("weather_strip");
	predicate wait <- new_predicate("wait");
	predicate apply_instructions <- new_predicate("apply_instructions");
	
	int nb_waiting update: length(people);
	int nb_drain_off update: 0;
	int nb_evacuate update: 0;
	int nb_inform update: 0;
	int nb_outside update: 0;
	int nb_upstairs update: 0;
	int nb_inquire update: 0;
	int nb_protect_car update: 0;
	int nb_protect_properties update: 0;
	int nb_turn_off update: 0;
	int nb_weather_strip update: 0;
	
	//emotion
	emotion fear <- new_emotion("fear");
	//***************************  INIT **********************************************************************
	init {
		do load_parameters;
		step <- 1 #mn; //step time of the simulation (pas de 5 min)
		water_speed_input <- 10 #m / #s;
		ratio_received_water <- 2.0;
		diffusion_rate <- min([1.0,water_speed_input * step / one_of(cell).shape.area]);
		write ("diffusuon rate : "+diffusion_rate);
		create building from: buildings_shape_file {
			if not (self overlaps world) {
				do die;
			}

		}

		create river from: waterways_shape_file {
			if not (self overlaps world) {
				do die;
			}

		}

		create road from: clean_network(roads_shape_file.contents,0.0, true, true) {
			if not (self overlaps world) {
				do die;
			}

		}

		file data <- csv_file("../includes/Test.csv", ",");
			matrix matrix_data <- matrix(data);
			loop i from: 1 to: matrix_data.rows - 1 {
				list<string> d <- matrix_data[0, i] split_with " ";
				list<string> dd <- d[0] split_with "/";
				list<string> dt <- d[1] split_with ":";
				date date_gama <- date([int(dd[2]), int(dd[1]), int(dd[0]), int(dt[0]), int(dt[1]), 0]);
				data_map[date_gama] <-
				[float(matrix_data[5, i]), float(matrix_data[25, i]), float(matrix_data[26, i]), float(matrix_data[28, i]), float(matrix_data[29, i]), float(matrix_data[30, i])];
			}

			starting_date <- first(data_map.keys);
		ask cell {
			water_flowing_altitude <- altitude;
			cell_area<-shape.area;
		}
 		
		//data_flood <- matrix(my_data_flood_file);
		river_origin <- (river with_min_of (each.location.x));
		river_ending <- (river with_max_of (each.location.x));
		ask river {
			my_cells <- cell overlapping self;
			ask my_cells {
				water_flowing_altitude <- altitude - river_depth;
			}

			altitude <- (my_cells mean_of (each.grid_value));
			my_location <- location + point(0, 0, altitude + 1 #m);
			cell_origin <- (my_cells with_max_of (each.grid_value));
			cell_destination <- (my_cells with_min_of (each.grid_value));
			river_length <- shape.perimeter;
			ask my_cells {
				water_height <- myself.river_height;
				is_river <- true;
			}

		}

		ask building {
			my_cells <- cell overlapping self;
			ask my_cells {
				add myself to: my_buildings;
				myself.my_neighbour_cells <- (myself.my_neighbour_cells + neighbors);
				if !is_river {
				}

			}

			my_neighbour_cells <- remove_duplicates(my_neighbour_cells);
			altitude <- (my_cells mean_of (each.grid_value));
			my_location <- location + point(0, 0, altitude + 1 #m);
		}

		ask road {
			my_cells <- cell overlapping self;
		}

		ask cell {
			if grid_value < 0 {
				is_sea <- true;
			}

			my_buildings <- remove_duplicates(my_buildings);
			escape_cell <- false;
			if grid_x < 10 {
				escape_cell <- true;
			}

		}

		escape_cells <- cell where each.escape_cell;
		ask river_ending {
			create canal {
				self.canal_cell_origin <- myself.cell_destination;
				self.canal_cell_destination<-(cell where each.is_sea) closest_to  myself.cell_destination;
				//self.canal_cell_destination <- one_of(cell where (each.name = "cell3587"));
				shape <- polyline([canal_cell_origin.location, canal_cell_destination.location]);
				my_cells <- cell overlapping self;
				ask my_cells {
					water_flowing_altitude <- altitude - canal_depth;
					is_canal <- true;
					if (self=myself.canal_cell_origin) {
						is_canal_origin<-true;
					}
					
				}

			}

		}

		geometry rivers <- union(river collect each.shape);
		geometry canals <- union(canal collect each.shape);
		using topology(world) {
			active_cells <- cell where (((each.location distance_to rivers) <= max_distance_to_river));
			active_cells <- active_cells + cell where (((each.location distance_to canals) <= max_distance_to_river));
			ask active_cells {
				is_active <- true;
			}

		}
		safe_roads <- road where empty(active_cells overlapping each);
	//	write "active cells:" + length(active_cells);
		ask building where ((each distance_to rivers) > max_distance_to_river and (each distance_to canals) > max_distance_to_river) {
			do die;
		}
		//ask road where ((each distance_to rivers) > max_distance_to_river) {do die;}
		road_network <- as_edge_graph(road);
		current_weights <- road as_map (each::each.shape.perimeter);
		/*ask building {
		 	create people {
				my_building<-myself;
				location<-my_building.location+point(0,0,my_building.height);
				if flip(proba_info) {do add_belief(water_is_coming);}
				water_height_danger<-my_building.height-1.5#m;
			}
		}*/
		create people  number: nb_of_people {
				location <- any_location_in(one_of(building));
		
			
		}//from: population_shape_file;
		ask people {
			obedience <- rnd(1.0);
			list<building> bds <- building overlapping self;
			if empty(bds) {
				do die;
			} else {
				my_building <- first(bds);
				location <- location + {0, 0, my_building.bd_height};
				if flip(proba_info) {
					do add_belief(water_is_coming);
				}

				water_height_danger <- my_building.bd_height - 1.5 #m;
			}

			current_stair <- rnd(my_building.nb_stairs);
			have_car <- false;
			if flip(0.8) {
				have_car <- true;
				create car {
					my_owner <- myself;
					myself.my_car <- self;
					location <- myself.location;
					float dist <- 100 #m;
					list<road> roads_neigh <- (road at_distance dist);
					loop while: empty(roads_neigh) {
						dist <- dist + 50;
						roads_neigh <- (road at_distance dist);
					}

					road a_road <- roads_neigh[rnd_choice(roads_neigh collect each.shape.perimeter)];
					location <- any_location_in(a_road);
					
					my_owner.heading <- first(a_road.shape.points) towards last(a_road.shape.points);
				}
				if (cell(my_car.location) in active_cells) {
					do add_belief(vulnerable_car);
				}

			}
			
			if flip(0.3) {
				do add_belief(energy_is_dangerous);
			}
			do add_belief(crisis_event);
			do add_belief(energy_is_on);
			do add_belief(evacuation_is_possible);
		
		/*	string 	header <- "id,seed,proba_know_rules,river_broad,river_depth,coeff_enter_canal,leaving_people"+
			",injuried_people_inside,injuried_people_outside,dead_people_inside,dead_people_outside,"
			 +"flooded_car,flooded_building,average_building_state";
		
		
			save header to: "results.csv" type:text rewrite: true; */
		}
		nb_waiting <- length(people);
		write ("nb people : "+length(people));
				ask river_origin {
			ask cell_origin {
			//	water_height <- 10#m;
			}
			}
		
		write ("nb building : " +length(building));
		write ("nb people : " +length(people));		 
	}
	
	//***************************  FIN INIT     **********************************************************************

	action load_parameters {
		switch scenario {
			match "S1" {
				//scenario 1
				proba_know_rules <- 0.1;
				river_broad <- 1 #m;
				river_depth <- 1 #m;
				coeff_enter_canal<-0.5;
			}
			match "S2" {
				//scenario 2
				proba_know_rules <- 1.0;
				river_broad <- 1 #m;
				river_depth <- 1 #m;
				coeff_enter_canal<-0.5;
			}
			match "S3" {
				//scenario 3
				proba_know_rules <- 0.1;
				river_broad <- 2 #m;
				river_depth <- 2 #m;
				coeff_enter_canal<-1.0;
			}
			match "S4" {
				//scenario 4
				proba_know_rules <- 1.0;
				river_broad <- 2 #m;
				river_depth <- 2 #m;
				coeff_enter_canal<-1.0;
			}
		}
	
	
	
		
	
	}
	//***************************  REFLEX GLOBAL **********************************************************************
	reflex update_flood when: every(#hour) {
		write sample(data_flood.rows);
		increment <- increment + 1;
		int injuried_people_inside<-length(people where (each.injuried_inside));
		int injuried_people_outside<-length(people where (each.injuried_outside));
		int flooded_building<-length(building where (each.serious_flood));
		float average_building_state<-mean(building collect each.state);
		
		
		int flooded_car<-length(car where (each.domaged));
		string 	results <- ""+ 
		int(self)+"," + seed+","+scenario+","+ proba_know_rules + ","+river_broad+","+
		river_depth+","+coeff_enter_canal+","+ cycle + ","+
		leaving_people+"," +injuried_people_inside+","+injuried_people_outside+","+dead_people_inside+","+
		dead_people_outside+"," +flooded_car+"," +flooded_building+","+average_building_state;
		
		
		save results to: "results_" + type_explo+ "_" + scenario+".csv" type:text rewrite: false;
		
		if (increment = data_flood.rows) {
			write ("*************************");
			write ("number of evacuated people : " +leaving_people);
			write ("number of injuried people inside : " +injuried_people_inside);
			write ("number of injuried people outside : " +injuried_people_outside);
			write ("number of dead people in building: " +dead_people_inside);
			write ("number of dead people outside: " +dead_people_outside);
			write ("number of flooded car : " +flooded_car);
			write ("number of flooded building: " +flooded_building);
			write ("average building state: " +average_building_state);	
			
			end_simul<-true;
		//	do pause;
		}
	
	}

	//Reflex to flow the water according to the altitute and the obstacle
	reflex flowing when: increment < (data_flood.rows) {
	//	float debit_water <- float(data_flood[0, increment]) #m3 / #h;
		float debit_water <- float(data_flood[0, increment]);
	//	initial_water_level <- debit_water / water_speed_input / river_broad * ratio_received_water*step/1#h;
		initial_water_level <- debit_water / cell_area * ratio_received_water*step/1#h;
		
		
		ask river_origin {
			ask cell_origin {
				cumul_water_enter<-cumul_water_enter+initial_water_level;
				water_height <-initial_water_level;
		//		write("*****************");
		//		write("initial level :"+ initial_water_level); 
		//		write("debit_water :"+ debit_water); 
		//		write ("area cell : "+cell_area#m2);
		//		write ("step : "+step);
		//		write("initial level cumul :"+ cumul_water_enter); 
			}

			ask (active_cells sort_by ((each.water_flowing_altitude + each.water_height + each.obstacle_height))) {
				already <- false;
				do flow;
			}

		}
		float max_wh_bd <- max(building collect each.water_height);
		float max_wh <- max(cell collect each.water_height);

		
	//	float sum_wh <- sum(cell collect each.water_height);
	//	write ("max eau :"+max_wh );
	//	write ("sum eau :"+sum_wh );
	}

	//Reflex to update the color of the cell
	reflex update_cell_color when: increment < (data_flood.rows) {
		ask active_cells {
			do update_color;
		}

	}

	reflex update_road {
		road_network <- as_edge_graph(road where each.usable);
		current_weights <- road as_map (each::each.shape.perimeter / each.speed_coeff);
	}

}
//***************************  FIN GLOBAL **********************************************************************


//***********************************************************************************************************
//***************************  ROUTE    **********************************************************************
//***********************************************************************************************************
species road {
	rgb color <- #darkslategrey;
	string type;
	int val_water;
	float cell_water_max;
	list<cell> my_cells;
	bool usable <- true;
	float speed_coeff <- 1.0 update:  1.0 / (1 + cell_water_max)  min: 0.01;

	reflex update_flood {
		cell_water_max <- max(my_cells collect each.water_height);
		usable <- true;
		if cell_water_max > 20 #cm {
			usable <- false;
		}

		val_water <- max([0, min([255, int(255 * (1 - (cell_water_max / 3.0)))])]);
		color <- rgb([255, val_water, 0]);
	}

	aspect default {
		draw shape color: color;
	}

}

//***********************************************************************************************************
//***************************  RIVER    **********************************************************************
//***********************************************************************************************************
species river {
	rgb color <- #blue;
	string type;
	cell cell_origin;
	cell cell_destination;
	list<cell> my_cells;
	float river_height <- 0 #m;
	float altitude;
	point my_location;
	float river_length;

	aspect default {
		draw shape color: color at: (my_location);
	}

}

//***********************************************************************************************************
//***************************  CANAL **********************************************************************
//***********************************************************************************************************
species canal {
	list<cell> my_cells;
	float depth;
	cell canal_cell_origin;
	cell canal_cell_destination;

	aspect default {
		draw shape color: #springgreen;
	}

}

//***********************************************************************************************************
//***************************  BUILDING **********************************************************************
//***********************************************************************************************************
species building {
	string type;
	list<cell> my_cells;
	list<cell> my_neighbour_cells;
	float altitude;
	float impermeability <- (5+rnd(5)) / 10; //1: impermeable, 0:prend toute l'eau
	float water_height <- 0.0;
	float water_evacuation <- 1 #m3 / #mn;
	point my_location;
	float bd_height <- rnd(3,10) #m ;
	float state <- 1.0; //entre 0 et 1
	float vulnerability <- rnd(100) / 100; //entre 0 et 1 (très vulnérable)
	bool is_water;
	rgb my_color <- #grey;
	bool nrj_on <- true;
	int nb_stairs min: 0 <- round(bd_height / 3.0) - 1; // A vérifier !
	bool serious_flood<-false; 
	
	reflex update_water {
		int val_water <- 0;
		float cell_water_max;
		cell_water_max <- max(my_cells collect each.water_height);
		water_height <- max([0,with_precision(max(water_height - (water_evacuation / shape.area * step), cell_water_max * (1 - impermeability)), 2)]);
		val_water <- max([0, min([255, int(255 * (1 - (water_height / 3.0)))])]);
		my_color <- rgb([255, val_water, val_water]);
		state <- min([state,max([0, state - (water_height / 10 / (step / (1 #mn))) * vulnerability])]);
		if water_height>1.0 {serious_flood<-true;}
	}

	aspect default {
		draw shape color: my_color depth: bd_height;
	}

}

//***********************************************************************************************************
//***************************  CAR **********************************************************************
//***********************************************************************************************************
species car {
	people my_owner;
	cell my_cell;
	float state <- 1.0; //1.0 bien à 0: toute cassée
	point my_location;
	rgb my_color <- #green;
	bool domaged<-false;
	float problem_water_height<-(10+rnd(20))#cm;
	bool usable<-true;
	reflex update_state {
		my_cell<-one_of(cell overlapping self);
		if my_cell=nil {my_cell<-cell closest_to(self);}
		state <- max([0, state - (my_cell.water_height-problem_water_height)*step / (1 #mn)]);
		if (state<0.9) {domaged<-true;}
		if (state<0.5) {usable<-false;}
		int val <- int(255 * state);
		my_color <- rgb(255 - val, val, 0.0);
		
		
	}
	aspect default {
		draw rectangle(5 #m, 3#m) depth: 4 rotate: my_owner.heading color: my_color ;
	}
	
	

}

//***********************************************************************************************************
//***************************  INSTITUTION **********************************************************************
//***********************************************************************************************************
species institution {
	
	
	
	
}

//***********************************************************************************************************
//***************************  DYKE **********************************************************************
//***********************************************************************************************************
species obstacle {
//height of the obstacle
	float height min: 0.0;
	//Color of the obstacle
	rgb color;
	//Pressure of the water
	float water_pressure update: compute_water_pressure();

	//List of cells concerned
	list<cell> cells_concerned;
	//List of cells in the neighbourhood 
	list<cell> cells_neighbours;

	//Action to compute the water pressure
	float compute_water_pressure {
	//If the obstacle doesn't have height, then there will be no pressure
		if (height = 0.0) {
			return 0.0;
		} else {
		//The leve of the water is equals to the maximul level of water in the neighbours cells
			float water_level <- cells_neighbours max_of (each.water_height);
			//Return the water pressure as the minimal value between 1 and the water level divided by the height
			return min([1.0, water_level / height]);
		}

	}

	action compute_height;

	aspect geometry {
		int val <- int(255 * water_pressure);
		color <- rgb(val, 255 - val, 0);
		draw shape color: color depth: height * 5 border: color;
	}

}

//Species dyke which is derivated from obstacle
species dyke parent: obstacle {
	int counter_wp <- 0;
	int breaking_threshold <- 24;

	//Action to represent the break of the dyke
	action break {
		do die;
	}

	//Reflex to break the dynamic of the water
	reflex breaking_dynamic {
		if (water_pressure = 1.0) {
			counter_wp <- counter_wp + 1;
			if (counter_wp > breaking_threshold) {
				do break;
			}

		} else {
			counter_wp <- 0;
		}

	}

}

//***********************************************************************************************************
//***************************  PEOPLE   **********************************************************************
//***********************************************************************************************************
species people skills: [moving] control: simple_bdi {
	building my_building;
	car my_car;
	bool have_car;
	int Age;
	string Sexe;
	string iris;
	string Couple;
	string CSP;
	int current_stair;
	bool know_rules <- flip(proba_know_rules);
	float health; //entre zero et 1
	point my_location;
	bool in_car <- false;
	bool inside<-true;
	bool injuried_inside<-false;
	bool injuried_outside<-false;
	
	float my_speed {
		if in_car {
			if my_car.usable {return 30 #km / #h;}
			if !my_car.usable {return 2 #km / #h;}
		} else {
			return 2 #km / #h;
		}

	}

	float water_height_perception <- 1 #cm;
	float water_height_problem <- (1 + rnd(10)) #cm;
	float water_height_danger <- (10 + rnd(100)) #cm;
	float water_height_danger_car <- (10 + rnd(60)) #cm;
	float water_height_danger_pied <- (10 + rnd(130)) #cm;
	
	
	float strength_drain_off <- rnd(0.5);
	float strength_evacuate <- rnd(1.0);
	float strength_inform <- rnd(-1.0, 0.5);
	float strength_upstairs <- rnd(1.0);
	float strength_inquire <- rnd(-1.0, 0.5);
	float strength_protect_car <- rnd(1.0);
	float strength_protect_properties <- rnd(1.0);
	float strength_protect_turn_off <- rnd(1.0);
	float strength_protect_weather_strip <- rnd(1.0);
	float strength_outside<-rnd(0.5);
		
	float danger_inside <- 0.0; //entre 0 et 1 (1 danger de mort imminente)
	float danger_outside <- 0.0; //entre 0 et 1 (1 danger de mort imminente)
	float proba_fear<-0.0;
	float proba_evacuation<-0.0;
	
	point current_target;
	point final_target;
	point random_target;
	building random_building;
	rgb my_color <- #mediumvioletred;
	bool use_emotions_architecture <- true; //we set this built-in variable to true to use the emotional process
	bool return_home <- false;

	

	//***************************  REFLEX DE PRISE D'INFO (plus rapide que perceive)  ********************************
	reflex inspect_near_water {
		bool water_cell_neighbour <- false;
		bool water_cell <- false;
		bool water_building <- false;
		do remove_belief(house_flooded);
		do remove_belief(water_at_my_door);
		danger_inside<-0.0;
		danger_outside<-0.0;
		
		if inside {
			float whp <- water_height_perception;
			ask my_building.my_cells {
				ask neighbors {
					if water_height >= whp {water_cell_neighbour <- true;}
				}
				if water_height >= whp {water_cell <- true;}
			}
	
			if my_building.water_height >= whp {
				water_building <- true;
				do add_belief(vulnerable_properties);	 
				if (my_building.water_height  >= water_height_problem) {
					do add_belief(vulnerable_building);
					proba_fear<-min([3*(step / 1 #mn)/100*fear_coeff,0.5]);			
				}		
			}
			
			if my_building.water_height >= water_height_danger {
				proba_fear<-min([5*(step / 1 #mn)/100*fear_coeff,0.5]);
				if (my_building.nrj_on) {danger_inside <- min([1.0, (my_building.water_height - whp) * step / 1 #mn]); //entre 0 et 1 (1 danger de mort imminente) 
				}
				if (!my_building.nrj_on) {danger_inside <- min([1.0, (my_building.water_height - water_height_danger) * step / 1 #mn]); //entre 0 et 1 (1 danger de mort imminente) 
				}
			if (my_building.water_height<(3*(current_stair+1))) {danger_inside<-0.0;}
			}	
			if water_building {	do add_belief(house_flooded);}
			if water_cell {	do add_belief(water_at_my_door);}
		}

		if !inside {
			cell my_current_cell<-one_of(cell overlapping self);
			float wh<-0.0;
			if my_current_cell!=nil {wh<-my_current_cell.water_height;}
			if in_car {danger_outside<-max([0,min([1.0, (wh-water_height_danger_car)* step / 1 #mn])]);
			//	if (wh>0.5#m) {write (name + " : je suis dans ma voiture en situation périlleuse");}
				
			}
			else {danger_outside<-max([0,min([1.0, (wh-water_height_danger_pied)* step / 1 #mn])]);}
		}
		if danger_outside >0 {injuried_outside<-true;}
		if danger_inside >0 {injuried_inside<-true;}
		
		
		
		if has_belief(water_is_coming) {
			proba_fear<-min([(step / 1 #mn)/100*fear_coeff,0.1]);
			proba_evacuation<-min([(step / 1 #mn)/100,0.5]);
		}
		if has_belief(water_at_my_door) {
			proba_fear<-min([2*(step / 1 #mn)/100*fear_coeff,0.2]);
			proba_evacuation<-min([10*(step / 1 #mn)/100,0.7]);
		}
		if has_belief(house_flooded) {
			proba_fear<-min([3*(step / 1 #mn)/100*fear_coeff,0.3]);
			proba_evacuation<-min([20*(step / 1 #mn)/100,1.0]);
		}
		
		
	
		
	}
	
	
	reflex test_proba when:every(30#mn) {
		if flip(proba_fear) {do add_emotion(fear);}		
		if flip(proba_evacuation) {do remove_belief(evacuation_is_possible);}	
		if flip(danger_outside) {
			dead_people_outside <- dead_people_outside + 1;
			do die;
		}
		if flip(danger_inside) {
			dead_people_inside <- dead_people_inside + 1;
			do die;
		}
		
	}
	
	reflex agenda when:every(30#mn) {
		//simule les activités quotidienne d'un individu
		if flip(0.1) {do add_belief(need_to_go_outside);}
		}
	
	//***************************  REGLES BDI ********************************************************
	//	
	
   law follow_rule_inquire when: know_rules and not has_belief(water_is_coming) new_obligation: inquire strength: 3.0 threshold: 0.1;
   law follow_rule_upstaire belief: crisis_event new_obligation: upstairs when:know_rules and (current_stair < my_building.nb_stairs) and house_flooded strength: 2.0  threshold: 0.2;
   law follow_rule_protect when: know_rules and has_belief(water_is_coming) and not has_belief(water_at_my_door) new_obligation: protect_properties  strength: 2.0  threshold: 0.2;
   law follow_rule_strip when: know_rules and has_belief(water_is_coming) and not has_belief(house_flooded) new_obligation: weather_strip  strength: 2.0  threshold: 0.2;
   law follow_rule_wait belief: crisis_event new_obligation: wait when:know_rules strength: 1.0  threshold: 0.5;
   
	rule belief: (house_flooded) when: not has_belief(water_at_my_door) new_desire: drain_off strength: strength_drain_off;
	rule belief: evacuation_is_possible emotion: fear new_desire: evacuate strength: strength_evacuate;
	rule belief: water_is_coming new_desire: inform strength: strength_inform;
//	rule when: (has_belief(need_to_go_outside) and not has_belief(water_is_coming)) or (has_belief(water_is_coming) and has_belief(someone_to_help)) new_desire: outside strength:20.0;
	rule when: (has_belief(need_to_go_outside) and not has_belief(water_is_coming)) new_desire: outside strength:strength_outside;
	rule when: has_belief(water_at_my_door) or has_belief(house_flooded) and (current_stair < my_building.nb_stairs) emotion: fear new_desire: upstairs strength: strength_upstairs;
	rule  when: not has_belief(water_is_coming) new_desire: inquire strength: strength_inquire;
	rule beliefs: [vulnerable_car, water_is_coming] new_desire: protect_car strength: strength_protect_car;
	rule beliefs: [vulnerable_properties, water_is_coming] new_desire: protect_properties strength: strength_protect_properties;
	rule beliefs: [energy_is_on, energy_is_dangerous, water_at_my_door] new_desire: turn_off strength: strength_protect_turn_off;
	rule beliefs: [water_is_coming, vulnerable_building] new_desire: weather_strip strength: strength_protect_weather_strip;
	

	//***************************  NORMS  ********************************************************
	norm instruction_wait obligation: wait{
	//	write name + " j'applique les règles de sécurité: j'attends"  ;
		 do remove_intention(wait, true); 
		
	}
	norm behaviour_inquire obligation: inquire {
	//	write name + " j'applique les règles de sécurité: je m'informe"  ;
		do inquiring_information;
		do remove_intention(inquire, true);
	}
	
	norm behaviour_upstairs obligation: upstairs {
	//	write name + " j'applique les règles de sécurité: je monte à l'etage"  ;
		do going_upstairs;
		do remove_intention(upstairs, true);
	}
	
	
	norm protect_properties obligation: protect_properties {
	//	write name + " j'applique les règles de sécurité: je monte à l'etage"  ;
		do protect_properties;
		do remove_intention(protect_properties, true);
	}
	
	norm weather_strip_house obligation: weather_strip {
	//	write name + " j'applique les règles de sécurité: je monte à l'etage"  ;
		do weather_strip_house;
		do remove_intention(weather_strip, true);
	}
	 
	
		//***************************  PLANS  ********************************************************
	
	
	/*  comportements à simuler :
	* gérer les meubles
	* calfeutrer
	* s'enfuir
	* Ne rien faire
	* prendre de l'info
	* diffuser de l'info
	* évacuer l'eau
	* couper électricité/gaz
	*/
	//OK
	plan drain_off_water intention: drain_off {
	//	write  name + (" je vais protéger ma maison");
		my_building.water_height <- max([0, my_building.water_height - 0.05 #m3 / #mn / my_building.shape.area * step]);
		do remove_intention(drain_off, true);
		nb_drain_off <- nb_drain_off+ 1;
		nb_waiting <- nb_waiting - 1;
	}
	
	//OK
	plan evacuate intention: evacuate {
		inside<-false;
		nb_evacuate <- nb_evacuate+ 1;
		nb_waiting <- nb_waiting - 1;
	//	write (name + " je dois fuir.");
		my_color <- #gold;
		speed <- my_speed();
		if (final_target = nil) {
			final_target <- (escape_cells with_min_of (each.location distance_to location)).location;
			if (have_car) {
				current_target <- my_car.location;
			} else {
				current_target <- final_target;
		//		write ("j'ai atteint ma voiture");
			}

		} else {
			do goto target: current_target on: road_network move_weights: current_weights recompute_path: false;
			if( in_car) {
				my_car.location <- location;
			}
			if (current_target = location) {
				if (current_target = final_target) {
					
					leaving_people <- leaving_people + 1;
			//		write "bye bye : " + leaving_people;
					if (in_car) {ask my_car {do die;}}
					do die;
				} else {
					in_car <- true;
					current_target <- final_target;
				}

			}

		}

	}

//OK mais on pourrait aussi jouter la position de l'eau
	plan give_information intention: inform  {
		nb_inform <- nb_inform+ 1;
		nb_waiting <- nb_waiting - 1;
//		write name + " je dois informer";
		ask one_of(people) {
			do add_belief(water_is_coming);
		}
		strength_inform <-  strength_inform - strength_information_decrement;
		do remove_intention(inform, true);
		
	}

//Ok pour la partie random (pas pour aller aider quelqu'un)
	plan go_outside intention: outside {
		nb_outside <- nb_outside+ 1;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je dois sortir";
		inside<-false;
		my_color <- #pink;
		speed <- my_speed();
		if (random_target = nil) {
			random_building<-one_of (building);
			random_target <- (random_building.location);
			if (have_car) {
				current_target <- my_car.location;
			} else {
				current_target <- random_target;
			}

		} else {
			do goto target: current_target on: road_network move_weights: current_weights recompute_path: false;
			if( in_car) {
				my_car.location <- location;
			}
			if (current_target = location) {
				if (current_target = random_target) {
		//			write "j'y suis";
				my_building<-random_building;
				inside<-true;
				do remove_belief(need_to_go_outside);
				do remove_intention(outside, true);
				float dist<-50#m;
				list<road> roads_neigh <- (road at_distance dist);
					loop while: empty(roads_neigh) {
						dist <- dist + 20;
						roads_neigh <- (road at_distance dist);
					}
					road a_road <- roads_neigh[rnd_choice(roads_neigh collect each.shape.perimeter)];
					my_car.location <- any_location_in(a_road);
			

				
				} else {
					in_car <- true;
					current_target <- random_target;
				}

			}

		}

		
	}


	action going_upstairs {
		nb_upstairs <- nb_upstairs+ 1;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je dois monter à l'étage";
		current_stair <- my_building.nb_stairs;
	}
	
	//OK
	plan go_upstairs intention: upstairs {
		do going_upstairs;
		do remove_intention(upstairs, true);
	}

	action inquiring_information {
		nb_inquire <- nb_inquire+ 1;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je m'informe";
		do add_belief(water_is_coming);	
	}
	//ok
	plan inquire_information intention: inquire {
		do inquiring_information;
		do remove_intention(inquire, true); 	
	}

//ok
	plan protect_my_car intention: protect_car {
		nb_protect_car <- nb_protect_car+ 1;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je dois protéger ma voiture";
		
		speed <- my_speed();
		if (final_target = nil) {
			road a_road <- safe_roads[rnd_choice(safe_roads collect (1.0/(1 + each.location distance_to my_car.location)))];
			final_target <- any_location_in(a_road);
			current_target <- my_car.location;

		} else {
			do goto target: current_target on: road_network move_weights: current_weights recompute_path: false;
			if( in_car) {
				my_car.location <- location;
			}
			if (current_target = location) {
				if (current_target = final_target) {
					in_car <- false;
					current_target <- any_location_in(my_building);
					return_home <- true;
				} else {
					if (return_home) {
						do remove_belief(vulnerable_car);
						do remove_intention(protect_car, true);
						return_home <- false;
					} else {
						in_car <- true;
						current_target <- final_target;
					}
					
				}

			}

		}
	}

//Ok
	plan protect_my_properties intention: protect_properties {
		do protect_properties;
		do remove_intention(protect_properties, true); 	
	}


action protect_properties {
		nb_protect_properties <- nb_protect_properties+ 1;
		current_stair<-0;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je dois protéger ma proprieté";
		my_building.vulnerability <- my_building.vulnerability - (0.05 * step / 1 #mn);
}

//ok
	plan turn_off_nrj intention: turn_off {
		nb_turn_off <- nb_turn_off+ 1;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je dois enlever le courant";
		my_building.nrj_on <- false;
		do remove_belief(energy_is_on);
		do remove_intention(turn_off, true);
	}

//OK
	plan weather_strip_house intention: weather_strip {
		
		do remove_intention(weather_strip, false); 	
	}
	
	
	action weather_strip_house {
		nb_weather_strip <- nb_weather_strip+ 1;
		current_stair<-0;
		nb_waiting <- nb_waiting - 1;
	//	write name + " je dois protéger ma proprieté";
		my_building.impermeability <- min([1, my_building.impermeability + 0.01]);
}
	

	//***************************  APPARENCE  ********************************************************
	aspect default {
		draw cylinder(1 #m, 5 #m) color: my_color;
	}

}

//***********************************************************************************************************
//***************************  CELL     **********************************************************************
//***********************************************************************************************************
grid cell neighbors: 8 file: mnt_file {
	bool is_active <- false;
	float water_height;
	float altitude <- grid_value;
	bool is_river <- false;
	bool is_sea <- false;
	bool is_canal <- false;
	bool is_canal_origin<-false;
	bool already;
	float height;
	float water_flowing_altitude;
	list<building> obstacles;
	list<building> my_buildings;
	float obstacle_height <- 0.0;
	bool escape_cell;

	//Action to compute the highest obstacle among the obstacles
	float compute_highest_obstacle {
		if (empty(obstacles)) {
			return 0.0;
		} else {
			return obstacles max_of (each.bd_height);
		}

	}
	//ask (active_cells sort_by ((each.water_flowing_altitude + each.water_height + each.obstacle_height)))
	//Action to flow the water 
	action flow {
	//if the height of the water is higher than 0 then, it can flow among the neighbour cells
		if (water_height > 0) {
		//We get all the cells already done
			list<cell> neighbour_cells_al <- neighbors where (each.already);
			//If there are cells already done then we continue         
			if (!empty(neighbour_cells_al)) {
				if !is_canal_origin {
				//We compute the height of the neighbours cells according to their altitude, water_height and obstacle_height
					ask neighbour_cells_al {
						height <- max([water_flowing_altitude + water_height, water_flowing_altitude + obstacle_height]);
					}
					
					//The height of the cell is equals to its altitude and water height
					height <- water_flowing_altitude + water_height;
					
					//The water of the cells will flow to the neighbour cells which have a height less than the height of the actual cell
					list<cell> flow_cells <- (neighbour_cells_al where (height > each.height));					
				//If there are cells, we compute the water flowing
					if (!empty(flow_cells)) {
						float water_max <- diffusion_rate*max([0.0, min([water_height/length(flow_cells)])]);
						loop flow_cell over: shuffle(flow_cells) sort_by (each.height) {
						//	float water_flowing <- max([0.0, min([(height - flow_cell.height), water_height * diffusion_rate])]);
						float water_flowing <- max([0.0, min([(height - flow_cell.height), water_max])]);
						water_height<-water_height - water_flowing;
						flow_cell.water_height <- flow_cell.water_height + water_flowing;
					//		height <- water_flowing_altitude + water_height;
					//		flow_cell.height <- flow_cell.water_flowing_altitude + flow_cell.water_height;
							
						}

					}

				} 
				
				else { //si canal
						float water_max <- diffusion_rate*water_height*coeff_enter_canal;
						water_height<-water_height - water_max;		
				}
				/* 
				//We compute the height of the neighbours cells according to their altitude, water_height and obstacle_height
					ask neighbour_cells_al {
						height <- water_flowing_altitude + water_height;
					}
					//The height of the cell is equals to its altitude and water height
					height <- water_flowing_altitude + water_height;
					//The water of the cells will flow to the neighbour cells which have a height less than the height of the actual cell
					list<cell> flow_cells <- (neighbour_cells_al where (height > each.height and (is_canal or is_sea)));
					//If there are cells, we compute the water flowing
					if (!empty(flow_cells)) {
						loop flow_cell over: shuffle(flow_cells) sort_by (each.height) {
							float water_flowing <- max([0.0, min([(height - flow_cell.height), water_height * diffusion_rate])]);
							water_height <- with_precision(water_height - water_flowing, 2);
							flow_cell.water_height <- flow_cell.water_height + water_flowing;
							height <- water_flowing_altitude + water_height;
						}
 */
					}  
 
				}

			

		already <- true;
		if is_sea {
			water_height <- 0.0;
		}

	}

	//Update the color of the cell
	action update_color {
		if is_river {
			color <- #blue;
		}

		int val_water <- 0;
		val_water <- max([0, min([255, int(255 * (1 - (water_height / 4)))])]);
		color <- rgb([val_water, val_water, 255]);
		if is_sea {
			color <- #gamablue;
		}

		grid_value <- water_height + water_flowing_altitude;
	}

	aspect default {
		if display_grid {
		//	draw shape depth:(altitude+water_height) color: color border: #black;
			draw shape  color: color border: #white;
		}

	}

}

//***********************************************************************************************************
//***************************  OUTPUT  **********************************************************************
//***********************************************************************************************************
experiment "go_flood" type: gui {
	parameter "scenario" var:scenario ;
	parameter "type_explo" var:type_explo;
	
	output {
		display map type: opengl background: #black draw_env: false {
		//si vous voulez afficher le mnt
			image "../includes/background.png" transparency: 0.3 refresh: false;
			agents active_cells value: active_cells;
			//grid cell elevation: grid_value triangulation: true;
			species building;
			species road refresh: false;
			species river refresh: false;
			species canal refresh: false;
			species people;
			species car;
		}
		
		display charts {
			chart "number of evacuation and death" size:{1.0, 0.5} {
				data "number of people" value: length(people) color: #gray;
				data "number of evacuated people" value: leaving_people color: #green;
				data "number of deads" value: dead_people_inside+dead_people_outside color: #red;
			}
			
	
			chart "current plan" size:{1.0, 0.5} position: {0.0, 0.5} {
				data "nb of people waiting" value: nb_waiting color: #gray; 
				data "nb of people draininf off" value:nb_drain_off color: #blue; 
				data "nb of people evacuating" value:nb_evacuate color: #yellow; 
				data "nb of people informing others" value:nb_inform color: #magenta; 
				data "nb of people going outside" value:nb_outside color: #pink; 
				data "nb of people going upstair" value:nb_upstairs color: #violet; 
				data "nb of people searching information" value:nb_inquire color: #orange; 
				data "nb of people protecting car" value:nb_protect_car color: #red; 
				data "nb of people protecting property" value:nb_protect_properties color: #brown; 
				data "nb of people turn off electricity" value:nb_turn_off color: #cyan; 
				data "nb of people striping property" value:nb_weather_strip color: #lightgreen; 
				
				
			}
			
		}

	}
	
	}
// This experiment runs the simulation 5 times.
// At the end of each simulation, the people agents are saved in a shapefile
experiment 'Run 1 simulation' type: batch repeat:2 keep_seed:false until:(end_simul) {
	
	// the reflex will be activated at the end of each run; in this experiment a run consists of the execution of 5 simulations (repeat: 5)
	reflex end_of_runs
	{
		int cpt <- 0;
		// each simulation of the run is an agent; it is possible to access to the list of these agents by using the variable "simulations" of the experiment. 
		// Another way of accessing to the simulations consists in using the name of model + _model: here "batch_example_model"
		//in this example, we ask all the simulation agents of the run to save (at the end of the simulation) the people population in a shapefile with their is_infected and is_immune attributes 
		ask simulations
		{
	cpt <- cpt + 1;
		}
	}
}