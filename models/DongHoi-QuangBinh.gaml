model NewModel

global {
	grid_file DEM_grid_file <- grid_file("../includes/DEM.tif");
	file river_shapefile <- file("../includes/river.shp");
	geometry river <- geometry(river_shapefile);
	file shape_file_roads <- file("../includes/road.shp");
	file buildings_shapefile <- file("../includes/buildings.shp");
	file shape_file_evacuation <- file("../includes/evacuation_point.shp");
	shape_file bounds_shape_file <- shape_file("../includes/QBBB.shp");
	file MOC_shapefile <- file("../includes/MOC.shp");
	geometry shape <- envelope(bounds_shape_file);
	bool height_propagation <- false;
	map<date, list<float>> data_map;
	float step <- 1 #h;
	int nb_of_people <- 100;
	int casualties <- 0;
	int evacuated <- 0;
	int roaddie <- 0 update: length(road where (each.drowned = true));
	int buildingdie <- 0 update: length(building where (each.drowned = true));
	string the_alert_strategy;
	graph<geometry, geometry> road_network;
	graph<geometry, geometry> river_network;
	list<cell> current_cells;
	list<cell> done;
	int action_type <- -1;
	list<string> actions <- ["build dyke", "destroy dyke", "end of turn"];
	float budget ;
	float budget_year <- 20000000.0;
	
	string PLAYER_TURN <- "PLAYER TURN";
	string SIMULATION <- "SIMULATION";
	
	string stage <- PLAYER_TURN;
	
	list<float> flood_coefficient <- [0.5, 1.0, 0.3];
	int index_flood <- 0;
	float current_coeff;
	
	point target_point;
	bool ok_build_dyke <- true;
	point start_point <- nil;
	

	reflex run_simulation when: stage = SIMULATION {
		if (current_date in data_map.keys) {
	 		do add_water;
	 	} else {
	 		stage <- PLAYER_TURN;
	 		index_flood <- index_flood + 1;
	 		do update_data_map;
	 		do start_player_turn;
	 		do pause;
	 	}
	}
	
	
	action start_player_turn {
		do tell("Start of the player turn!");
		
		budget <- budget_year;
		ask cell {
			flooding_level <- 0.0;
			color <-  #white;
		} 
		
		/*ask cell {
			float v <- altitude / 1000 * 255;
			write sample(v);
			color <- rgb(255 , 255 - v, 255 - v );
		}
		*/
		ask experiment {
			do update_outputs;
		}
			
	}
	action start_simulation {
		if (length(flood_coefficient) > index_flood) {
			ask building {
				drowned <- false;
				colorbuilding <- #gray;
			}
			ask road {
				drowned <- false;
				colorroad <- #black;
			}
			do tell("Start of the simulation stage!");
			current_coeff <- flood_coefficient[index_flood];
			
			stage <- SIMULATION;
			do compute_height_propagation;
	 		do resume;
	 		
		} else  {
			
			do tell("END OF GAME!");
		}
		
		
	}
	action activate_act {
		button selected_but <- first(button overlapping (circle(1) at_location #user_location));
		
		if (selected_but != nil) {
			if (selected_but.index = 2) {
				do start_simulation;
			
			} else {
				start_point <- nil;
				ask selected_but {
					ask button {
						bord_col <- #black;
					}
	
					if (action_type != id) {
						action_type <- id;
						bord_col <- #red;
					} else {
						action_type <- -1;
					}
				}
			}
		}
	}

	
	float price_computation(point target){
		return (start_point distance_to target) with_precision 1;
	}
	action action_management {
		switch action_type {
			match 0 {
				if start_point = nil {
					start_point <- #user_location;
				} else  {
					float price <- price_computation(#user_location);
					if (ok_build_dyke) {
						create dyke with: (shape: line([start_point, #user_location]));
						budget <- (budget - price) with_precision 1;
					}

					start_point <- nil;
					target_point <- nil;
					ok_build_dyke <- false;
				} 
			}

			match 1 {
				list<dyke> d <- dyke overlapping (#user_location buffer 1);
				if (not empty(d)) {
					ask d closest_to #user_location {
						do die;
					}
				}
			} 
			
		} 
	}

	int cpt <- 0;

	init {
		create building from: buildings_shapefile;
		create road from: clean_network(shape_file_roads.contents, 0.0, false, true);
		create people number: nb_of_people {
			location <- any_location_in(one_of(building));
		}
		road_network <- as_edge_graph(road);
		river_network <- as_edge_graph(river_shapefile);
		
		list<building> all_buildings <- list(building);
		

		create evacuation_point from: shape_file_evacuation;
		create riverr from: river_shapefile;
		file data <- csv_file("../includes/FloodData.csv", ",");
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
		
		do init_water;
		do start_player_turn;
	
		
	}


	action update_data_map{
		float s <- current_date - first(data_map.keys);
		list<date> previous_dates <- copy(data_map.keys);
		loop d over: previous_dates {
			list<float> v <- data_map[d];
			data_map[d add_seconds s] <- v;
			remove key: d from: data_map ;
			
		}
		
	}

	


	action compute_height_propagation {
		current_cells <- cell overlapping (river);
		ask cell  {
			dyke_altitude <- 0.0;
		}
		ask dyke where not each.is_broken {
			ask cell overlapping (shape + width) {
				dyke_altitude <- (myself.height);
			}
		}
		
		ask building {
			ask cell overlapping shape {
				dyke_altitude <- max(dyke_altitude,(myself.height));
			}
		}
	
		ask current_cells {
			altitude <- grid_value + dyke_altitude;
			//color <- #blue;
		}
		
		done <- copy(current_cells);
		loop while: not empty(current_cells) {
			loop c over: copy(current_cells) {
				current_cells >> c;
				done << c;
				loop cn over: c.neighbors {
					if not (cn in done) and not (cn in current_cells) {
						current_cells << cn;
					}
					cn.altitude <- max(cn.grid_value + cn.dyke_altitude , min(c.altitude, cn.altitude));
				}
			}
		}

	}
	
	action init_water {
		ask cell overlapping river {
			is_river <- true;
		}

	}
	
	action precompute_propagate_water  {
		do compute_height_propagation;

		ask cell {
			grid_value <- altitude;
		}

		save (cell) format: "asc" to: "../includes/elevation.asc";
	}

	

	action add_water {
		bool need_recomputation <- false;
		float water_level <- data_map[current_date][0] * current_coeff;
		ask cell {
			flooding_level <- water_level - altitude;
			if flooding_level <= 0.0 {
			// Bỏ qua dòng này để giữ nguyên màu ban đầu của ô
			} else {
				if dyke_altitude > 0.0 {
					dyke_altitude <- 0.0;
					ask dyke overlapping self {
						is_broken <- true;
					}
					need_recomputation <- true;
				}
				//is_river <- true;
			//	if (is_river) {
					float val <- 255 * (1 - flooding_level / 1.0);
					color <- rgb(val, val, 255);
					
					
				//}

			}

		}
		if need_recomputation {
			do compute_height_propagation ;
		}
	} 
}

species evacuation_point {
	rgb color <- #red;
	aspect base {
		draw triangle(50) color: color;
	}
}

species riverr {
	rgb color <- #blue;
	aspect base {
		draw shape color: color;
	}
}

species road {
	int lanes;
	bool drowned <- false;
	float height <- 0.5;
	rgb colorroad <- #black;
	cell the_cell <- one_of(cell);
	list<road> overlapping_roads <- [];

	init {
		overlapping_roads <- road where (each overlaps self);
	}

	reflex check_drowning {
		ask riverr {
			ask myself.overlapping_roads {
				cell a_cell <- one_of(cell overlapping self);
				if (a_cell != nil and a_cell.flooding_level > 0.0 and a_cell.grid_value > height) // Kiểm tra a_cell không phải là nil
				{
					road_network >- self;
					drowned <- true;
					colorroad <- #red;
				}
			}
		}
	}

	aspect base {
		draw shape color: colorroad;
	}
}

species dyke {
	float height <- 10.0;
	bool is_broken <- false;
	float width <- 10.0;
	
	aspect default {
		draw shape + width depth: height color: is_broken ? #red : #green;
	}
}

species building {
	float height <- 5.0 + rnd(5.0);
	rgb colorbuilding <- #gray;
	bool drowned <- false;
	rgb colorroad <- #black;
	cell the_cell <- one_of(cell);
	list<building> overlapping_building <- [];

	init {
        overlapping_building <- building where (each overlaps self);
	}

	reflex check_drowning {
		ask riverr {
			ask myself.overlapping_building {
				cell a_cell <- one_of(cell overlapping self);
				if (a_cell != nil and a_cell.flooding_level > 0.0 and a_cell.grid_value > height) // Kiểm tra a_cell không phải là nil
				{
					drowned <- true;
					colorbuilding <- #red;
				}

			}

		}

	}

	aspect base {
		draw shape color: colorbuilding;
	}
}

species people skills: [moving] {
	float height_pp <- 1.7 #m;
	string color <- 'green';
	bool boat <- false;
	bool alerted <- false;
	bool drowned <- false;
	bool saved <- false;
	point target <- {1690.1801628989738, 3838.6843718571527, 0.0};
	float perception_distance;
	float speed <- 0.5 #km / #h;

	reflex alert_target {
		if (not alerted and not drowned and not saved and color = 'green') {
			switch the_alert_strategy {
				match "RANDOM" {
					target <- any_location_in(one_of(evacuation_point));
					do goto(target: target, on: road_network);
				}

				match "CLOSEST" {
					target <- (evacuation_point closest_to self).location;
					do goto(target: target, on: road_network);
				}
			}

			if (target != nil) {
				do goto;
			}

			if (location = target) {
				evacuated <- evacuated + 1;
				saved <- true;
				target <- nil;
			} 
		}

		if (drowned or saved) {
			do die;
		} 
	}

	list<people> overlapping_pp <- [];

	init {
		overlapping_pp <- people where (each overlaps self);
	}

	reflex check_drowning {
		ask riverr {
			ask myself.overlapping_pp {
				cell a_cell <- one_of(cell overlapping self);
				if (a_cell != nil and a_cell.flooding_level > 0.0) {
					drowned <- true;
				}

			}

		}

	}

	reflex die when: drowned {
		casualties <- casualties + 1;
		do die;
	}

	aspect default {
		draw circle(36) color: boat ? #red : #green;
	} 
}

grid cell file: DEM_grid_file neighbors: 4 {
	float altitude <- #max_float;
	float dyke_altitude;
	bool is_river;
	float flooding_level;
	aspect default {
		draw "" + (grid_value with_precision 1) + "," + (altitude with_precision 1) color: #black font: (font(10));
	}
}

grid button width: 2 height: 2 {
	int id <- int(self);
	rgb bord_col <- #black;

	aspect normal {
		draw rectangle(shape.width * 0.8, shape.height * 0.8).contour + (shape.height * 0.01) color: bord_col;
		draw (actions[id]) size: {shape.width * 0.5, shape.height * 0.5} color: #white;
	}
}

/*experiment compute_height_ptopagation {
	action _init_ {
		create simulation with: (height_propagation: true);
	}
	output {
		display map {
			species cell;
		}
	}
}*/

experiment game type: gui {
	parameter "Alert Strategy" var: the_alert_strategy init: "CLOSEST" among: ["RANDOM", "CLOSEST", "BOAT"] category: "Alert";
	parameter "Number of people" var: nb_of_people init: 100 min: 100 max: 20000 category: "Initialization";
	output {
		layout horizontal([0.0::7285, 1::2715]) tabs: true controls: false;
		display map {
			grid cell;
			graphics "river " {
				draw river color: #blue;
			}

			species building aspect: base refresh: false;
			species road aspect: base;
			species people aspect: default;
			species evacuation_point aspect: base;
			species dyke;
			graphics "point" {
				if start_point != nil {
					draw square(80) color: #pink at: start_point;
				}
				if target_point != nil and ok_build_dyke {
					draw square(80) color: #magenta at: target_point;
				}
			}

			event #mouse_move {
				if (start_point != nil) {
					ask world {
						target_point <- #user_location;
						float p <- price_computation(target_point);
						ok_build_dyke <- p <= budget;
						if ok_build_dyke {
							geometry l <- line([start_point,target_point ]);
							if ((cell overlapping l) first_with (each.is_river)) != nil {
								ok_build_dyke <- false;
							}
							if ok_build_dyke and (not empty(building overlapping l) ) {
								ok_build_dyke <- false;
							}
						}
					}
					
				}
			}
			event #mouse_down {
				ask simulation {
					do action_management;
				}

			}

		}

		/*display dem type: 3d {
			grid cell elevation: grid_value * 10 grayscale: true;
			graphics "river " {
				draw river color: #red;
			}

		}*/

		display action_button background: #black name: "Tools panel" type: 2d antialias: false {
			species button aspect: normal;
			graphics "budget" {
				draw "budget: " + (budget with_precision 0) color: #white font:font(24) at: {world.location.x, 300} anchor: #center ;
			}
			event #mouse_down {
				ask simulation {
					do activate_act;
				}

			}

		}

		monitor "Number of people alive" value: evacuated;
		monitor "Number of people die" value: casualties;
		monitor "Number of road die" value: roaddie;
		monitor "Number of building drowned" value: buildingdie;
		
	}

}