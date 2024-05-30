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
	map<string, rgb> color_per_type <- ["CurrentDate"::current_date, "Budget"::budget, "LimitScore"::limitscore, "Score"::score];
	map<date, list<float>> data_map;
	float step <- 1 #mn;
	date current_date <- #now;
	int nb_of_people <- 100;
	int casualties <- 0;
	int evacuated <- 0;
	int roaddie <- 0 update: length(road where (each.drowned = true));
	int buildingdie <- 0 update: length(building where (each.drowned = true));
	string the_alert_strategy;
	graph<geometry, geometry> road_network;
	graph<geometry, geometry> road_network_usable;
	map<road, float> new_weights;
	graph<geometry, geometry> river_network;
	list<cell> current_cells;
	list<cell> done;
	int action_type <- -1;
	list<string> actions <- ["build dyke", "destroy dyke", "end of turn"];
	float budget;
	float score;
	float limitscore;
	float budget_year <- 8000.0;
	bool need_to_recompute_graph <- false;
	list<file> images <- [file("../includes/build_icon.png"), file("../includes/eraser.png"), file("../includes/gstart_icon.jpg")];
	file my_csv_file <- csv_file("../includes/FloodDataH_short.csv", ",");
	string PLAYER_TURN <- "PLAYER TURN";
	string SIMULATION <- "SIMULATION";
	string stage <- PLAYER_TURN;
	list<float> flood_coefficient <- [0.3, 0.8, 0.2];
	int index_flood <- 0;
	float current_coeff;
	point target_point;
	bool ok_build_dyke <- true;
	point start_point <- nil;
	bool update_drowning <- false update: true;
	map<string, float> benchmark_map;
	bool benchmark_mode <- false;
	reflex run_simulation when: stage = SIMULATION {
		float t;
		if benchmark_mode {
			t <- gama.machine_time;
		}
		
		if (every(#h)) {
			if (current_date in data_map.keys) {
				if need_to_recompute_graph {
					new_weights <- road as_map (each::each.shape.perimeter * (each.drowned ? 3.0 : 1.0));
					road_network_usable <- as_edge_graph(road where not each.drowned);
				}
				t <- add_benchmark("recompute_graph",t);	
				do add_water;
				t <- add_benchmark("add_water",t);
				
			} else {
				stage <- PLAYER_TURN;
				index_flood <- index_flood + 1;
				do update_data_map;
				do start_player_turn;
				do pause;
			}
		
		}
		if (benchmark_mode ) {
			write "*********** Benchmark result ***********";
			loop k over: benchmark_map.keys sort_by (-1 *benchmark_map[each]) {
				write k + " -> " + (benchmark_map[k]/1000.0) + " s";
			}
		}

	}
	
	float add_benchmark (string k, float ref_time) {
		if benchmark_mode {
			if not (k in benchmark_map.keys) {
				benchmark_map[k] <- 0.0;
			}
			benchmark_map[k] <- benchmark_map[k]  + gama.machine_time - ref_time;
			ref_time <- gama.machine_time;
			return gama.machine_time;
		}
		return 0.0;
	}

	action start_player_turn {
		do tell("Start of the player turn!");
		do tell(string(score));
		budget <- budget_year;
		score <- budget;
		ask cell {
			flooding_level <- 0.0;
			color <- #white;
		}

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
		} else {
			do tell("END OF GAME!");
			do tell(string(score));
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

	float price_computation (point target) {
		return (start_point distance_to target) with_precision 1;
	}

	action action_management {
		switch action_type {
			match 0 {
				if start_point = nil {
					start_point <- #user_location;
				} else {
					float price <- price_computation(#user_location);
					if (ok_build_dyke) {
						create dyke with: (shape: line([start_point, #user_location]));
						budget <- (budget - price) with_precision 1;
						score <- score - price;
					}

					start_point <- nil;
					target_point <- nil;
					ok_build_dyke <- false;
				} }

			match 1 {
				list<dyke> d <- dyke overlapping (#user_location buffer 1);
				if (not empty(d)) {
					ask d closest_to #user_location {
						do die;
					}

				}

			} } }

	int cpt <- 0;

	init {
		create building from: buildings_shapefile;
		create road from: clean_network(shape_file_roads.contents, 0.0, false, true);
		create people number: nb_of_people {
			location <- any_location_in(one_of(building));
		}

		road_network <- as_edge_graph(road);
		road_network_usable <- as_edge_graph(road);
		river_network <- as_edge_graph(river_shapefile);
		new_weights <- road as_map (each::each.shape.perimeter);
		list<building> all_buildings <- list(building);
		create evacuation_point from: shape_file_evacuation;
		create riverr from: river_shapefile;
		matrix matrix_data <- matrix(my_csv_file);
		loop i from: 1 to: matrix_data.rows - 1 {
			list<string> d <- matrix_data[0, i] split_with " ";
			list<string> dd <- d[0] split_with "/";
			list<string> dt <- d[1] split_with ":";
			date date_gama <- date([int(dd[2]), int(dd[1]), int(dd[0]), int(dt[0]), int(dt[1]), 0]);
			data_map[date_gama] <- [float(matrix_data[5, i])];
		}

		starting_date <- first(data_map.keys);
		do init_water;
		do start_player_turn;
	}

	action update_data_map {
		float s <- current_date - first(data_map.keys);
		list<date> previous_dates <- copy(data_map.keys);
		loop d over: previous_dates {
			list<float> v <- data_map[d];
			data_map[d add_seconds s] <- v;
			remove key: d from: data_map;
		}

	}

	reflex notification when: score >= limitscore {
		do tell("You passed the level!");
		do die;
	}

	action compute_height_propagation {
		current_cells <- cell overlapping (river);
		ask cell {
			dyke_altitude <- 0.0;
		}

		ask dyke where not each.is_broken {
			ask cell overlapping (shape + width) {
				dyke_altitude <- (myself.height);
			}

		}

		ask building {
			ask cell overlapping shape {
				dyke_altitude <- max(dyke_altitude, (myself.height));
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

					cn.altitude <- max(cn.grid_value + cn.dyke_altitude, min(c.altitude, cn.altitude));
				}

			}

		}

	}

	action init_water {
		ask cell overlapping river {
			is_river <- true;
		}

	}

	action precompute_propagate_water {
		do compute_height_propagation;
		ask cell {
			grid_value <- altitude;
		}

		save (cell) format: "asc" to: "../includes/elevation.asc";
	}

	action add_water {
		float t <- gama.machine_time;
		update_drowning <- true;
		bool need_recomputation <- false;
		matrix matrix_data <- matrix(my_csv_file);
		float water_level <- data_map[current_date][0];
		ask cell {
			flooding_level <- water_level - altitude;
			if flooding_level <= 0.0 {
			} else {
				if dyke_altitude > 0.0 {
					dyke_altitude <- 0.0;
					ask dykes where not (each.is_broken)  {
						is_broken <- true;
						need_recomputation <- true;
					}

				}
				float val <- 255 * (1 - flooding_level / 1.0);
				color <- rgb(val, val, 255);
			}

		}
		t <- add_benchmark("compute_water_level",t);	
				
		if need_recomputation {
			do compute_height_propagation;
		}
		t <- add_benchmark("compute_height_propagation",t);	
				
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
	list<cell> my_cells ;
	
	init {
		my_cells <- cell overlapping self;
	}
	
	
	reflex check_drowning when: not drowned and  update_drowning{
		float t;
		if benchmark_mode {
			t <- gama.machine_time;
		}
		
		ask my_cells where (each.flooding_level > 0.5) // Kiểm tra a_cell không phải là nil
		{
			myself.drowned <- true;
			myself.colorroad <- #red;
			need_to_recompute_graph <- true;
			break;
		}
		
		ask world {
			do add_benchmark("Road - check_drowning", t);
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

	init {
		ask cell overlapping self {
			dykes << myself;
		}
	}
	aspect default {
		draw shape + width depth: height color: is_broken ? #red : #green;
	}

}

species building {
	float height <- 5.0 + rnd(5.0);
	rgb colorbuilding <- #gray;
	bool drowned <- false;
	rgb colorroad <- #black;
	list<cell> my_cells ;
	
	init {
		my_cells <- cell overlapping self;
	}

	reflex check_drowning when: not drowned and  update_drowning{
		float t;
		if benchmark_mode {
			t <- gama.machine_time;
		}
		ask my_cells where (each.flooding_level > 1.0) {
			myself.drowned <- true;
			myself.colorbuilding <- #red;
			score <- score - 5.0;
			break;
		}
		ask world {
			do add_benchmark("Building - check_drowning", t);
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
	point target <- nil;
	float perception_distance;
	float speed <- 0.5 #km / #h;
	path my_path;

	reflex become_alerted when: not alerted and flip(0.01) {
		alerted <- true;
	}
	
	reflex alert_target when: alerted {
		float t;
		if benchmark_mode {
			t <- gama.machine_time;
		}
		if target = nil {
			switch the_alert_strategy {
				match "RANDOM" {
					target <- (one_of(evacuation_point)).location;
				}

				match "CLOSEST" {
					using (topology(road_network_usable)) {
						evacuation_point ep <- (evacuation_point closest_to self);
						target <- ep.location;
					}
				}
			}

			my_path <- road_network_usable path_between (location, target);
				//				write name + " " + sample(target) + " " + sample(my_path);
		}

		if my_path != nil {
			do follow(path: my_path, move_weights: new_weights);
			if (location = target) {
				score <- score + 100;
				evacuated <- evacuated + 1;
				target <- nil;
				do die;
			} 
		}
		ask world {
			do add_benchmark("alert_target", t);
		} 
	}

	reflex check_drowning {
		float t;
		if benchmark_mode {
			t <- gama.machine_time;
		}
		cell a_cell <- cell(location);
		if (a_cell != nil and a_cell.flooding_level > 0.2 and flip(0.5)) {
			casualties <- casualties + 1;
			score <- score - 10;
			do die;
		}
		ask world {
			do add_benchmark("People - check_drowning", t);
		}
	}

	aspect default {
		draw circle(36) color: boat ? #red : #green;
	} }

grid cell file: DEM_grid_file neighbors: 4 {
	float altitude <- #max_float;
	float dyke_altitude;
	list<dyke> dykes;
	bool is_river;
	float flooding_level;
	string type <- one_of(color_per_type.keys);
	rgb color <- color_per_type[type];

	aspect default {
		draw "" + (grid_value with_precision 1) + "," + (altitude with_precision 1) color: #black font: (font(10));
	}

}

grid button width: 2 height: 2 {
	int id <- int(self);
	rgb bord_col <- #black;

	aspect normal {
		draw rectangle(shape.width * 0.8, shape.height * 0.8).contour + (shape.height * 0.01) color: bord_col;
		draw image_file(images[id]) size: {shape.width * 0.8, shape.height * 0.5};
	}

}

experiment game type: gui {
	parameter "Alert Strategy" var: the_alert_strategy init: "CLOSEST" among: ["RANDOM", "CLOSEST"] category: "Alert";
	//	parameter "Number of people" var: nb_of_people init: 100 min: 100 max: 20000 category: "Initialization";
	output {
		layout horizontal([0.0::7285, 1::2715]) tabs: true controls: true;
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
					draw circle(30) color: #pink at: start_point;
				}

				if target_point != nil and ok_build_dyke {
					draw circle(30) color: #magenta at: target_point;
				}

			}

			event #mouse_move {
				if (start_point != nil) {
					ask world {
						target_point <- #user_location;
						float p <- price_computation(target_point);
						ok_build_dyke <- p <= budget;
						if ok_build_dyke {
							geometry l <- line([start_point, target_point]);
							if ((cell overlapping l) first_with (each.is_river)) != nil {
								ok_build_dyke <- false;
							}

							if ok_build_dyke and (not empty(building overlapping l)) {
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

			overlay position: {5, 5} size: {300 #px, 150 #px} background: #black transparency: 0.2 border: #black rounded: true {
				float y <- 30 #px;
				loop type over: color_per_type.keys {
					if (type = "Score") {
						draw "Score:" + score at: {10 #px, y + 4 #px} color: #white font: font("Helvetica", 18, #bold);
					}

					if (type = "LimitScore") {
						draw "LimitScore:" + limitscore at: {10 #px, y + 4 #px} color: #white font: font("Helvetica", 18, #bold);
					}

					if (type = "Budget") {
						draw "Budget:" + budget at: {10 #px, y + 4 #px} color: #white font: font("Helvetica", 18, #bold);
					}

					if (type = "CurrentDate") {
						draw "CurrentDate: " + string(date(current_date)) at: {10 #px, y + 4 #px} color: #white font: font("Helvetica", 18, #bold);
					}

					y <- y + 30 #px;
				}

			}

		}

		display action_button background: #black name: "Tools panel" type: 2d antialias: false {
			species button aspect: normal;
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

experiment "2 Simulations" type: gui {

	action _init_ {
		create simulation with: [nb_of_people::500, the_alert_strategy::"CLOSEST", my_csv_file::csv_file("../includes/FloodData.csv")] {
			do start_simulation;
		}

		create simulation with: [nb_of_people::500, the_alert_strategy::"RANDOM", my_csv_file::csv_file("../includes/FloodData.csv")] {
			do start_simulation;
		}

	}

	output {
		display map {
			grid cell;
			graphics "river " {
				draw river color: #blue;
			}

			species building aspect: base refresh: false;
			species road aspect: base;
			species people aspect: default;
			species evacuation_point aspect: base;
		}

	}

	permanent {
		display Comparison1 background: #white {
			chart "People die " type: series {
				loop s over: simulations {
					data "PP die " + int(s) value: s.casualties color: s.color marker: false style: line thickness: 6;
				}

			}

		}

		display Comparison2 background: #white {
			chart "Road die" type: series {
				loop s over: simulations {
					data "Road drowned " + int(s) value: s.roaddie color: s.color marker: false style: line thickness: 6;
				}

			}

		}

		display Comparison3 background: #white {
			chart "Building drowned" type: series {
				loop s over: simulations {
					data "Building drowned " + int(s) value: s.buildingdie color: s.color marker: false style: line thickness: 6;
				}

			}

		}

	}

}

experiment game_with_mode parent: game {

	action _init_ {
		map result <- user_input_dialog("Flooding game", [choose("Choose a difficulty", string, "normal", ["easy", "normal", "hard"])]);
		string difficulty <- result["Choose a difficulty"];
		switch difficulty {
			match "easy" {
				create simulation with: (nb_of_people: 200, budget_year: 4000, the_alert_strategy: "CLOSEST", my_csv_file: csv_file("../includes/FLoodDataH.csv"), limitscore: 20000);
			}

			match "normal" {
				create simulation with: (nb_of_people: 400, budget_year: 3500, the_alert_strategy: "CLOSEST", my_csv_file: csv_file("../includes/FLoodDataH.csv"), limitscore: 32000);
			}

			match "hard" {
				create simulation with: (nb_of_people: 600, budget_year: 2500, the_alert_strategy: "CLOSEST", my_csv_file: csv_file("../includes/FLoodDataH.csv"), limitscore: 55000);
			} } } }
