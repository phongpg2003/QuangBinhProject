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
	list<string> actions <- ["build dyke", "destroy dyke"];
	float budget <- 100000000000000000.0;

	action activate_act {
		button selected_but <- first(button overlapping (circle(1) at_location #user_location));
		if (selected_but != nil) {
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

	point start_point <- nil;

	action dyke_management {
		switch action_type {
			match 0 {
				if start_point = nil {
					start_point <- #user_location;
				} else {
					float price <- (start_point distance_to #user_location) with_precision 1;
					if (price <= budget) {
						create dyke with: (shape: line([start_point, #user_location]));
						budget <- (budget - price) with_precision 1;
					}

					start_point <- nil;
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
		if height_propagation {
			current_cells <- cell overlapping (river);
			ask current_cells {
				altitude <- grid_value;
				color <- #blue;
			}

			done <- copy(current_cells);
		} else {
			create building from: buildings_shapefile;
			create road from: clean_network(shape_file_roads.contents, 0.0, false, true);
			create people number: nb_of_people {
				location <- any_location_in(one_of(building));
			}

			create evacuation_point from: shape_file_evacuation;
			create riverr from: river_shapefile;
			road_network <- as_edge_graph(road);
			river_network <- as_edge_graph(river_shapefile);
			do init_water;
			list<building> all_buildings <- list(building);
			matrix elevation_grid_file <- matrix(csv_file("../includes/elevation.asc", " "));
			ask cell {
				altitude <- float(elevation_grid_file[grid_x, grid_y]);
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
		}
	}

	reflex propagate_water when: height_propagation {
		loop while: not empty(current_cells) {
			loop c over: copy(current_cells) {
				cpt <- cpt + 1;
				if cpt mod 1000 = 0 {
					write string(length(done)) + "/" + length(cell);
				}

				current_cells >> c;
				done << c;
				loop cn over: c.neighbors {
					if not (cn in done) and not (cn in current_cells) {
						current_cells << cn;
					}

					cn.altitude <- max(cn.grid_value, min(c.altitude, cn.altitude));
				}
			}
		}

		ask cell {
			grid_value <- altitude;
		}

		save (cell) format: "asc" to: "../includes/elevation.asc";
	}

	action init_water {
		ask cell overlapping river {
			is_river <- true;
		}

	}

	reflex add_water {
		float water_level <- data_map[current_date][0];
		ask cell {
			flooding_level <- water_level - altitude;
			if flooding_level <= 0.0 {
			// Bỏ qua dòng này để giữ nguyên màu ban đầu của ô
			} else {
				is_river <- true;
				if (is_river) {
					float val <- 255 * (1 - flooding_level / 1.0);
					color <- rgb(val, val, 255);
				}

			}

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
	float height <- 1000.0;
	init {
		ask cell overlapping self {
			is_river <- false;
			flooding_level <- -10.0;
			altitude <- myself.height;
		}

	}

	aspect default {
		draw shape + 10.5 depth: 2 color: #red;
	}
}

species building {
	float height <- 1.0 + rnd(2);
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
	bool is_river;
	float flooding_level;
	aspect default {
		draw "" + (grid_value with_precision 1) + "," + (altitude with_precision 1) color: #black font: (font(10));
	}
}

grid button width: 1 height: 2 {
	int id <- int(self);
	rgb bord_col <- #black;

	aspect normal {
		draw rectangle(shape.width * 0.8, shape.height * 0.8).contour + (shape.height * 0.01) color: bord_col;
		draw (actions[id]) size: {shape.width * 0.5, shape.height * 0.5} color: #white;
	}
}

experiment compute_height_ptopagation {
	action _init_ {
		create simulation with: (height_propagation: true);
	}
	output {
		display map {
			species cell;
		}
	}
}

experiment simulate_flood type: gui {
	parameter "Alert Strategy" var: the_alert_strategy init: "CLOSEST" among: ["RANDOM", "CLOSEST", "BOAT"] category: "Alert";
	parameter "Number of people" var: nb_of_people init: 100 min: 100 max: 20000 category: "Initialization";
	output {
		layout horizontal([0.0::7285, 1::2715]) tabs: true;
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
			graphics "start_point" {
				if start_point != nil {
					draw circle(1) color: #blue at: start_point;
				}

			}

			graphics "budget" {
				draw "budget:" + budget font: font(20) color: #brown at: {100, 100};
			}

			event #mouse_down {
				ask simulation {
					do dyke_management;
				}

			}

		}

		display dem type: 3d {
			grid cell elevation: grid_value * 10 grayscale: true;
			graphics "river " {
				draw river color: #red;
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