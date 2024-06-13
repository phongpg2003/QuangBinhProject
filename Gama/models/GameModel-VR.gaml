model NewModel_model_VR

import "GameModel.gaml"

species unity_linker parent: abstract_unity_linker {
	string player_species <- string(unity_player);
	int max_num_players  <- -1;
	int min_num_players  <- 10;
	list<point> init_locations <- define_init_locations();
	unity_property up_building;
	unity_property up_road;

	list<point> define_init_locations {
		return [world.location];
	}
	
	init {
		//define the unity properties
		do define_properties;
		
		//add the static_geometry agents as static agents/geometries to send to unity with the up_geom unity properties.
		do add_background_geometries(building,up_building);
		do add_background_geometries(road collect (each.shape + 4),up_road);
		
	}
	
	//action that defines the different unity properties
	action define_properties {
		
		//define a unity_aspect called geom_aspect that will display the agents using their geometries, with a height of 10 meters, the gray color, and we use the default precision. 
		unity_aspect building_aspect <- geometry_aspect(10.0, #gray, precision);
		
		//define the up_geom unity property, with the name "polygon", no specific layer, no interaction, and the agents location are not sent back 
		//to GAMA. 
		up_building <- geometry_properties("building", nil, building_aspect, #no_interaction, false);
		
		// add the up_geom unity_property to the list of unity_properties
		unity_properties << up_building;
		
		
		
		//define a unity_aspect called geom_aspect that will display the agents using their geometries, with a height of 10 meters, the gray color, and we use the default precision. 
		unity_aspect road_aspect <- geometry_aspect(10.0, #gray, precision);
		
		//define the up_geom unity property, with the name "polygon", no specific layer, no interaction, and the agents location are not sent back 
		//to GAMA. 
		up_road <- geometry_properties("road", nil, road_aspect, #no_interaction, false);
		
		// add the up_geom unity_property to the list of unity_properties
		unity_properties << up_road;
	}
	


}

species unity_player parent: abstract_unity_player{
	float player_size <- 1.0;
	rgb color <- #red;	
	float cone_distance <- 10.0 * player_size;
	float cone_amplitude <- 90.0;
	float player_rotation <- 90.0;
	bool to_display <- true;
	aspect default {
		if to_display {
			if selected {
				 draw circle(player_size) at: location + {0, 0, 4.9} color: rgb(#blue, 0.5);
			}
			draw circle(player_size/2.0) at: location + {0, 0, 5} color: color ;
			draw player_perception_cone() color: rgb(color, 0.5);
		}
	}
}

experiment vr_xp parent:game autorun: false type: unity {
	float minimum_cycle_duration <- 3600.0;
	string unity_linker_species <- string(unity_linker);
	list<string> displays_to_hide <- ["map","Tools panel","Number of people alive","Number of people die","Number of road die","Number of building drowned"];
	float t_ref;

	action create_player(string id) {
		ask unity_linker {
			do create_player(id);
		}
	}

	action remove_player(string id_input) {
		if (not empty(unity_player)) {
			ask first(unity_player where (each.name = id_input)) {
				do die;
			}
		}
	}

	output {
		 display map_VR parent:map{
			 species unity_player;
			 event #mouse_down{
				 float t <- gama.machine_time;
				 if (t - t_ref) > 500 {
					 ask unity_linker {
						 move_player_event <- true;
					 }
					 t_ref <- t;
				 }
			 }
		 }
	}
}
