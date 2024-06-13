model NewModel_model_VR

import "GameModel.gaml"

species unity_linker parent: abstract_unity_linker {
	string player_species <- string(unity_player);
	int max_num_players  <- -1;
	int min_num_players  <- 10;
	list<point> init_locations <- define_init_locations();
	unity_property up_people;

	list<point> define_init_locations {
		return [world.location];
	}
	
	init {
		//define the unity properties
		do define_properties;
		
	}
	
	//action that defines the different unity properties
	action define_properties {
		
		//define a unity_aspect called tree_aspect that will display in Unity the agents with the SM_arbres_001 prefab, with a scale of 2.0, no y-offset, 
		//a rotation coefficient of 1.0 (no change of rotation from the prefab), no rotation offset, and we use the default precision. 
		unity_aspect car_aspect <- prefab_aspect("Prefabs/Visual Prefabs/City/Vehicles/Car",100,0.2,1.0,-90.0, precision);
		
		//define the up_car unity property, with the name "car", no specific layer, the car_aspect unity aspect, no interaction, and the agents location are not sent back 
		//to GAMA. 
		up_people<- geometry_properties("car", nil, car_aspect, #no_interaction, false);
		
		// add the up_tree unity_property to the list of unity_properties
		unity_properties << up_people;
		
		
	}
	
	reflex send_agents when: not empty(unity_player) {
		do add_geometries_to_send(people where (each.my_path != nil),up_people);
		
		
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
	float minimum_cycle_duration <- 0.05;
	string unity_linker_species <- string(unity_linker);
	list<string> displays_to_hide <- ["map"];
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
