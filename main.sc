import java.io._

//     to do
// make executable which asks for input
// new shapes {triangle      archepeligo}
// river , forests , hills...
// deep water selection shouldnt include corners but does
// cliffs
//


object islandmaker9 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  	def to_text_file (args: Array[Array[Int]], file_name:String,  list_of_features:Array[Array[Int]]){
  		var file_name2 = file_name+".txt";
			var mi_writer = new PrintWriter(new File(file_name2) )
			mi_writer.write(" 0   =   water");mi_writer.println();
			mi_writer.write(" 1   =   land");mi_writer.println();
			mi_writer.write(" 2   =   deep water");mi_writer.println();
			mi_writer.write(" 3   =   beach"); mi_writer.println();
			mi_writer.write(" 9   =   rock");mi_writer.println();mi_writer.println();
			mi_writer.println();
			var k = 0;
			var border_string = "";
	      for(k <- 0 to args(0).length+5){border_string+= "#";}
			var border_string2 = "#";
			for(k <- 0 to args(0).length+3){border_string2+= " " }
			border_string2+= "#" ;
			var title_string= "" ;
			var num_of_spaces = args(0).length+5 - file_name.length
			var sp = 0;
			for(sp <- 0 to num_of_spaces/2){title_string += " "
	      }
			title_string = title_string + file_name
			
			mi_writer.println();
			mi_writer.write(title_string);
	    mi_writer.println();
			mi_writer.write(border_string);
	    mi_writer.println();
			mi_writer.write(border_string2);
	    mi_writer.println();
			var d = 0;
	    for(d <- 0 to args.length-1){
	      mi_writer.write( "#  " );
	      var e = 0;
	      for(e <- 0 to args(0).length-1){
	      	mi_writer.write(args(d)(e).toString);
	      	                             }
	    mi_writer.write("  #");
	    mi_writer.println(); }
	    mi_writer.write(border_string2);
	    mi_writer.println();
			mi_writer.write(border_string);
			mi_writer.println();
			
			mi_writer.write("               features");
			mi_writer.println();
			mi_writer.write(" x                y               type");
			mi_writer.println();
			var t1 = 0;
			for ( t1<- 0 to list_of_features.length-1 )	{
				var t2 = 0;
				for (t2 <- 0 to list_of_features(0).length-1)	{
				var feature_name="";
					if (t2<2){mi_writer.write(list_of_features(t1)(t2).toString); mi_writer.write("       ,       ");}
					else{println("above 2 ");if(list_of_features(t1)(t2)==0){mi_writer.write("shallows")} }
					
			}
			mi_writer.println();
			}
			println("list_of_x_features"  + list_of_features);
			println("making txt file");
			
			mi_writer.close()
			}                         //> to_text_file: (args: Array[Array[Int]], file_name: String, list_of_features
                                                  //| : Array[Array[Int]])Unit

  
  
  def replace_with (old:Int , new_un:Int, the_grid:Array[Array[Int]]){
  }                                               //> replace_with: (old: Int, new_un: Int, the_grid: Array[Array[Int]])Unit
  
  def find_edges2 (this_array:Array[Array[Int]])
  {
  val edge_list = Array.ofDim[Int](this_array.length * this_array(0).length, 2)
  var edge_ticker = 0;
  var d = 0;
  for(d <- 0 to this_array.length-1)
    {
    var e = 0;
    for(e <- 0 to this_array(0).length-1)
       {
        var current_tile_value = this_array(d)(e)
          var f = 0;
          for(f <- -1 to 1)
             {
             var g = 0;
             for(g <- -1 to 1)
                {
                  if (d!=0 && d < this_array.length-2 && e!= 0 && e < this_array(0).length-2)
                    {
                      if (this_array(d+f)(e+g) != current_tile_value & this_array(d)(e) == current_tile_value ){this_array(d)(e)=3;}
                    }
                }
             }
          }
      }
  }                                               //> find_edges2: (this_array: Array[Array[Int]])Unit
  
  
  
  def add_initial_land( this_array:Array[Array[Int]] , amount_land:Double, shape:String){
  if (shape == "square"){
  												var x_helper = Math.ceil((((this_array(0).length)) * (amount_land/2)))  ;
  												var y_helper = Math.ceil((((this_array.length)) * (amount_land/2))) ;
  												
  												var max_initial_tile = x_helper * y_helper ;
  												var pp = 0;
  												for (pp <- 0 to this_array(0).length-1){
  												
  														var qq = 0;
  												    for (qq <- 0 to this_array.length-1){
  																if (((this_array(0).length)/2) - x_helper < pp ){ if ( pp < ((this_array(0).length)/2) + x_helper){
  																if (((this_array.length)/2) - y_helper < qq ){ if ( qq < ((this_array.length)/2) + y_helper){    this_array(qq)(pp) =1
  																																																											//	println( "((this_array.length-1)/2)" + ((this_array.length-1)/2) +"x_helper"+ x_helper + "((this_array.length-1)/2) - x_helper"+ (((this_array.length-1)/2) - x_helper) +"               pp "+ pp+"                            ((this_array.length-1)/2) + x_helper" + (((this_array.length-1)/2) + x_helper))
  																																																																															   }}
  																
  																                            }}
  												
  												
  												
  											                                          	}
  												
  												
  												
  											                                       	}
  												
  
                        }
  
  
  
  }                                               //> add_initial_land: (this_array: Array[Array[Int]], amount_land: Double, shap
                                                  //| e: String)Unit
  
  // below is find edges
  def find_edges(this_array:Array[Array[Int]]){
  val edge_list = Array.ofDim[Int](this_array.length * this_array(0).length, 2)
  var edge_ticker = 0;
  var d = 0;
  for(d <- 0 to this_array.length-1){
      var e = 0;
      for(e <- 0 to this_array(0).length-1){
       // if (d > 3){println("init      d" + d + "      e" + e)}
        if (this_array(d)(e) != 0){
          var neg_one_to_one = Array(-1,0,1)
          var f = 0;
          for(f <- -1 to 1){
            var g = 0;
            for(g <- -1 to 1){
            if (d!=0 && d < this_array.length-2 && e!= 0 && e < this_array(0).length-2){
              if (this_array(d+f)(e+g) == 0 & this_array(d)(e) == 1 ){
                  
                  //println("init      d" + d + "      e" + e)
                  this_array(d)(e)=3;
 
            
                                            }          // next to water
                                            }
                                              }
                                            }
                                  }
                                        }
                                    }
  
  
  }                                               //> find_edges: (this_array: Array[Array[Int]])Unit
  
  def make_grid (x:Int , y:Int):Array[Array[Int]] = {
  	
  	var the_grid = Array.fill(y, x)(0); // 0 = initialized but empty tile    // HAS TO BE T FIRST!!!!!
  	return the_grid
  }                                               //> make_grid: (x: Int, y: Int)Array[Array[Int]]
  
  
  
  def add_imperfections(this_grid:Array[Array[Int]],imperfections:Int, r:scala.util.Random){
 var ii = 0;
  for (ii <- 0 to imperfections){
  var imperfection_type = r.nextInt(2)  //  0 = indentation       1 = lump
  var side = r.nextInt(3) // like unit circle    ie 0 = coming from right        1 = coming from top           2 = coming from left               3 = coming from bottom

   var edge_found = false;
   //println(side + "      side ")
   if (side == 0){
     var random_y=r.nextInt(this_grid.length-1);
			  var x2 = 999999;
			  var x1 = this_grid(0)(this_grid(0).length-1);
			  var index_counter = this_grid(0).length-1;
			   while(edge_found == false)
			  {
					var first_type = x1
					index_counter += (-1)
					x2 = x1
					//println("indexes y, x "+ random_y + "         " +index_counter )
					x1 = this_grid(random_y)(index_counter)
					if (index_counter < this_grid(0).length/2){random_y=r.nextInt(this_grid.length-1); index_counter=this_grid(0).length - 1;  x2 = 999999; x1 = this_grid(random_y)(index_counter);}
			  	if (x1 != x2){
			  	if (imperfection_type == 0){
			  		if (this_grid(random_y)(index_counter) == 3){this_grid(random_y)(index_counter) = 0}
			  		if (this_grid(random_y)(index_counter-1) == 1) {this_grid(random_y)(index_counter-1) = 3;}
			  															}
			  	if (imperfection_type == 1 && index_counter+1 < this_grid(0).length ){this_grid(random_y)(index_counter) = 1; this_grid(random_y)(index_counter+1) = 3;}
			  	
			  	//println("666666666666666666666")
			  	edge_found = true;
			  }
  
				}}
				
				//###########################
				
  if (side == 1){
  
     var random_x= r.nextInt(this_grid(0).length-1);
			  var y2 = 999999;
			  var y1 = this_grid(0)(random_x);;
			  var index_counter = 0;
			   while(edge_found == false)
			  {
					index_counter += (1)
					y2 = y1
					//println("indexes x "+ random_x + "        y  " +index_counter       + "         "  + y1 + "y1              " + y2 + "y2" )
					y1 = this_grid(index_counter)(random_x)
					if (index_counter > this_grid.length/2){random_x = r.nextInt(this_grid(0).length-1); index_counter=0; println("new rand x " + random_x);  y2 = 999999; y1 = 999999}
			  	if (y1 != y2){
			  	//println("qqqq    index counter  " + index_counter + "                 random_x " + random_x)
			  	if (imperfection_type == 0){
			  	//println("elp 000")
			  		if (this_grid(index_counter)(random_x) == 3){this_grid(index_counter)(random_x) = 0; println("elp1")}
			  		if (this_grid(index_counter)(random_x) == 1) {this_grid(index_counter)(random_x+1) = 3; println("elp2")}
			  															//println("elp3")
			  															}
			  	if (imperfection_type == 1 && index_counter-1 > 0){println("elp444");this_grid(index_counter)(random_x) = 1; this_grid(index_counter-1)(random_x) = 3;}
			  	
			  	//println("666666666666666666666")
			  	random_x= r.nextInt(this_grid(0).length-1);
			    y2 = 999999;
			    y1 = this_grid(0)(random_x);;
			  	edge_found = true;
			  }
  	}}
  
    if (side == 2){
  			var random_y=r.nextInt(this_grid.length-1);
			  var x2 = 999999;
			  var x1 = this_grid(random_y)(0);
			  var index_counter = 0;
			   while(edge_found == false)
			  {
					index_counter += (1)
					x2 = x1
					//println("indexes y, x "+ random_y + "         " +index_counter )
					x1 = this_grid(random_y)(index_counter)
					if (index_counter > this_grid(0).length/2){random_y=r.nextInt(this_grid.length-1);index_counter=0;   x2 = 999999; x1 = this_grid(random_y)(index_counter);  println("new nums")}
			  	if (x1 != x2){
			  	if (imperfection_type == 0){
			  		if (this_grid(random_y)(index_counter) == 3){this_grid(random_y)(index_counter) = 0}
			  		if (this_grid(random_y)(index_counter+1) == 1) {this_grid(random_y)(index_counter+1) = 3;}
			  															}
			  	if (imperfection_type == 1  && index_counter - 1 > -1){this_grid(random_y)(index_counter) = 1; this_grid(random_y)(index_counter-1) = 3;}
			  	
			  	//println("666666666666666666666")
			  	edge_found = true;

			  }
  			}}
  
    if (side == 3){
  			 var random_x= r.nextInt(this_grid(0).length-1);
			  var y2 = 999999;
			  var y1 = this_grid(this_grid.length-1)(random_x);;
			  var index_counter = this_grid.length-1;
			   while(edge_found == false)
			  {
					index_counter += (-1)
					y2 = y1
					// // println("indexes x "+ random_x + "        y  " +index_counter       + "         "  + y1 + "y1              " + y2 + "y2" )
					y1 = this_grid(index_counter)(random_x)
					if (index_counter < this_grid.length/2){println("new nums"); random_x = r.nextInt(this_grid(0).length-1); index_counter=this_grid.length-1; println("new rand x " + random_x);  y2 = 999999; y1 = this_grid(this_grid.length-1)(random_x)}
			  	if (y1 != y2){
			  	//println("qqqq    index counter  " + index_counter + "                 random_x " + random_x)
			  	if (imperfection_type == 0){
			  //	println("elp 000")
			  		if (this_grid(index_counter)(random_x) == 3){this_grid(index_counter)(random_x) = 0; println("elp1")}
			  		if (this_grid(index_counter)(random_x) == 1) {this_grid(index_counter)(random_x-1) = 3; println("elp2")}
			  															println("elp3")
			  															}
			  	if (imperfection_type == 1 && index_counter+1 < index_counter ){println("elp444");this_grid(index_counter)(random_x) = 1; this_grid(index_counter-1)(random_x) = 3;}
			  	
			  	//println("666666666666666666666")
			  	random_x= r.nextInt(this_grid(0).length-1);
			    y2 = 999999;
			    y1 = this_grid(0)(random_x);;
			  	edge_found = true;

			  }
  
  
  }
  }
  }
  
  }                                               //> add_imperfections: (this_grid: Array[Array[Int]], imperfections: Int, r: s
                                                  //| cala.util.Random)Unit
  
  def add_ocean(this_array:Array[Array[Int]])
  {
  var d = 0;
  println("addding ocean")
  for(d <- 0 to this_array.length-1)
    {
      var e = 0;
      for(e <- 0 to this_array(0).length-1)
      {
			var deepwater = true   // pptt true
 			 if (this_array(d)(e) == 0)
          {
 			 		var f = 0;
          for(f <- -2 to 2)
            {
            var g = 0;
            for(g <- -2 to 2)
              {
                var howmanytwos=0;
                if (d == 2 || d == -2){howmanytwos+=1}
                if (e == 2 || e == -2){howmanytwos+=1}
                	if (howmanytwos < 2 && -1 < d+f && d+f < (this_array.length-1) && -1 < e+g && e+g < (this_array(0).length-1) ){
	                
                		if (this_array(d+f)(e+g) != 0 && this_array(d+f)(e+g) != 2 && this_array(d+f)(e+g) != 9){deepwater = false}
            	                                                                                                                  }
 			 		     }
          }
 			 		if (deepwater == true){
 			 		this_array(d)(e) = 2}
 	        }
        }
     }
  }                                               //> add_ocean: (this_array: Array[Array[Int]])Unit
  
  
  
  
  def add_oceanic_features(number_of_features:Int, this_grid:Array[Array[Int]], r:scala.util.Random,list_of_features:Array[Array[Int]] ){
  var nn = 0;
  if(number_of_features > 0){
  for(nn <- 0 to number_of_features-1){
 // println("feature " + nn)
  var feature_type = r.nextInt(2)
  feature_type = 0;
  var spot_chosen = false;
  var x = r.nextInt(this_grid(0).length-1);  var y = r.nextInt(this_grid.length-1);
  while(spot_chosen == false){ x = r.nextInt(this_grid(0).length-1);  y = r.nextInt(this_grid.length-1);
   		spot_chosen = true;
   		x = r.nextInt(this_grid(0).length-1); y = r.nextInt(this_grid.length-1);
   		var f = 0;
         for(f <- -2 to 3)
            {
            var g = 0;
            for(g <- -3 to 2)
              {
              if (-1 < y+f && y+f < (this_grid.length-1) && -1 < x+g && x+g < (this_grid(0).length-1)){
              //  println("on the map ")
              if (this_grid(y+f)(x+g) != 2){spot_chosen = false;   //println("not here y" + (y) + "     x " + (x));
              }}
							 // println(" yoooo hoooo  ")
               }}
  
  
   }
   	 list_of_features(nn)(0) = x
   	 list_of_features(nn)(1) = y
   	 list_of_features(nn)(2) = feature_type
   	 

     println(" past loop  ")
  // make random rocks and shallow water
  var f = 0;
         for(f <- -3 to 3)
            {
            var g = 0;
            for(g <- -3 to 3)
              {
              
              if (-1 < y+f && y+f < (this_grid.length-1) && -1 < x+g && x+g < (this_grid(0).length-1)){
              var random_num = r.nextInt(3); if (random_num == 0){this_grid(y+f)(x+g) = 9}; if (random_num == 1){this_grid(y+f)(x+g) = 0}
              
              }    }}
  
  }}
  }                                               //> add_oceanic_features: (number_of_features: Int, this_grid: Array[Array[Int
                                                  //| ]], r: scala.util.Random, list_of_features: Array[Array[Int]])Unit
  
  def make_island(x:Int , y:Int , amount_land:Double, shape:String, imperfections:Int , name:String, num_of_oceanic_features:Int){
  	
  	val r = scala.util.Random
  	var this_grid = make_grid(x,y)
  	var jjj = 0;
  	
  	var list_of_features = Array.fill(num_of_oceanic_features, 3)(0)
  	
  	
  	add_initial_land(this_grid, amount_land, shape)
  	find_edges(this_grid)
  	add_imperfections(this_grid,imperfections,r)
  	this_grid(this_grid.length-2)(this_grid(0).length-2) = 9;    //  first index is y second is x
  	add_ocean(this_grid)
  	add_oceanic_features(num_of_oceanic_features, this_grid,r,list_of_features)
  	// add_costal_features()
  	//
  	to_text_file(this_grid, name,list_of_features)
  	println("DONE")
  }                                               //> make_island: (x: Int, y: Int, amount_land: Double, shape: String, imperfec
                                                  //| tions: Int, name: String)Unit
                                                  
  // x dimension, y dimension, ratio of water to land, which shape, number of imperfections, name, number of significant oceanic features
  make_island(80,30 ,0.4,"square",20, "jackland new",4)
 }
