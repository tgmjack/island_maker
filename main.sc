import scala.io.Source
import java.io._
import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Rectangle,Color,Font,BasicStroke}
import java.awt.geom._

// scroll to bottom to define your island

object make_island {
  def to_text_file (args: Array[Array[Int]], file_name:String,  list_of_features:Array[Array[Int]]){
  		var file_name2 = file_name+".txt";
			var mi_writer = new PrintWriter(new File(file_name2) )
			mi_writer.write(" 0   =   water");mi_writer.println();
			mi_writer.write(" 1   =   land");mi_writer.println();
			mi_writer.write(" 2   =   deep water");mi_writer.println();
			mi_writer.write(" 3   =   beach"); mi_writer.println();
			mi_writer.write(" 7   =   hill"); mi_writer.println();
			mi_writer.write(" 8   =   mountain"); mi_writer.println();
			mi_writer.write(" 9   =   rock");mi_writer.println();mi_writer.println();
			mi_writer.println();
			var k = 0;
			var border_string = "    ";
	      for(k <- 0 to args(0).length+5){border_string+= "#";}
			var border_string2 = "    #";
			for(k <- 0 to args(0).length-1){border_string2+= " " }
			border_string2+= "    #" ;
			var title_string= "" ;
			var num_of_spaces = args(0).length+5 - file_name.length
			var sp = 0;
			for(sp <- 0 to num_of_spaces/2){title_string += " "
	      }
			title_string = title_string + file_name
			mi_writer.println();
			
			
			
			
			mi_writer.println();
			mi_writer.write(title_string);
	    mi_writer.println();
	    
	    		mi_writer.write("       ")
    var dd = 0;
    var x_tick_dist = 6; // keep above 3
    var space_counter = 0;
     for(dd <- 0 to args(0).length-1){  if ( dd%x_tick_dist==0){   mi_writer.write(dd.toString); }
	    else{ space_counter += 1; if (dd<10){mi_writer.write(" ")}; if (space_counter%x_tick_dist != 0 && dd > 9 && dd < 99  ){mi_writer.write(" ")}; if(dd > 99){ if ((dd+1)%x_tick_dist == 0|| (dd+2)%x_tick_dist == 0 || (dd+3)%x_tick_dist == 0 ){mi_writer.write(" ")};}}}
	    mi_writer.println();
	    
	    
	    
			mi_writer.write(border_string);
	    mi_writer.println();
			mi_writer.write(border_string2);
	    mi_writer.println();
	    mi_writer.write("");
	    
	    
	    
	    
	    
	    
	   // ####################
	    
	// print every 6th num
	//    var dd = 0;
	//    for(dd <- 0 to args(0).length-1){  if ( dd%6==0){   mi_writer.write(dd);}
	//    }
	//    mi_writer.println();
	    
			var d = 0;
	    for(d <- 0 to args.length-1){
	    if (d<10){mi_writer.write(" "); }
	    if (d<100){mi_writer.write(" "); }
	    if (d<1000){mi_writer.write(" "); }
	      mi_writer.write( d+"#  " );
	      var e = 0;
	      for(e <- 0 to args(0).length-1){
	      	mi_writer.write(args(d)(e).toString);
	      	                             }
	    mi_writer.write("  #");
	    mi_writer.println(); }
	    
	    
	    // ######################
	    
	    
	    mi_writer.write(border_string2);
	    mi_writer.println();
			mi_writer.write(border_string);
			mi_writer.println();mi_writer.println();mi_writer.println();
			
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
					else{println("above 2 ");if(list_of_features(t1)(t2)==0){mi_writer.write("shallows")}
					if(list_of_features(t1)(t2)==8){mi_writer.write("mountain")}
					}
					
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
                        if (shape == "triangle"){
                        // define corners based on wateer amount. fill
                        
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
  
  def add_land_features(num_of_oceanic_features:Int, num_of_land_features:Int, this_grid:Array[Array[Int]],r:scala.util.Random ,list_of_features:Array[Array[Int]]){
  
  var nn = 0;
  if(num_of_land_features > 0){
  for(nn <- 0 to num_of_land_features-1){
 // println("feature " + nn)
  //var feature_type = r.nextInt(8)  // 8 = mountain     make start from 8
  var feature_type = 8;
  var spot_chosen = false;
  var x = r.nextInt(this_grid(0).length-1);  var y = r.nextInt(this_grid.length-1);
  while(spot_chosen == false){ x = r.nextInt(this_grid(0).length-1);  y = r.nextInt(this_grid.length-1);
   		spot_chosen = true;
   		x = r.nextInt(this_grid(0).length-1); y = r.nextInt(this_grid.length-1);
   		var f = 0;
         for(f <- -2 to 2)
            {
            var g = 0;
            for(g <- -2 to 2)
              {
              if (-1 < y+f && y+f < (this_grid.length-1) && -1 < x+g && x+g < (this_grid(0).length-1)){
              //  println("on the map ")
              if (this_grid(y+f)(x+g) != 1){spot_chosen = false;   //println("not here y" + (y) + "     x " + (x));
              }}
							 // println(" yoooo hoooo  ")
               }}
    
   }
   
   list_of_features(num_of_oceanic_features-1 + nn)(0) = x
   	 list_of_features(num_of_oceanic_features-1 + nn)(1) = y
   	 list_of_features(num_of_oceanic_features-1 + nn) (2) = feature_type
   if (feature_type == 8){
   var f2 = 0;
         for(f2 <- -3 to 3)
            {
            var g2 = 0;
            for(g2 <- -3 to 3)
              {var rock_rand = r.nextInt(3);
              if (rock_rand==0)
              {
              this_grid(y+f2)(x+g2) = 9;
              }
              
              }}
   
   		var f = 0;
   		
         for(f <- -1 to 1)
            {
            var g = 0;
            for(g <- -1 to 1)
              {
						   
   
   				
   				this_grid(y+f)(x+g) = 7;
   				this_grid(y)(x) = 8;
   				
   		}}
   }
   
  
  
  }
  
  }
  
  }                                               //> add_land_features: (num_of_oceanic_features: Int, num_of_land_features: In
                                                  //| t, this_grid: Array[Array[Int]], r: scala.util.Random, list_of_features: A
                                                  //| rray[Array[Int]])Unit
  
  
  
  def make_island(x:Int , y:Int , amount_land:Double, shape:String, imperfections:Int , name:String, num_of_oceanic_features:Int, num_of_land_features:Int){
  	
  	val r = scala.util.Random
  	var this_grid = make_grid(x,y)
  	var jjj = 0;
  	
  	var list_of_features = Array.fill((num_of_oceanic_features + num_of_land_features)-1, 3)(0)
  	
  	
  	add_initial_land(this_grid, amount_land, shape)
  	find_edges(this_grid)
  	add_imperfections(this_grid,imperfections,r)
  	this_grid(this_grid.length-2)(this_grid(0).length-2) = 9;    //  first index is y second is x
  	add_ocean(this_grid)
  	add_oceanic_features(num_of_oceanic_features, this_grid,r,list_of_features)
  	add_land_features(num_of_oceanic_features,num_of_land_features, this_grid,r,list_of_features)
  	// add_costal_features()
  	//
  	to_text_file(this_grid, name,list_of_features)
  	draw(this_grid)
  }                                               //> make_island: (x: Int, y: Int, amount_land: Double, shape: String, imperfec
                                                  //| tions: Int, name: String, num_of_oceanic_features: Int, num_of_land_featur
                                                  //| es: Int)Unit
  def draw(map : Array[Array[Int]]){
		
		val size = (30 * (map(0).length-1), 30 * (map.length-1))
		val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
		val g = canvas.createGraphics()
		g.setColor(Color.WHITE)
		g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
		g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
		
		var d = 0;
			for(d <- 0 to map.length-1){
			      var e = 0;
			      for(e <- 0 to map(0).length-1){
			      
			     		println("yoo hoo")
			      	if(map(d)(e) == 2){g.setColor(Color.BLUE); g.fill(new Rectangle(e*30, d*30, 30, 30));}
			      	if(map(d)(e) == 1){g.setColor(Color.GREEN); g.fill(new Rectangle(e*30, d*30, 30, 30));}
			      	if(map(d)(e) == 3){g.setColor(Color.ORANGE); g.fill(new Rectangle(e*30, d*30, 30, 30));}
			      	if(map(d)(e) == 0){g.setColor(Color.CYAN); g.fill(new Rectangle(e*30, d*30, 30, 30));}
			      	if(map(d)(e) == 7){g.setColor(Color.green.darker()); g.fill(new Rectangle(e*30, d*30, 30, 30));}
			      	if(map(d)(e) == 8){g.setColor(Color.GRAY); g.fill(new Rectangle(e*30, d*30, 30, 30));}
			      	if(map(d)(e) == 9){g.setColor(Color.LIGHT_GRAY); g.fill(new Rectangle(e*30, d*30, 30, 30));}
																					}
																	}
g.dispose()
javax.imageio.ImageIO.write(canvas, "png", new java.io.File("island_drawing.png"))
}                                                 //> draw: (map: Array[Array[Int]])Unit

def to_text_file_mini (args: Array[Array[Int]])
	{
		var mi_writer = new PrintWriter(new File("mini.txt") );
		
		var d = 0;
		
		for(d <- 0 to args.length-1)
		{
		      var e = 0;
		      for(e <- 0 to args(0).length-1){
		      	//mi_writer.write(args(d)(e).toString);
		      	                             }
		        mi_writer.println();
		        //println("newline")
		}
mi_writer.close()
}                                                 //> to_text_file_mini: (args: Array[Array[Int]])Unit

		
		def read_text_file_and_build_island(filename:String): Array[Array[Int]] = {
		// read txt file
		// make array
		println("1");
		var tick = 0
		var x = 0
		var y = 0;
		var y_ticker = 0;
		var found_island=0;
		var zeroes = "000";
		var twos = "222";
		var ones = "111";
		var threes = "333";
		var sevens = "777";
		var eigths = "888";
		var nines = "999";
		var x_length = 0;
		var x_length_set = false;
		var hash = "####" // very stupid solution
		for (line <- Source.fromFile(filename).getLines) {
		    
		    //if (line.length>7){//println("b1  >" + line(7)+ "<    " + line(7).getType);
		    //if(line(7).equals('#')){println("hashtag")}}
		    //println(line.getType)
		    if (line.length>7){
		    //	println(">7");
		    
		    if (line(9).equals(hash(0)) && line(10).equals(hash(0)) && line(11).equals(hash(0)) && line(7).equals(hash(0)) && line(8).equals(hash(0))){if ( found_island == 0 ){
		    println("b2 " + (line.length-5).toString);
		    found_island=2;
		    x = line.length-6;
		    }}}
		    if (found_island == 1 && x_length_set == false && line(3).equals(zeroes(0)))
		    { println("x_ counter "); for (xtt <- 0 to line.length-1){if ( line(xtt).equals(twos(0)) || line(xtt).equals(ones(0)) || line(xtt).equals(threes(0)) || line(xtt).equals(sevens(0)) || line(xtt).equals(eigths(0)) || line(xtt).equals(nines(0)) ) {x_length +=1;}  }
		    x = x_length - 1;
		    x_length_set = true;
		    }
		    //println("2");
		    if (found_island == 1 || found_island == 2){ y_ticker += 1 }
		    if (line.length > 6){
		    if (line(9).equals(hash(0)) && line(10).equals(hash(0)) && line(11).equals(hash(0)) && line(7).equals(hash(0)) && line(8).equals(hash(0)) ){if ( found_island == 1 ){
		    found_island=0;
		    y = y_ticker-4;
		    }}
		    if (found_island == 2){found_island = 1}
		}}
		println("3  x = "+x +"         y = "+y);
		var the_grid = Array.fill(y, x)(0);
		
		/*var dd = 0;
			for(dd <- 0 to the_grid.length-1){
			      var ee = 0;
			      for(ee <- 0 to the_grid(0).length-1){
			      	println(the_grid(dd)(ee).toString);
			      	                             			}
			        println("new line in grid");
			      	                             }
		*/
		var first_x = 0;
		found_island=0;
		y_ticker = -1;
		var x_start = false;
		for (line <- Source.fromFile(filename).getLines) {
		    if (line.length > 10){
		    if (line(9).equals(hash(0)) && line(10).equals(hash(0)) && line(11).equals(hash(0)) && line(7).equals(hash(0)) && line(8).equals(hash(0))){if ( found_island == 0 ){
		    found_island=2;
		    println("2nd time through");
		    }}}
		    //println("4");
		    if (found_island == 1 && (line(12).equals(twos(0)) || line(15).equals(twos(0)) || line(line.length-10).equals(twos(0))))
		    {  y_ticker +=1;   var x_ticker = 0;
		    	for(x_ticker <- 0 to x-1)
		    	{
		    	if (x_start == false){
		    	if (line.length > x_ticker){
		    	   if (line(x_ticker).equals(twos(0))){first_x = x_ticker; x_start = true;}
		    			
		    			if (line(x_ticker).equals(ones(0))){first_x = x_ticker; x_start = true;}
		    			
		    			if (line(x_ticker).equals(threes(0))){first_x = x_ticker; x_start = true;}
		    			
		    			if (line(x_ticker).equals(sevens(0))){first_x = x_ticker; x_start = true;}
		    			
		    			if (line(x_ticker).equals(eigths(0))){ first_x = x_ticker; x_start = true;}
		    			
		    			if (line(x_ticker).equals(nines(0))){ first_x = x_ticker; x_start = true;}
							  }
							}
		    	
		    		if (x_start == true){
		    		if (line.length+first_x > x_ticker){
		    			
		    			if (line(x_ticker+first_x).equals(twos(0))){ /*println(the_grid(y_ticker)(x_ticker-7) + " is now ");*/ the_grid(y_ticker)(x_ticker)= 2; /*println(the_grid(y_ticker)(x_ticker-7));*/}
		    			
		    			if (line(x_ticker+first_x).equals(ones(0))){ /*println(the_grid(y_ticker)(x_ticker-7) + " is now ");*/ the_grid(y_ticker)(x_ticker)= 1; /*println(the_grid(y_ticker)(x_ticker-7));*/}
		    			
		    			if (line(x_ticker+first_x).equals(threes(0))){ /*println(the_grid(y_ticker)(x_ticker-7) + " is now ");*/ the_grid(y_ticker)(x_ticker)= 3; /*println(the_grid(y_ticker)(x_ticker-7));*/}
		    			
		    			if (line(x_ticker+first_x).equals(sevens(0))){ /*println(the_grid(y_ticker)(x_ticker-7) + " is now ");*/ the_grid(y_ticker)(x_ticker)= 7; /*println(the_grid(y_ticker)(x_ticker-7));*/}
		    			
		    			if (line(x_ticker+first_x).equals(eigths(0))){ /*println(the_grid(y_ticker)(x_ticker-7) + " is now ");*/ the_grid(y_ticker)(x_ticker)= 8; /*println(the_grid(y_ticker)(x_ticker-7));*/}
		    			
		    			if (line(x_ticker+first_x).equals(nines(0))){ /*println(the_grid(y_ticker)(x_ticker-7) + " is now ");*/ the_grid(y_ticker)(x_ticker)= 9; /*println(the_grid(y_ticker)(x_ticker-7));*/}
		    			//println("y "+ y_ticker + "  x " + (x_ticker-7) + " with value "+ the_grid(y_ticker)(x_ticker-7) + " is now 2    " + (line(x_ticker))); println(the_grid(y_ticker)(x_ticker-7) + "     but it int though");
		    			if (x_ticker > line.length-20){}
		    			}} }}
		    //println("55");
		    
		    if (line.length > 10){ var x2 = 0;// for (x2<-0 to x){println( "y_tick " + y_ticker + "                     x " + x2); println(the_grid(y_ticker)(x2))};println("5555");
		    if (line(9).equals(hash(0)) && line(10).equals(hash(0)) && line(11).equals(hash(0)) && line(7).equals(hash(0)) && line(8).equals(hash(0))){if ( found_island == 1 ){
		    found_island=0;
		    }}
		    
		}
		if (found_island ==2){ found_island = 1}
		}
		/*println("after check")
		var d = 0;
			for(d <- 0 to the_grid.length-1){
			      var e = 0;
			      for(e <- 0 to the_grid(0).length-1){
			      	println(the_grid(d)(e).toString);
		}	      	                             }
		*/
		
		to_text_file_mini(the_grid)
		return (the_grid)
}                                                 //> read_text_file_and_build_island: (filename: String)Array[Array[Int]]
  
                                                  
  //         x dimension, y dimension, ratio of water to land, which shape, number of imperfections, name    ,    number of significant oceanic features , number of land features
  make_island(140        ,     30    ,         0.4           ,  "square"  ,          20         , "jackland 2"          ,  3                                  ,2)
                                                  
                                                  
}
