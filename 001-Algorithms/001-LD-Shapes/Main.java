/*
 * In this file we include code snippets of equivalent constructs written in
 * Haskell programming language. Deciding whether or not Java is an adequate choice for
 * teaching students algorithms is data structures is left as an excercise for the reader.
 */

// :( :( :( :( :( 

import java.util.HashMap;

/*
 * data Vertex = Vertex { v_x :: Float, v_y :: Float }
 */
class Vertex {
    public Vertex(Float _x, Float _y) {
	x = _x;
	y = _y;
    }
    public Float x;
    public Float y;
}

/*
 * data Point = Point { p_x :: Float, p_y :: Float }
 */
class Point {
    public Point(Float _x, Float _y) {
	x = _x;
	y = _y;
    }
    public Float x;
    public Float y;
}

/*
 * newtype Polygon = Polygon [Vertex]
 */
class Polygon {
    public Polygon(Vertex[] _vertices) {
	vertices = _vertices;
    }
    public Vertex[] vertices;
}

/*
 * data Color = Green | Red | Blue | White
 */
class Color {
    public static final HashMap<String, Integer> colors;
    // what a joke
    static {
	colors = new HashMap<String, Integer>();
	colors.put("green", 1000);
	colors.put("red",    100);
	colors.put("blue",    10);
	colors.put("white",    1);
    }
}

/*
 * main :: IO ()
 * main = putStrLn $ show $ Vertex 1.0 2.0
 */
public class Main {
    public static void main(String[] args) {
	Vertex v = new Vertex(new Float(1.0), new Float(2.0));
	System.out.println(v.x.toString() + "," + v.y.toString());
	if (Color.colors.get("green") > Color.colors.get("red")) {
	    System.out.println("Well, that works.");
	}
    }
}
