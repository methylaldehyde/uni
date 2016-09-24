/*
 * In this file we include code snippets of equivalent constructs written in
 * Haskell programming language. Deciding whether or not Java is an adequate choice for
 * teaching students algorithms is data structures is left as an excercise for the reader.
 */

// :( :( :( :( :( 

import java.util.HashMap;
import java.lang.*;

/*
 * data Vertex = Vertex { v_x :: double, v_y :: double }
 */
class Vertex {
    public Vertex(double _x, double _y) {
	x = _x;
	y = _y;
    }
    public double x;
    public double y;
}

/*
 * data Point = Point { p_x :: double, p_y :: double }
 */
class Point {
    public Point(double _x, double _y) {
	x = _x;
	y = _y;
    }
    public double x;
    public double y;
}

/*
 * newtype Polygon = Polygon [Vertex]
 */
class Polygon {
    public Polygon(Vertex[] _vertices) {
	vertices = _vertices;
    }
    // Really?
    public Polygon mkCopy() {
	Vertex[] vs = vertices.clone();
	for (int i = 0; i < vs.length; i++) {
	    vs[i] = new Vertex(vertices[i].x, vertices[i].y);
	}
	Polygon p1 = new Polygon(vs);
	return p1;
    }
    public String mkString() {
	String result = "";
	for (Vertex v : vertices) {
	    result = result + " (" + Double.toString(v.x) + ", " + Double.toString(v.y) + ")";
	}
	return result;
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

class Solve {
    // Parallel-move a polygon, picking lowest vertex as
    // a reference.
    public static Polygon parallelLowest(Polygon s) {
	Vertex v_min = new Vertex(s.vertices[0].x, s.vertices[0].y);
	for (Vertex l : s.vertices) {
	    if (v_min.y > l.y) {
		v_min.x = l.x;
		v_min.y = l.y;
	    }
	    // If we're on the same level, prefer leftmost point
	    if (v_min.y == l.y) {
		if (v_min.x > l.x) {
		    v_min.x = l.x;
		    v_min.y = l.y;
		}
	    }
	}
	Polygon result = s.mkCopy();
	for (Vertex m : result.vertices) {
	    m.x = m.x - v_min.x;
	    m.y = m.y - v_min.y;
	}
	return result;
    }
     
    // A special case of point in polygon solver that solves
    // point in polygon for rectangles
    public static Boolean rectangle(Polygon s, Point p) {
	// Contains minimal x coordinate of a rectangle
	// and the minimal y coordinate of a rectangle
	Point s_min = new Point(s.vertices[0].x, s.vertices[0].y);
	// Contans maximal x coordinate of a rectangle
	// and the maximal y coordinate of a rectangle
	Point s_max = new Point(s.vertices[0].x, s.vertices[0].y);
	// Loop through the vertices of given rectangle s
	// and fill s_min and s_max temporary variables
	for (Vertex v : s.vertices) {
	    if (v.x < s_min.x) {
		s_min.x = v.x;
	    }
	    if (v.y < s_min.y) {
		s_min.y = v.y;
	    }
	    if (v.x > s_max.x) {
		s_max.x = v.x;
	    }
	    if (v.y > s_max.y) {
		s_max.y = v.y;
	    }
	}
	// If the point is in rectangle, return true. Otherwise, return false
	if (p.x <= s_max.x &&
	    p.x >= s_min.x &&
	    p.y <= s_max.y &&
	    p.y >= s_min.y) {
	    return true;
	} else {
	    return false;
	}
    }

    // A special case of point in polygon solver that solves
    // point in polygon for triangles with angles at the base
    // equal to 45 degrees. We only support the following
    // kinds of triangles:
    //
    // A.
    //     -----
    //      \  |
    //       \ |
    //        \|
    //
    //
    // B.
    //        /|
    //       / |
    //      /  |
    //     -----
    //
    // C.
    //     -----
    //     |  /
    //     | /
    //     |/
    //
    //
    // D.
    //     |\
    //     | \
    //     |  \
    //     -----
    public static void triangle45() {
    }

    // A special case of point in polygon solver that solves
    // point in polygon problem for a circle or half-circle.
    public static void circle() {
    }
}

/*
 * main :: IO ()
 * main = putStrLn $ show $ Vertex 1.0 2.0
 */
public class Main {
    public static void main(String[] args) {
	// An example of a rectangle:
	Vertex vA = new Vertex((10.0), (2.0));
	Vertex vB = new Vertex((15.0), (2.0));
	Vertex vC = new Vertex((15.0), (-4.0));
	Vertex vD = new Vertex((10.0), (-4.0));
	Polygon r = new Polygon(new Vertex[] {vA, vB, vC, vD});
	System.out.println("Working with polygon:");
	System.out.println(r.mkString());
	if (Solve.rectangle(r, new Point((0.0), (0.0)))) {
	    System.out.println("Something went wrong");
	}
	if (Solve.rectangle(r, new Point((10.0), (-4.0)))) {
	    System.out.println("We are smart cats. We test for corner cases!");
	}
	if (Solve.rectangle(r, new Point((13.37), (-2.3)))) {
	    System.out.println("We're in.");
	}
	System.out.println("Parallel transition of the polygon:");
	System.out.println(Solve.parallelLowest(r).mkString());
	System.out.println("Make sure that the initial polygon is untouched:");
	System.out.println(r.mkString());
    }
}
