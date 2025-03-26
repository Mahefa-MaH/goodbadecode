// Good Code Example: Calculating the area of a rectangle
class Rectangle {
    private double length;
    private double width;

    public Rectangle(double length, double width) {
        this.length = length;
        this.width = width;
    }

    public double getArea() {
        return length * width;
    }
}

// Bad Code Example: Calculating the area of a rectangle with potential errors
class BadRectangle {
    double length;
    double width;

    public double getArea() {
        return length * width; //No input validation or error handling.
    }
}

public class GoodAndBadCode {
    public static void main(String[] args) {
        Rectangle goodRectangle = new Rectangle(5, 10);
        double goodArea = goodRectangle.getArea();
        System.out.println("Good Rectangle Area: " + goodArea);


        BadRectangle badRectangle = new BadRectangle();
        badRectangle.length = 5;
        badRectangle.width = 10;
        double badArea = badRectangle.getArea();
        System.out.println("Bad Rectangle Area: " + badArea);

        //Demonstrates potential for errors in BadRectangle
        BadRectangle badRectangle2 = new BadRectangle();
        badRectangle2.length = -5; //Negative length, should throw exception in good code.
        badRectangle2.width = 10;
        double badArea2 = badRectangle2.getArea();
        System.out.println("Bad Rectangle Area with negative length: " + badArea2);

    }
}
