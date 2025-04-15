// Good Code: Using a custom event for better decoupling and maintainability.

package
{
    public class GoodCodeExample extends MovieClip
    {
        public function GoodCodeExample()
        {
            addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, handleMyCustomEvent);
            dispatchEvent(new MyCustomEvent(MyCustomEvent.MY_CUSTOM_EVENT, "Hello from Good Code!"));

        }

        private function handleMyCustomEvent(event:MyCustomEvent):void
        {
            trace("Received event: " + event.data);
        }

    }
}


package
{
    public class MyCustomEvent extends Event
    {
        public static const MY_CUSTOM_EVENT:String = "myCustomEvent";
        public var data:String;
        public function MyCustomEvent(type:String, data:String)
        {
            super(type, false, false);
            this.data = data;
        }
    }
}




// Bad Code: Tightly coupled, difficult to maintain, and lacks error handling.

package
{
    public class BadCodeExample extends MovieClip
    {
        public function BadCodeExample()
        {
            var myVariable:String =  someFunctionThatMightFail();
            trace(myVariable.toUpperCase()); //Potential error: NullPointerException
        }

        private function someFunctionThatMightFail():String
        {
            //Simulates a potential failure.  Could be a network request, file access, etc.
            if(Math.random() < 0.5) return "some string"; else return null;

        }
    }
}
