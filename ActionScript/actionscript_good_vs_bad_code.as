// Good Code: Using a custom event for better code organization and maintainability.

package
{
    public class GoodCodeExample extends MovieClip
    {
        public function GoodCodeExample()
        {
            addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, handleMyCustomEvent);
            dispatchEvent(new MyCustomEvent(MyCustomEvent.MY_CUSTOM_EVENT));
        }

        private function handleMyCustomEvent(event:MyCustomEvent):void
        {
            trace("Custom event received: ", event.data);
        }
    }
}

package
{
    public class MyCustomEvent extends Event
    {
        public static const MY_CUSTOM_EVENT:String = "myCustomEvent";
        public var data:String;

        public function MyCustomEvent(type:String, bubbles:Boolean = false, cancelable:Boolean = false, data:String = null)
        {
            super(type, bubbles, cancelable);
            this.data = data;
        }
    }
}


// Bad Code: Tight coupling and lack of event handling.  Difficult to maintain and extend.

package 
{
    public class BadCodeExample extends MovieClip
    {
        private var _myVar:String = "Initial Value";

        public function BadCodeExample()
        {
            someFunctionThatChangesMyVar();
            trace("Value of _myVar: " + _myVar);

        }

        private function someFunctionThatChangesMyVar():void
        {
            _myVar = "Modified Value";
        }
    }
}
