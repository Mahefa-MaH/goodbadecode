// Good Code: Using Event Listeners and Custom Events for Modular Design

package
{
    import flash.events.Event;
    import flash.events.EventDispatcher;

    public class GoodCode extends EventDispatcher
    {
        public function GoodCode()
        {
            addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, onMyCustomEvent);
        }

        private function onMyCustomEvent(event:MyCustomEvent):void
        {
            trace("Good Code: Event received:", event.data);
        }

        public function triggerEvent(data:String):void
        {
            dispatchEvent(new MyCustomEvent(MyCustomEvent.MY_CUSTOM_EVENT, false, false, data));
        }
    }
}


package
{
    import flash.events.Event;

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


// Bad Code: Tight Coupling and Lack of Event Handling

package
{
    public class BadCode
    {
        private var _data:String;

        public function BadCode(data:String)
        {
            _data = data;
            process();
        }

        private function process():void
        {
            trace("Bad Code: Processing:", _data);
            // tightly coupled and no easy way to extend or decouple
        }
    }
}
