// Good Code: Using event listeners and a custom event for better organization and maintainability.

package 
{
	import flash.events.Event;
	import flash.display.Sprite;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, onMyCustomEvent);
			dispatchEvent(new MyCustomEvent(MyCustomEvent.MY_CUSTOM_EVENT));
		}

		private function onMyCustomEvent(event:MyCustomEvent):void
		{
			trace("Custom event received: ", event.data);
		}
	}

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


// Bad Code: Directly manipulating properties, lack of event handling, and poor structure.  Difficult to maintain and extend.

package 
{
	import flash.display.Sprite;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			this.graphics.beginFill(0xFF0000);
			this.graphics.drawRect(10,10,100,50);
			this.graphics.endFill();
			this.x = 100;
			this.y = 100;
			//Direct manipulation of properties is bad practice in advanced scenarios.
			//Lack of structure and event handling makes the code difficult to maintain and extend.
		}
	}
}
