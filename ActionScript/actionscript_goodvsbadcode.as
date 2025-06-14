// Good Code: Using EventListeners and a custom Event

package
{
	import flash.display.Sprite;
	import flash.events.Event;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, handleCustomEvent);
			dispatchEvent(new MyCustomEvent(MyCustomEvent.MY_CUSTOM_EVENT));
		}

		private function handleCustomEvent(event:MyCustomEvent):void
		{
			trace("Custom event received: " + event.data);
		}
	}
}

import flash.events.Event;

class MyCustomEvent extends Event
{
	public static const MY_CUSTOM_EVENT:String = "myCustomEvent";
	public var data:String;

	public function MyCustomEvent(type:String, bubbles:Boolean = false, cancelable:Boolean = false, data:String = null)
	{
		super(type, bubbles, cancelable);
		this.data = data;
	}
}

// Bad Code: Direct manipulation of display objects and lack of event handling

package
{
	import flash.display.Sprite;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			var square:Sprite = new Sprite();
			square.graphics.beginFill(0xFF0000);
			square.graphics.drawRect(0, 0, 100, 100);
			square.graphics.endFill();
			addChild(square);

			//Directly manipulating properties without events, this makes it hard to manage and debug.
			square.x = 100;
			square.y = 100;
			
			//No event listener to handle changes in the state or interactions with other objects
		}
	}
}
