// Good Code: Using event listeners and a custom event for better organization and maintainability.

package
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			// Create a custom event
			var myEvent:MyEvent = new MyEvent(MyEvent.MY_EVENT);

			// Add an event listener for the custom event
			addEventListener(MyEvent.MY_EVENT, handleMyEvent);

			// Dispatch the custom event when the mouse is clicked
			addEventListener(MouseEvent.CLICK, dispatchMyEvent);
		}

		private function dispatchMyEvent(e:MouseEvent):void
		{
			dispatchEvent(new MyEvent(MyEvent.MY_EVENT));
		}

		private function handleMyEvent(e:MyEvent):void
		{
			trace("Custom event handled:", e.data);
		}
	}
}

// Custom event class
class MyEvent extends Event
{
	public static const MY_EVENT:String = "myEvent";
	public var data:String;

	public function MyEvent(type:String, bubbles:Boolean = false, cancelable:Boolean = false, data:String = null)
	{
		super(type, bubbles, cancelable);
		this.data = data;
	}
}


// Bad Code:  Using direct function calls and global variables, leading to tight coupling and poor maintainability.

package
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class BadCodeExample extends Sprite
	{
		public var globalVariable:String = "Initial Value";

		public function BadCodeExample()
		{
			addEventListener(MouseEvent.CLICK, handleClick);
		}

		private function handleClick(e:MouseEvent):void
		{
			modifyGlobalVariable();
			trace("Global variable modified:", globalVariable);
		}

		private function modifyGlobalVariable():void
		{
			globalVariable = "Modified Value";
		}
	}
}
