// Good Code: Using Event Listeners and Custom Events for Modular Design

package 
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite 
	{
		public function GoodCodeExample() 
		{
			// Create a button
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0xFF0000);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 50;
			button.y = 50;
			addChild(button);

			// Add a mouse click event listener
			button.addEventListener(MouseEvent.CLICK, onClick);

			// Create a custom event to handle button clicks
			var customEvent:CustomEvent = new CustomEvent(Event.CHANGE, false, false, "Button Clicked");
			dispatchEvent(customEvent);

			// Add an event listener to handle the custom event
			addEventListener(Event.CHANGE, onCustomEvent);

		}

		// Event handler for button clicks
		private function onClick(event:MouseEvent):void 
		{
			// Dispatch a custom event to notify other parts of the application.
			var customEvent:CustomEvent = new CustomEvent(Event.CHANGE, false, false, "Button Clicked");
			dispatchEvent(customEvent);
		}

		// Event handler for custom events
		private function onCustomEvent(event:Event):void 
		{
			// Handle the custom event
			trace("Custom event received: " + event.data);
		}
	}
}


// Bad Code: Tight Coupling and Lack of Modular Design

package 
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class BadCodeExample extends Sprite 
	{
		public function BadCodeExample() 
		{
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0x0000FF);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 150;
			button.y = 50;
			addChild(button);
			button.addEventListener(MouseEvent.CLICK, onButtonClick);
		}

		private function onButtonClick(event:MouseEvent):void 
		{
			// Directly manipulating other objects within the same class.
			trace("Button clicked! Directly manipulating objects here...");
			// This tightly couples this function to other parts of the application.
			// It lacks modularity and makes the code harder to maintain and reuse.
		}
	}
}
