// Good Code Example: Using Event Listeners and Custom Events for Modular Design

package
{
	import flash.events.Event;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends MovieClip
	{
		public function GoodCodeExample()
		{
			// Add event listener for a custom event
			this.addEventListener("myCustomEvent", onMyCustomEvent);

			// Dispatch the custom event when a button is clicked.
			button.addEventListener(MouseEvent.CLICK, onClick);
		}

		private function onClick(event:MouseEvent):void
		{
			// Dispatch a custom event
			dispatchEvent(new Event("myCustomEvent"));
		}


		private function onMyCustomEvent(event:Event):void
		{
			// Handle the custom event
			trace("Custom event received!");
			// Perform other actions based on the event
		}
	}
}


// Bad Code Example: Tight Coupling and Lack of Modularity


package
{
	public class BadCodeExample extends MovieClip
	{
		public function BadCodeExample()
		{
			button.addEventListener(MouseEvent.CLICK, onButtonClick);
		}

		private function onButtonClick(event:MouseEvent):void
		{
			// Directly manipulate other display objects 
			// which leads to tight coupling and makes code difficult to maintain
			otherObject.x = 200;
			trace("Button clicked!");
			// Perform other actions directly within the event handler.
		}
	}
}
