// Good Code: Using event listeners and a custom event for better organization and maintainability.

package
{
	import flash.events.Event;
	import flash.display.Sprite;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			// Custom event for better organization
			addEventListener("myCustomEvent", myCustomEventHandler);

			// Simulate some action that triggers the event
			dispatchEvent(new Event("myCustomEvent"));
		}

		private function myCustomEventHandler(event:Event):void
		{
			trace("Custom event handled successfully!");
		}
	}
}


// Bad Code:  Directly manipulating timeline elements and lacking structure.  Difficult to maintain and debug.

package
{
	import flash.display.MovieClip;
	import flash.display.Sprite;

	public class BadCodeExample extends MovieClip
	{
		public function BadCodeExample()
		{
			// Directly accessing timeline elements - fragile and hard to maintain
			this.gotoAndStop(2);
			this["myMC"].visible = false;
			//Direct manipulation of properties  - difficult to track changes and potential conflicts
			this.x = 100;
			
		}
	}
}
