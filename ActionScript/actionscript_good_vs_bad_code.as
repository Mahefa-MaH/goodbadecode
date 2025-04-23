// Good Code: Using Event Listeners and Custom Events for Modular Design

package
{
	public class GoodCodeExample extends MovieClip
	{
		public function GoodCodeExample()
		{
			// Register event listener for a custom event
			addEventListener("myCustomEvent", handleCustomEvent);

			// Dispatch the custom event after a delay
			setTimeout(dispatchEvent(new Event("myCustomEvent")), 2000);
		}

		private function handleCustomEvent(event:Event):void
		{
			trace("Custom event received!");
		}
	}
}


// Bad Code: Tight Coupling and Inflexible Design

package
{
	public class BadCodeExample extends MovieClip
	{
		private var myVariable:String = "Hello";

		public function BadCodeExample()
		{
			// Directly manipulating another object's property.  Tight Coupling!
			this.myVariable = "world";
			someOtherObject.myProperty = this.myVariable;
		}
	}
}

// Note: someOtherObject needs to be defined elsewhere in a proper application context for the above to work. This is just a snippet to illustrate a concept.


