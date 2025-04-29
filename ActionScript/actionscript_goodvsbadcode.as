// Good Code: Using a custom event for better code organization and maintainability.

package 
{
	public class GoodCodeExample extends MovieClip
	{
		public function GoodCodeExample()
		{
			addEventListener("myCustomEvent", handleMyCustomEvent);
			dispatchEvent(new Event("myCustomEvent"));
		}

		private function handleMyCustomEvent(event:Event):void
		{
			trace("Custom event handled successfully!");
		}
	}
}


// Bad Code: Tightly coupled code with direct function calls, resulting in poor organization and reduced reusability.

package 
{
	public class BadCodeExample extends MovieClip
	{
		public function BadCodeExample()
		{
			doSomething();
		}

		private function doSomething():void
		{
			trace("Something happened...");
			anotherFunction();
		}

		private function anotherFunction():void
		{
			trace("Another thing happened...");
		}
	}
}
