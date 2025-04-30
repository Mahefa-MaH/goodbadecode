// Good Code: Using a custom event for better decoupling and maintainability.
package
{
	public class GoodCodeExample extends MovieClip
	{
		public function GoodCodeExample()
		{
			addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, handleCustomEvent);
		}

		private function handleCustomEvent(event:MyCustomEvent):void
		{
			trace("Custom Event Received: " + event.data);
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


// Bad Code: Tight coupling and poor error handling.
package
{
	public class BadCodeExample extends MovieClip
	{
		public function BadCodeExample()
		{
			var myObject:MyObject = new MyObject();
			try
			{
				trace(myObject.someMethod());
			}
			catch(error:Error)
			{
				// Poor error handling
				trace("An error occurred!");
			}
		}
	}
}

package
{
	public class MyObject
	{
		public function someMethod():String
		{
			return "Some value"; //Can throw error here.
		}
	}
}
