// Good Code: Using a custom event for better decoupling and maintainability.

package
{
	public class GoodCodeExample extends MovieClip
	{
		public function GoodCodeExample()
		{
			addEventListener(MyCustomEvent.MY_CUSTOM_EVENT, onMyCustomEvent);
			dispatchEvent(new MyCustomEvent( "Data from Event" ));
		}

		private function onMyCustomEvent(event:MyCustomEvent):void
		{
			trace("Received data: ", event.data);
		}
	}
}

package
{
	public class MyCustomEvent extends Event
	{
		public static const MY_CUSTOM_EVENT:String = "myCustomEvent";
		public var data:String;

		public function MyCustomEvent(data:String)
		{
			super(MY_CUSTOM_EVENT, false, false);
			this.data = data;
		}
	}
}


// Bad Code: Tight coupling and lack of error handling.

package
{
	public class BadCodeExample extends MovieClip
	{
		private var myVariable:Object;

		public function BadCodeExample()
		{
			myVariable = { name: "test"};
			accessData();

		}

		private function accessData():void
		{
			//Potentially dangerous. No error handling and directly accessing a potential null object.
			trace(myVariable.name.toUpperCase());

		}
	}
}
