// Good Code: Using a custom event for better decoupling and maintainability.

package
{
	import flash.events.Event;
	import flash.events.EventDispatcher;

	public class GoodCodeExample extends EventDispatcher
	{
		public function GoodCodeExample()
		{
			addEventListener("dataReady", onDataReady);
		}

		public function fetchData():void
		{
			// Simulate fetching data
			var data:String = "Data fetched successfully!";
			dispatchEvent(new Event("dataReady"));
		}


		private function onDataReady(event:Event):void
		{
			trace("Data received: " + event.type);
		}
	}
}


// Bad Code: Tightly coupled code with direct method calls, hindering reusability and testability

package
{
	public class BadCodeExample
	{
		private var dataProcessor:DataProcessor = new DataProcessor();

		public function BadCodeExample()
		{
			dataProcessor.processData();
		}
	}

	internal class DataProcessor
	{
		public function processData():void
		{
			trace("Processing data directly...");
		}
	}
}
