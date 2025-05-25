// Good Code: Using a custom event for better decoupling and maintainability.

package 
{
	import flash.events.Event;
	import flash.events.EventDispatcher;

	public class GoodCodeExample extends EventDispatcher
	{
		public function GoodCodeExample() 
		{
			// Listen for the custom event.
			addEventListener("dataReady", onDataReady);
		}

		public function loadData():void
		{
			// Simulate asynchronous data loading.
			setTimeout(function():void{
				dispatchEvent(new Event("dataReady"));
			}, 1000);
		}

		private function onDataReady(event:Event):void
		{
			trace("Data loading is complete.");
                        //Further actions here.
		}
	}
}


// Bad Code: Tight coupling and lack of error handling.
package 
{
	public class BadCodeExample
	{
		private var _data:String;

		public function BadCodeExample() 
		{
			//Directly access and modify another class's data.
			_data = SomeOtherClass.getData();
                        // No error handling, if SomeOtherClass.getData() throws an error.
			trace("Data: " + _data);
		}
	}

        // Dummy class for BadCodeExample
        public class SomeOtherClass{
            public static function getData():String{
                return "Some data";
            }
        }
}
