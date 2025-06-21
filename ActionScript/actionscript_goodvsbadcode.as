//Good Code: Using a custom event to handle data updates between classes.

package
{
	public class DataManager
	{
		public event function dataUpdated(e:Event):void{
			//Handle Data Update here
		}

		public function updateData(newData:Object):void
		{
			//Process new data
			dispatchEvent(new Event("dataUpdated"));
		}
	}
}


package
{
	public class DataDisplay extends MovieClip
	{
		private var dataManager:DataManager;

		public function DataDisplay()
		{
			dataManager = new DataManager();
			dataManager.addEventListener("dataUpdated", onDataUpdated);

		}

		private function onDataUpdated(e:Event):void
		{
			//Update Display elements with the new data from DataManager.
			trace("Data Updated");
		}
	}
}



//Bad Code: Tight Coupling and lack of error handling.

package
{
	public class BadDataManager
	{
		public var data:Object;
		public function updateData(newData:Object):void{
			data = newData;
		}
	}
}

package
{
	public class BadDataDisplay extends MovieClip
	{
		private var dataManager:BadDataManager = new BadDataManager();

		public function BadDataDisplay()
		{
			//Directly access and update, no event handling.
			dataManager.updateData({name:"John",age:30});
			trace(dataManager.data.name); //Error prone if data is null or missing.

		}
	}
}
