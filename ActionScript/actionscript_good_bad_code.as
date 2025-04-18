// Good Code: Using a custom event for communication between classes.

package com.example
{
    import flash.events.Event;
    import flash.events.EventDispatcher;

    public class DataManager extends EventDispatcher
    {
        private var _data:Array;

        public function DataManager()
        {
            _data = [];
        }

        public function loadData(url:String):void
        {
            //Simulate loading data from a URL.  In reality, you would use a Loader or URLRequest here.
            var data:Array = [1,2,3,4,5];
            _data = data;
            dispatchEvent(new DataEvent(DataEvent.DATA_LOADED, data));
        }
    }
}

package com.example
{
    import flash.events.Event;

    public class DataEvent extends Event
    {
        public static const DATA_LOADED:String = "dataLoaded";
        public var data:Array;
        public function DataEvent(type:String, data:Array)
        {
            super(type, false, false);
            this.data = data;
        }
    }
}

package com.example
{
    import flash.display.Sprite;
    import flash.events.EventDispatcher;


    public class DataDisplay extends Sprite
    {
        private var _dataManager:DataManager;

        public function DataDisplay()
        {
            _dataManager = new DataManager();
            _dataManager.addEventListener(DataEvent.DATA_LOADED, onDataLoaded);
            _dataManager.loadData("data.xml");
        }

        private function onDataLoaded(event:DataEvent):void
        {
            trace("Data loaded:", event.data);
        }
    }
}



// Bad Code: Tight coupling and lack of error handling.

package com.example.bad
{
    import flash.display.Sprite;

    public class BadDataManager extends Sprite
    {
        public var data:Array;

        public function BadDataManager()
        {
            data = [10,20,30,40,50]; //Hardcoded Data
        }

        public function getData():Array
        {
            return data;
        }
    }
}

package com.example.bad
{
    import flash.display.Sprite;

    public class BadDataDisplay extends Sprite
    {
        private var _dataManager:BadDataManager;

        public function BadDataDisplay()
        {
            _dataManager = new BadDataManager();
            trace("Data:", _dataManager.getData());

        }
    }
}
