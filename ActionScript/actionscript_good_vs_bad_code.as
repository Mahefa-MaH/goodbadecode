// Good Code: Using a custom event for better code organization and maintainability.
package
{
	import flash.events.Event;
	import flash.display.Sprite;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			//Custom Event
			addEventListener("myCustomEvent",customEventHandler);
			dispatchEvent(new Event("myCustomEvent"));
		}

		private function customEventHandler(event:Event):void{
			trace("Custom event handled successfully!");
		}
	}
}


// Bad Code: Tightly coupled code with poor error handling and unclear logic.
package 
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			//Directly accessing and manipulating properties without proper encapsulation or error checks.
			buttonMode = true;
			addEventListener(MouseEvent.CLICK, onClick);

		}
		private function onClick(e:MouseEvent):void{
			//Directly manipulating stage properties which is bad practice
			stage.addChild(new Sprite());
		}
	}
}
