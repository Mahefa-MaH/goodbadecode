// Good Code: Using Event Listeners and a State Machine for Robustness

package
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		private var state:String = "idle";

		public function GoodCodeExample()
		{
			addEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
		}

		private function onAddedToStage(e:Event):void
		{
			removeEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
			//Simplified State Machine Example. Could be expanded for more complex scenarios.

			addEventListener(MouseEvent.CLICK, onClick);

		}

		private function onClick(e:MouseEvent):void{
			switch(state){
				case "idle":
					state = "active";
					trace("Activated");
					break;
				case "active":
					state = "idle";
					trace("Deactivated");
					break;
			}
		}
	}
}


// Bad Code:  Direct Manipulation, Lack of Structure, and Error Prone

package
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			this.addEventListener(MouseEvent.CLICK,onClick);
		}

		private function onClick(e:MouseEvent):void
		{
			if (this.graphics.beginFill(0xff0000)){
				this.graphics.drawRect(10,10,100,100);
				this.graphics.endFill();
			} else {
				this.graphics.clear();
			}
		}
	}
}
