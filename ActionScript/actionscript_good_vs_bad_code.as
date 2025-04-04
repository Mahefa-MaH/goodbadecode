// Good Code Example: Using Event Listeners and a State Machine for Robust UI Interaction

package 
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		private enum State {IDLE, HOVER, CLICKED};
		private var currentState:State = State.IDLE;

		public function GoodCodeExample()
		{
			this.addEventListener(MouseEvent.ROLL_OVER, onRollOver);
			this.addEventListener(MouseEvent.ROLL_OUT, onRollOut);
			this.addEventListener(MouseEvent.CLICK, onClick);
			this.buttonMode = true;
		}

		private function onRollOver(e:MouseEvent):void
		{
			if (currentState == State.IDLE)
			{
				currentState = State.HOVER;
				graphics.beginFill(0xFF0000); //Change color on hover
				graphics.drawRect(0,0,100,50);
				graphics.endFill();
			}
		}

		private function onRollOut(e:MouseEvent):void
		{
			if (currentState == State.HOVER)
			{
				currentState = State.IDLE;
				graphics.clear(); //Reset to default state
			}
		}

		private function onClick(e:MouseEvent):void
		{
			if (currentState == State.HOVER)
			{
				currentState = State.CLICKED;
				//Perform click action
				trace("Clicked!");
				currentState = State.IDLE;
				graphics.clear();
			}
		}
	}
}


// Bad Code Example:  Direct Manipulation, Lack of Event Handling and State Management.

package 
{
	import flash.display.Sprite;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			graphics.beginFill(0x0000FF);
			graphics.drawRect(0, 0, 100, 50);
			graphics.endFill();
		}
	}
}
