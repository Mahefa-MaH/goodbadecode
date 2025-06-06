// Good Code: Using Event Listeners and State Management for Efficient Button Handling

package 
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class GoodButton extends Sprite
	{
		private var _isEnabled:Boolean = true;

		public function GoodButton()
		{
			buttonMode = true;
			addEventListener(MouseEvent.CLICK, onClick);
			addEventListener(MouseEvent.MOUSE_OVER, onOver);
			addEventListener(MouseEvent.MOUSE_OUT, onOut);

		}


		private function onClick(event:MouseEvent):void
		{
			if (_isEnabled)
			{
				// Perform action here
				trace("Button Clicked!");
			}
		}

		private function onOver(event:MouseEvent):void
		{
			if (_isEnabled)
			{
				//Change button appearance
				graphics.beginFill(0xFF0000);
				graphics.drawRect(0,0,100,30);
				graphics.endFill();

			}
		}

		private function onOut(event:MouseEvent):void
		{
			//Reset button appearance
			graphics.clear();
			graphics.beginFill(0x0000FF);
			graphics.drawRect(0,0,100,30);
			graphics.endFill();

		}

		public function set enabled(value:Boolean):void
		{
			_isEnabled = value;
			//Update button appearance based on enabled state.
		}

	}
}


// Bad Code: Inefficient Button Handling with Direct Frame Scripting and Global Variables

//This code is bad because it uses global variables and is difficult to maintain and reuse.  Event handling is also poor

stop();
var buttonClicked:Boolean = false;


button.addEventListener(MouseEvent.CLICK, buttonClickHandler);


function buttonClickHandler(event:MouseEvent):void
{
	buttonClicked = true;
	trace("Button Clicked!"); // Perform action based on global variable state
	if (buttonClicked)
	{
		gotoAndStop(2); //Direct frame control, bad practice
	}

}
