// Good Code Example: Using Event Listeners and State Management for Button Clicks

package 
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		private var _button:Sprite;
		private var _isButtonClicked:Boolean = false;

		public function GoodCodeExample() 
		{
			_button = new Sprite();
			_button.graphics.beginFill(0x00FF00);
			_button.graphics.drawRect(0, 0, 100, 50);
			_button.graphics.endFill();
			_button.x = 50;
			_button.y = 50;
			addChild(_button);

			_button.addEventListener(MouseEvent.CLICK, onClickHandler);
		}

		private function onClickHandler(event:MouseEvent):void
		{
			_isButtonClicked = !_isButtonClicked;
			if (_isButtonClicked)
			{
				_button.graphics.clear();
				_button.graphics.beginFill(0xFF0000);
				_button.graphics.drawRect(0, 0, 100, 50);
				_button.graphics.endFill();
			} else {
				_button.graphics.clear();
				_button.graphics.beginFill(0x00FF00);
				_button.graphics.drawRect(0, 0, 100, 50);
				_button.graphics.endFill();
			}
		}
	}
}


// Bad Code Example: Directly Manipulating Display List Without Event Listeners

package 
{
	import flash.display.Sprite;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample() 
		{
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0x0000FF);
			button.graphics.drawRect(0,0,100,50);
			button.graphics.endFill();
			button.x = 50;
			button.y = 50;
			addChild(button);

			//No event listener, direct manipulation - prone to errors and hard to maintain.
			button.addEventListener(MouseEvent.CLICK, function(e:MouseEvent):void{
				button.graphics.clear();
				button.graphics.beginFill(0xFF0000);
				button.graphics.drawRect(0,0,100,50);
				button.graphics.endFill();
			});

		}
	}
}
