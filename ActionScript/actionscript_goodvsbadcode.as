// Good Code: Using event listeners for efficient UI interaction

package
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0xFF0000);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 50;
			button.y = 50;
			addChild(button);

			button.addEventListener(MouseEvent.CLICK, handleClick);
		}

		private function handleClick(event:MouseEvent):void
		{
			trace("Button clicked!");
		}
	}
}


// Bad Code: Inefficient and hard-to-maintain event handling

package
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0x0000FF);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 150;
			button.y = 50;
			addChild(button);

			button.addEventListener(MouseEvent.CLICK, function(e:MouseEvent):void{trace("Button clicked (bad way)!");}); //anonymous function, makes debugging hard
		}
	}
}
