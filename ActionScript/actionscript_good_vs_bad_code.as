// Good Code: Using Event Listeners and Custom Events for efficient UI interaction.

package
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		public function GoodCodeExample()
		{
			// Create a button
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0x00FF00);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 50;
			button.y = 50;
			addChild(button);

			// Add a mouse click listener to dispatch a custom event
			button.addEventListener(MouseEvent.CLICK, handleClick);


			// Listen for the custom event
			addEventListener("buttonClicked", handleButtonClicked);

		}

		private function handleClick(event:MouseEvent):void{
			dispatchEvent(new Event("buttonClicked"));
		}

		private function handleButtonClicked(event:Event):void{
			trace("Button Clicked!");
		}
	}
}


// Bad Code: Direct manipulation of display objects and lack of event listeners leading to hard to maintain code.

package
{
	import flash.display.Sprite;
	import flash.display.Shape;

	public class BadCodeExample extends Sprite
	{
		public function BadCodeExample()
		{
			var rect:Shape = new Shape();
			rect.graphics.beginFill(0xFF0000);
			rect.graphics.drawRect(0,0,100,100);
			rect.graphics.endFill();
			addChild(rect);
			rect.addEventListener(MouseEvent.CLICK,function(e:MouseEvent):void{
				rect.graphics.clear();
				rect.graphics.beginFill(0x0000FF);
				rect.graphics.drawRect(0,0,100,100);
				rect.graphics.endFill();
			});
		}
	}
}
