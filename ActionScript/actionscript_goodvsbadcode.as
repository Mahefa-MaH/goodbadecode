// Good Code: Using event listeners and a custom event for better organization and decoupling.

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
			button.graphics.beginFill(0xFF0000);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 50;
			button.y = 50;
			addChild(button);

			// Add a mouse click event listener to the button
			button.addEventListener(MouseEvent.CLICK, handleClick);
		}

		// Handle the click event, dispatch a custom event
		private function handleClick(event:MouseEvent):void
		{
			var customEvent:Event = new Event("buttonClicked");
			dispatchEvent(customEvent);
		}
	}
}

// Bad Code: Directly manipulating other objects, tightly coupled and hard to maintain.

package
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class BadCodeExample extends Sprite
	{
		private var otherObject:Sprite;

		public function BadCodeExample()
		{
			// Create a button
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0x0000FF);
			button.graphics.drawRect(0, 0, 100, 50);
			button.graphics.endFill();
			button.x = 150;
			button.y = 50;
			addChild(button);
			
			otherObject = new Sprite();
			otherObject.graphics.beginFill(0x00FF00);
			otherObject.graphics.drawRect(0,0,50,50);
			otherObject.graphics.endFill();
			otherObject.x = 200;
			otherObject.y = 150;
			addChild(otherObject);

			// Directly manipulate other object on click
			button.addEventListener(MouseEvent.CLICK, function(e:MouseEvent):void{
				otherObject.x += 20;
			});
		}
	}
}

