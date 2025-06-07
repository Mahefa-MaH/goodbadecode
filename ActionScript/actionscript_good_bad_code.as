// Good Code: Using event listeners and a state machine for robust UI interaction.

package
{
	import flash.display.Sprite;
	import flash.events.MouseEvent;

	public class GoodCodeExample extends Sprite
	{
		private var state:String = "idle";

		public function GoodCodeExample()
		{
			//Button Creation
			var button:Sprite = new Sprite();
			button.graphics.beginFill(0xFF0000);
			button.graphics.drawRect(0,0,100,50);
			button.graphics.endFill();
			button.x = 50;
			button.y = 50;
			addChild(button);


			button.addEventListener(MouseEvent.CLICK, onClick);
		}

		private function onClick(e:MouseEvent):void
		{
			switch (state)
			{
				case "idle":
					trace("Button Clicked: Performing Action A");
					state = "active";
					break;
				case "active":
					trace("Button Clicked: Performing Action B");
					state = "idle";
					break;
			}
		}
	}
}


// Bad Code:  Direct manipulation of display objects without event listeners, leading to unpredictable behavior and difficulty in maintenance.

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
			button.x = 150;
			button.y = 50;
			addChild(button);


			button.addEventListener(MouseEvent.CLICK, function(e:MouseEvent):void{
				this.graphics.clear(); //Directly manipulating button graphics.  Bad practice.
				this.graphics.beginFill(Math.random()*0xFFFFFF);
				this.graphics.drawRect(0,0,100,50);
				this.graphics.endFill();
			});
		}
	}
}
