// Good Code: Using event listeners and a custom class for better organization and maintainability.

package
{
	public class GoodCodeExample extends MovieClip
	{
		private var myButton:Button;
		private var myLabel:TextField;

		public function GoodCodeExample()
		{
			myButton = new Button();
			myButton.addEventListener(MouseEvent.CLICK, buttonClicked);
			addChild(myButton);

			myLabel = new TextField();
			myLabel.text = "Click the button!";
			addChild(myLabel);

			myButton.x = 100;
			myButton.y = 100;
			myLabel.x = 100;
			myLabel.y = 150;

		}

		private function buttonClicked(event:MouseEvent):void
		{
			myLabel.text = "Button clicked!";
		}
	}
}


// Bad Code:  Direct manipulation of timeline elements and global variables, leading to spaghetti code and difficult maintainability.

//This code is bad because of its global variable reliance and lack of organization.

stop();

myGlobalVar = 0;

button1.addEventListener(MouseEvent.CLICK, function():void{
	myGlobalVar++;
	trace("Button Clicked! Count: " + myGlobalVar);
	_root.myLabel.text = "Clicked " + myGlobalVar + " times!";
});

//Further bad practices would include direct manipulation of timeline items via _root,
//lack of error handling, and no encapsulation of functionality in classes.



