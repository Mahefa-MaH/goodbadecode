// Good Code: Using strongly-typed variables and a custom event for better code organization and maintainability.

package
{
	public class GoodCodeExample extends MovieClip
	{
		private var _score:int = 0;

		public function GoodCodeExample()
		{
			addEventListener(ScoreChangeEvent.SCORE_CHANGED, onScoreChanged);
			//Simulate score update
			dispatchEvent(new ScoreChangeEvent(ScoreChangeEvent.SCORE_CHANGED, 10));
		}

		private function onScoreChanged(event:ScoreChangeEvent):void
		{
			_score += event.scoreDelta;
			trace("Score updated: ", _score);
		}

	}
}

package
{
	public class ScoreChangeEvent extends Event
	{
		public static const SCORE_CHANGED:String = "scoreChanged";
		public var scoreDelta:int;

		public function ScoreChangeEvent(type:String, scoreDelta:int)
		{
			super(type);
			this.scoreDelta = scoreDelta;
		}
	}
}


// Bad Code: Loose typing, mixing data and display logic, and lack of event handling making it harder to maintain and debug.

package 
{
	public class BadCodeExample extends MovieClip
	{
		public function BadCodeExample()
		{
			var score:Object = 0; //Loose typing
			score = score + 10;
			this.scoreText.text = "Score: " + score; //Mixing data and display logic

		}
	}
}
