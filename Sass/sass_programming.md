// Good Sass: Using mixins and functions for reusable code and maintainability.

@mixin button-style($color, $size: 1em) {
  background-color: $color;
  padding: 0.5em $size;
  border: none;
  cursor: pointer;
}

@function rem($px) {
  @return $px / 16 + rem;
}


.button-primary {
  @include button-style(blue, 1.5em);
}

.button-secondary {
  @include button-style(green);
}

.text {
  font-size: rem(18px);
}


// Bad Sass: Repetitive code and lack of organization.

.button-primary {
  background-color: blue;
  padding: 0.5em 1.5em;
  border: none;
  cursor: pointer;
}

.button-secondary {
  background-color: green;
  padding: 0.5em 1em;
  border: none;
  cursor: pointer;
}

.another-button {
  background-color: red;
  padding: 0.5em 1em;
  border: none;
  cursor: pointer;
}

.text-size {
  font-size: 1.125rem;
}
