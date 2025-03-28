/* Good CSS */
.good-selector {
  color: blue;
  font-size: 16px;
  margin: 10px;
}

/* Bad CSS */
#bad-id1, #bad-id2, .bad-class1, .bad-class2 { /* Too many selectors in one rule*/
  color: red;
  font-size: 12px;
  margin: 5px;
  padding: 2px;
  border: 1px solid black; /* Too many properties*/
}

div p span { /* Overly specific selector*/
  color: green;
}

* { /* Universal selector - avoid unless absolutely necessary*/
  box-sizing: border-box;
}

