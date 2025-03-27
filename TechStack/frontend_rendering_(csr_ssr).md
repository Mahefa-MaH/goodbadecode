Let's break down the differences and best practices of Client-Side Rendering (CSR) and Server-Side Rendering (SSR).

**1. Core Difference between CSR and SSR:**

* **CSR (Client-Side Rendering):** The web server sends a minimal HTML structure to the client's browser.  The browser then uses JavaScript to fetch data from APIs and dynamically populate the page content.  The user sees a blank or partially-rendered page initially, then it fills in as the JavaScript executes.

* **SSR (Server-Side Rendering):** The web server generates the complete HTML, including the data, *before* sending it to the client's browser. The user sees a fully rendered page immediately.  While some JavaScript might still be used for interactive elements, the initial page load is handled entirely on the server.


**2. Basic CSR Implementation (JavaScript and HTML):**

```html
<!DOCTYPE html>
<html>
<head>
  <title>CSR Example</title>
</head>
<body>
  <div id="content"></div>
  <script>
    fetch('data.json') // Replace with your API endpoint
      .then(response => response.json())
      .then(data => {
        const contentDiv = document.getElementById('content');
        contentDiv.innerHTML = `<h1>${data.title}</h1><p>${data.description}</p>`;
      });
  </script>
</body>
</html>
```

`data.json` would contain something like: `{"title": "My Title", "description": "My Description"}`


**3. Basic SSR Implementation (Node.js and EJS):**

```javascript
const express = require('express');
const ejs = require('ejs');

const app = express();
app.set('view engine', 'ejs');

app.get('/', async (req, res) => {
  try {
    const data = await fetchData(); // Replace with your data fetching logic
    res.render('index', { data });
  } catch (error) {
    res.status(500).send('Error');
  }
});

// Simulate data fetching (replace with your actual API call)
async function fetchData() {
    return { title: "My SSR Title", description: "My SSR Description" };
}

app.listen(3000, () => console.log('Server listening on port 3000'));
```

`index.ejs` would be:

```html
<h1><%= data.title %></h1>
<p><%= data.description %></p>
```


**4. When CSR is Preferred:**

* **Single-Page Applications (SPAs):**  CSR excels in SPAs where the entire application is built using JavaScript and the initial HTML is just a shell.  Updates are done dynamically without full page reloads, leading to a more interactive experience.
* **Applications with frequent updates:** If the content changes very often, CSR allows for updating sections of the page without refreshing the entire thing.
* **Smaller applications with limited data:** For smaller projects with limited data, the overhead of SSR might not be justified.


**5. When SSR is Better:**

* **SEO (Search Engine Optimization):** Search engines can easily crawl and index content rendered on the server.  CSR pages may not be fully indexed unless extra measures (like pre-rendering or using JavaScript frameworks with SEO capabilities) are taken.
* **Initial Page Load Speed:**  The user immediately sees content, leading to a better user experience, especially on slower connections.
* **Social Media Sharing:** Social media bots are more likely to scrape and correctly display a fully rendered page from SSR.
* **Applications needing immediate content display:** For sites where content is paramount (news sites, blogs), immediate display is crucial.


**6. Key Performance Metrics:**

* **First Contentful Paint (FCP):** How quickly the browser renders any content (text, image).  Lower is better.
* **Largest Contentful Paint (LCP):**  How long it takes to render the largest content element.  Lower is better.
* **Time to Interactive (TTI):**  How long before the page becomes fully interactive.  Lower is better.
* **Cumulative Layout Shift (CLS):** Measures visual stability (how much the content shifts around while loading). Lower is better.
* **Total Blocking Time (TBT):** Measures the total time spent blocked by long tasks. Lower is better.


**7. Testing Client-Side vs. Server-Side Rendering:**

* **Network Throttling:** Disable JavaScript in your browser to see if the basic HTML structure is meaningful (SSR) or empty (CSR).
* **Inspecting Network Requests:** Use your browser's developer tools to analyze the network requests.  With SSR, most of the HTML is received in the initial response.  With CSR, the initial response is minimal, and further requests are made to fetch data.
* **View Source:** Examining the page source directly can reveal whether the HTML contains the full content (SSR) or just placeholders (CSR).


**8. Typical CSR Use Case:**

A complex interactive web application like a to-do list manager, a real-time chat application, or a dashboard with dynamic updates.


**9. Typical SSR Use Case:**

A blog, an e-commerce product page, a news website – where the initial content display and SEO are crucial.


**10. Effective SSR Example from Google's History:**

While Google doesn't explicitly promote a single "best" SSR example from its history, the evolution of their search results pages highlights effective SSR principles.  The initial rendering of search results, while leveraging a lot of client-side interactivity, prioritizes the quick display of core search results on the server, gradually enhancing the experience with client-side features.


**11. Bad Example of Poorly Implemented CSR from Yahoo!'s History:**

Yahoo! in its earlier days didn't have the widespread use of efficient JavaScript frameworks and practices.  A bad example would be a large, complex web page reliant on excessive JavaScript that had long loading times and poor performance.  This isn't easily pinpointed to a single specific page or time in history, but is more of a general characteristic of how earlier web applications were structured.  The lack of optimization and understanding of efficient CSR implementation led to poor user experiences across many of their sites.  This was partially due to the early state of JavaScript and web development technology.
